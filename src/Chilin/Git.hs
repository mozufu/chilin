module Chilin.Git
  ( runGit
  , tryGit
  , resolveRef
  , validateRef
  , initBare
  , writeSnapshot
  , readSnapshot
  , readSnapshotAt
  , codePath
  , trackerPath
  , withRepoLock
  , withRepoLocks
  , gitEnvironment
  ) where

import Chilin.Types
import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Concurrent.MVar (modifyMVar, newMVar, withMVar)
import Control.Exception (throwIO)
import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Digest, SHA1, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (status500, status503)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

codePath, trackerPath :: Env -> Repo -> FilePath
codePath env repo = envRoot env </> "repos" </> T.unpack (repoId repo) </> "code.git"
trackerPath env repo = envRoot env </> "repos" </> T.unpack (repoId repo) </> "tracker.git"

gitEnvironment :: IO [(String, String)]
gitEnvironment = do
  inherited <- getEnvironment
  pure $
    filter ((`elem` ["PATH", "TMPDIR", "SYSTEMROOT"]) . fst) inherited
      <> [ ("GIT_CONFIG_NOSYSTEM", "1")
         , ("GIT_CONFIG_GLOBAL", "/dev/null")
         , ("GIT_TERMINAL_PROMPT", "0")
         , ("GIT_ATTR_NOSYSTEM", "1")
         , ("GIT_NO_REPLACE_OBJECTS", "1")
         , ("GIT_AUTHOR_NAME", "Chilin")
         , ("GIT_AUTHOR_EMAIL", "chilin@localhost")
         , ("GIT_COMMITTER_NAME", "Chilin")
         , ("GIT_COMMITTER_EMAIL", "chilin@localhost")
         , ("LC_ALL", "C")
         , ("HOME", "/var/empty")
         ]

boundedRead :: Handle -> IO ByteString
boundedRead handle = go 0 []
 where
  go size chunks = do
    chunk <- BS.hGetSome handle 65536
    if BS.null chunk
      then pure (BS.concat (reverse chunks))
      else do
        let total = size + BS.length chunk
        when (total > 64 * 1024 * 1024) $ throwIO (AppError status503 "Git output exceeds the 64 MiB operation limit")
        go total (chunk : chunks)

tryGit :: Env -> FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
tryGit env path args input = do
  environment <- gitEnvironment
  result <- timeout (60 * 1000000)
    $ withCreateProcess
      (proc (envGit env) (["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false", "--git-dir=" <> path] <> args))
        { env = Just environment
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    $ \stdinHandle stdoutHandle stderrHandle process -> case (stdinHandle, stdoutHandle, stderrHandle) of
      (Just hin, Just hout, Just herr) -> do
        mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
        withAsync (BS.hPut hin input >> hClose hin) $ \writer -> do
          (out, err) <- concurrently (boundedRead hout) (boundedRead herr)
          exit <- waitForProcess process
          -- Failed Git commands may close stdin before consuming the request.
          when (exit == ExitSuccess) (wait writer)
          pure (exit, out, err)
      _ -> throwIO (AppError status500 "Cannot create Git process pipes")
  maybe (throwIO (AppError status503 "Git operation timed out")) pure result

runGit :: Env -> FilePath -> [String] -> ByteString -> IO ByteString
runGit env path args input = do
  (exit, out, _) <- tryGit env path args input
  case exit of
    ExitSuccess -> pure out
    ExitFailure _ -> conflict ("Git operation failed: " <> T.pack (case args of [] -> "unknown"; x : _ -> x))

resolveRef :: Env -> FilePath -> Text -> IO (Maybe Text)
resolveRef env path ref = do
  (exit, out, _) <- tryGit env path ["rev-parse", "--verify", "--end-of-options", T.unpack ref <> "^{commit}"] ""
  pure $ if exit == ExitSuccess then Just (T.strip (TE.decodeUtf8 out)) else Nothing

validateRef :: Env -> Text -> IO ()
validateRef env ref = do
  unless ("refs/heads/" `T.isPrefixOf` ref && not (T.any (`elem` ['\NUL', '\n', '\r']) ref)) $ badRequest "Expected a full branch ref under refs/heads/"
  (exit, _, _) <- tryGit env (envRoot env) ["check-ref-format", T.unpack ref] ""
  unless (exit == ExitSuccess) $ badRequest "Invalid branch ref"

initBare :: Env -> FilePath -> IO ()
initBare env path = do
  createDirectoryIfMissing True (takeDirectory path)
  _ <- runGit env path ["init", "--bare", "--object-format=sha1", "--initial-branch=main", path] ""
  forM_ [("core.fsync", "objects,reference"), ("core.fsyncMethod", "fsync"), ("receive.fsckObjects", "true"), ("transfer.fsckObjects", "true"), ("receive.denyDeletes", "true"), ("receive.denyNonFastForwards", "true"), ("receive.hideRefs", "refs/chilin/"), ("uploadpack.hideRefs", "refs/chilin/")] $ \(key, value) -> do
    _ <- runGit env path ["config", key, value] ""
    pure ()

withRepoLock :: Env -> Repo -> IO a -> IO a
withRepoLock env repo action = do
  lock <- modifyMVar (envLocks env) $ \locks -> case Map.lookup (repoId repo) locks of
    Just existing -> pure (locks, existing)
    Nothing -> do
      fresh <- newMVar ()
      pure (Map.insert (repoId repo) fresh locks, fresh)
  let directory = envRoot env </> "locks"
  createDirectoryIfMissing True directory
  withMVar lock $ \() -> withFileLock (directory </> T.unpack (repoId repo) <> ".lock") Exclusive (const action)

withRepoLocks :: Env -> [Repo] -> IO a -> IO a
withRepoLocks env repos action = foldr (withRepoLock env) action ordered
 where
  ordered = Map.elems (Map.fromList [(repoId repo, repo) | repo <- repos])

data TreeEntry = TreeEntry FilePath Text deriving (Show)

safeTrackerPath :: FilePath -> Bool
safeTrackerPath path = not (null path) && not (isAbsolute path) && all validPart (splitDirectories path)
 where
  validPart part = part `notElem` [".", "..", ".git", ""] && not (any (`elem` ['\NUL', '\n', '\r', '\\']) part)

readEntries :: Env -> FilePath -> Text -> IO [TreeEntry]
readEntries env path revision = do
  output <- runGit env path ["ls-tree", "-r", "-z", T.unpack revision] ""
  forM (filter (not . BS.null) (BC.split '\NUL' output)) $ \entry -> do
    let (metadata, rawName) = BC.break (== '\t') entry
    case (BC.words metadata, TE.decodeUtf8' (BS.drop 1 rawName)) of
      (["100644", "blob", oid], Right name) | safeTrackerPath (T.unpack name) -> pure (TreeEntry (T.unpack name) (TE.decodeUtf8 oid))
      _ -> conflict "Tracker contains an invalid path or a non-regular file"

readSnapshot :: Env -> FilePath -> IO Snapshot
readSnapshot env path = do
  revision <- resolveRef env path "refs/heads/main" >>= maybe (conflict "Tracker has no canonical revision") pure
  readSnapshotAt env path revision

readSnapshotAt :: Env -> FilePath -> Text -> IO Snapshot
readSnapshotAt env path revision = do
  unless (T.length revision == 40 && T.all (`elem` (['0' .. '9'] <> ['a' .. 'f'])) revision) $ badRequest "Invalid tracker revision"
  entries <- readEntries env path revision
  files <- readBlobs entries
  pure (Snapshot revision (Map.fromList files))
 where
  readBlobs [] = pure []
  readBlobs entries = do
    environment <- gitEnvironment
    result <- timeout (60 * 1000000)
      $ withCreateProcess
        (proc (envGit env) ["-c", "core.hooksPath=/dev/null", "--git-dir=" <> path, "cat-file", "--batch"])
          { env = Just environment
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
      $ \inputHandle outputHandle errorHandle process -> case (inputHandle, outputHandle, errorHandle) of
        (Just hin, Just hout, Just herr) -> do
          mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
          withAsync (boundedRead herr) $ \errors -> do
            files <- forM entries $ \(TreeEntry name oid) -> do
              BS.hPut hin (TE.encodeUtf8 oid <> "\n")
              hFlush hin
              header <- BC.hGetLine hout
              size <- case BC.words header of
                [actual, "blob", lengthText] | actual == TE.encodeUtf8 oid -> case BC.readInt lengthText of
                  Just (n, remainder) | BS.null remainder && n >= 0 && n <= 64 * 1024 * 1024 -> pure n
                  _ -> conflict "Invalid tracker object length"
                _ -> conflict "Missing tracker object"
              raw <- BS.hGet hout size
              newline <- BS.hGet hout 1
              unless (BS.length raw == size && newline == "\n") $ conflict "Truncated tracker object"
              contents <- either (const (conflict "Tracker files must be UTF-8")) pure (TE.decodeUtf8' raw)
              pure (name, contents)
            hClose hin
            exit <- waitForProcess process
            _ <- wait errors
            unless (exit == ExitSuccess) $ conflict "Cannot read canonical tracker objects"
            pure files
        _ -> throwIO (AppError status500 "Cannot create tracker read pipes")
    maybe (throwIO (AppError status503 "Tracker read timed out")) pure result

blobOid :: ByteString -> Text
blobOid bytes = T.pack (show (hash ("blob " <> BC.pack (show (BS.length bytes)) <> "\NUL" <> bytes) :: Digest SHA1))

writeSnapshot :: Env -> FilePath -> Maybe Text -> Map FilePath Text -> Text -> IO Text
writeSnapshot env path previous files message = do
  unless (all safeTrackerPath (Map.keys files)) $ badRequest "Unsafe canonical file path"
  previousEntries <- maybe (pure []) (readEntries env path) previous
  let existing = Map.fromList [(name, oid) | TreeEntry name oid <- previousEntries]
  blobs <- forM (Map.toAscList files) $ \(name, contents) -> do
    let bytes = TE.encodeUtf8 contents
        expected = blobOid bytes
    oid <-
      if Map.lookup name existing == Just expected
        then pure expected
        else
          T.strip . TE.decodeUtf8 <$> runGit env path ["hash-object", "-w", "--stdin"] bytes
    pure (name, oid)
  tree <- buildTree blobs
  commit <-
    T.strip . TE.decodeUtf8
      <$> runGit
        env
        path
        (["commit-tree", T.unpack tree] <> maybe [] (\p -> ["-p", T.unpack p]) previous)
        (TE.encodeUtf8 (message <> "\n"))
  (exit, _, _) <- tryGit env path ["update-ref", "refs/heads/main", T.unpack commit, maybe (replicate 40 '0') T.unpack previous] ""
  unless (exit == ExitSuccess) $ stale "Tracker revision changed during commit"
  pure commit
 where
  buildTree entries = do
    let direct = [(takeFileName name, oid) | (name, oid) <- entries, takeDirectory name == "."]
        nested =
          foldl'
            ( \acc (name, oid) -> case splitDirectories name of
                first : rest@(_ : _) -> Map.insertWith (<>) first [(foldl1 (</>) rest, oid)] acc
                _ -> acc
            )
            Map.empty
            entries
    subtrees <- forM (Map.toAscList nested) $ \(name, members) -> (name,) <$> buildTree members
    let render mode kind (name, oid) = TE.encodeUtf8 (mode <> " " <> kind <> " " <> oid <> "\t" <> T.pack name) <> "\NUL"
        input = BS.concat (map (render "100644" "blob") direct <> map (render "040000" "tree") subtrees)
    T.strip . TE.decodeUtf8 <$> runGit env path ["mktree", "-z"] input
