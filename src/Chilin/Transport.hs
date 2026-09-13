module Chilin.Transport (gitHttp, runSSH) where

import Chilin.Git (codePath, trackerPath, withRepoLock)
import Chilin.Pulls (recoverRepoLocked)
import Chilin.Repository (lookupActor, lookupRepo, requireAccess)
import Chilin.Types
import Control.Concurrent.Async (concurrently, concurrently_, link, wait, withAsync)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types
import Network.HTTP.Types.Header (hExpires, hPragma, hVary)
import Network.Wai
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, stdin, stdout)
import System.IO.Error (isResourceVanishedError)
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- No ambient credentials, Git configuration, proxy settings, hooks, or helpers
-- cross the network-facing process boundary. Only server-chosen config survives.
secureEnvironment :: Env -> [(String, String)]
secureEnvironment env =
  [ ("PATH", takeDirectory (envGit env) ++ ":/usr/bin:/bin")
  , ("HOME", "/dev/null")
  , ("XDG_CONFIG_HOME", "/dev/null")
  , ("GIT_CONFIG_NOSYSTEM", "1")
  , ("GIT_CONFIG_GLOBAL", "/dev/null")
  , ("GIT_TERMINAL_PROMPT", "0")
  , ("GIT_ATTR_NOSYSTEM", "1")
  , ("GIT_PROTOCOL_FROM_USER", "0")
  , ("LANG", "C")
  , ("LC_ALL", "C")
  , ("GIT_NO_REPLACE_OBJECTS", "1")
  , ("TMPDIR", "/tmp")
  ]

secureArguments :: [String]
secureArguments =
  concatMap
    (\setting -> ["-c", setting])
    [ "core.hooksPath=/dev/null"
    , "core.fsmonitor=false"
    , "protocol.allow=never"
    , "receive.fsckObjects=true"
    , "transfer.fsckObjects=true"
    , "receive.maxInputSize=67108864"
    , "receive.hideRefs=refs"
    , "receive.hideRefs=!refs/heads"
    , "receive.hideRefs=!refs/tags"
    , "uploadpack.hideRefs=refs/chilin"
    , "uploadpack.hideRefs=refs/replace"
    , "uploadpack.allowAnySHA1InWant=false"
    , "uploadpack.allowTipSHA1InWant=false"
    , "uploadpack.allowReachableSHA1InWant=false"
    , "http.receivepack=true"
    , "http.uploadpack=true"
    , "http.maxRequestBuffer=67108864"
    , "pack.threads=1"
    , "pack.windowMemory=32m"
    , "pack.deltaCacheSize=32m"
    ]

bounded :: Int -> IO BS.ByteString -> IO BS.ByteString
bounded limit next = go 0 []
 where
  go size chunks = do
    chunk <- next
    if BS.null chunk
      then pure (BS.concat (reverse chunks))
      else do
        let total = size + BS.length chunk
        when (total > limit) $ throwIO (AppError status413 "Git transfer exceeds server limit")
        go total (chunk : chunks)

deadline :: Int -> IO a -> IO a
deadline seconds action = timeout (seconds * 1000000) action >>= maybe (throwIO (AppError status504 "Git transfer timed out")) pure

runBackend :: Env -> [(String, String)] -> BS.ByteString -> IO BS.ByteString
runBackend env variables input = deadline 120
  $ withCreateProcess
    ( (proc (envGit env) (secureArguments ++ ["http-backend"]))
        { env = Just (variables ++ secureEnvironment env)
        , cwd = Just (envRoot env)
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }
    )
  $ \hin hout herr ph -> case (hin, hout, herr) of
    (Just inputHandle, Just outputHandle, Just errorHandle) -> do
      mapM_ (`hSetBinaryMode` True) [inputHandle, outputHandle, errorHandle]
      withAsync (BS.hPut inputHandle input >> hClose inputHandle) $ \writer -> do
        ((output, _), exitCode) <-
          concurrently
            (concurrently (bounded (256 * 1024 * 1024) (BS.hGetSome outputHandle 32768)) (bounded (1024 * 1024) (BS.hGetSome errorHandle 32768)))
            (waitForProcess ph)
        wait writer `catch` \(err :: IOException) -> unless (isResourceVanishedError err) (throwIO err)
        unless (exitCode == ExitSuccess) $ throwIO (AppError status502 "Git backend failed")
        pure output
    _ -> throwIO (AppError status500 "Git process pipes unavailable")

gitHttp :: Env -> Maybe Actor -> Repo -> Bool -> [Text] -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
gitHttp env actor repo tracker suffix request respond = do
  receive <- case (requestMethod request, suffix) of
    ("GET", ["info", "refs"]) -> case queryString request of
      [("service", Just "git-upload-pack")] -> pure False
      [("service", Just "git-receive-pack")] -> pure True
      _ -> badRequest "Expected exactly one supported Git service"
    ("POST", ["git-upload-pack"]) -> pure False
    ("POST", ["git-receive-pack"]) -> pure True
    _ -> notFound "Git endpoint not found"
  when (receive && isNothing actor) $ throwIO (AppError status401 "Git authentication required")
  when (tracker && receive) $ forbidden "Tracker Git repositories are read-only"
  requireAccess env actor repo (if receive then WriteAccess else ReadAccess)
  let service = if receive then "git-receive-pack" else "git-upload-pack"
  when (requestMethod request == "POST") $ do
    unless (null (queryString request)) $ badRequest "Unexpected Git RPC query parameters"
    unless (lookup hContentType (requestHeaders request) == Just ("application/x-" <> service <> "-request")) $ throwIO (AppError status415 "Unsupported Git RPC content type")
  encoding <- case [value | (name, value) <- requestHeaders request, name == hContentEncoding] of
    [] -> pure "identity"
    [value] | value `elem` ["identity", "gzip", "x-gzip"] -> pure value
    _ -> throwIO (AppError status415 "Unsupported Git content encoding")
  case requestBodyLength request of
    KnownLength n | n > 67108864 -> throwIO (AppError status413 "Git request exceeds 64 MiB")
    _ -> pure ()
  body <- deadline 120 $ bounded 67108864 (getRequestBodyChunk request)
  when (requestMethod request == "GET" && not (BS.null body)) $ badRequest "Git discovery cannot have a request body"
  protocol <- case lookup "Git-Protocol" (requestHeaders request) of
    Nothing -> pure []
    Just value | BS.length value <= 256 && BS.all (\c -> c >= 32 && c <= 126) value -> pure [("HTTP_GIT_PROTOCOL", B8.unpack value)]
    _ -> badRequest "Invalid Git protocol header"
  let physical = if tracker then trackerPath env repo else codePath env repo
      variables =
        [ ("GIT_PROJECT_ROOT", takeDirectory physical)
        , ("GIT_HTTP_EXPORT_ALL", "1")
        , ("PATH_INFO", "/" ++ takeFileName physical ++ "/" ++ T.unpack (T.intercalate "/" suffix))
        , ("REQUEST_METHOD", B8.unpack (requestMethod request))
        , ("QUERY_STRING", if requestMethod request == "GET" then "service=" ++ B8.unpack service else "")
        , ("CONTENT_TYPE", B8.unpack (fromMaybe "" (lookup hContentType (requestHeaders request))))
        , ("CONTENT_LENGTH", show (BS.length body))
        , ("HTTP_CONTENT_ENCODING", B8.unpack encoding)
        , ("SERVER_PROTOCOL", "HTTP/1.1")
        , ("GATEWAY_INTERFACE", "CGI/1.1")
        , ("REMOTE_USER", maybe "anonymous" (T.unpack . actorId) actor)
        ]
          ++ protocol
      perform = runBackend env variables body
  bytes <- if receive then withRepoLock env repo (requireAccess env actor repo WriteAccess >> recoverRepoLocked env repo >> perform) else perform
  response <- cgiResponse bytes
  respond response

cgiResponse :: BS.ByteString -> IO Response
cgiResponse bytes = do
  let (headers, rest) = BS.breakSubstring "\r\n\r\n" bytes
      (headerBytes, body) =
        if BS.null rest
          then
            let (h, b) = BS.breakSubstring "\n\n" bytes in (h, BS.drop 2 b)
          else (headers, BS.drop 4 rest)
  when (BS.length headerBytes > 65536 || BS.null rest && not ("\n\n" `BS.isInfixOf` bytes)) $ throwIO (AppError status502 "Invalid Git CGI response")
  parsed <- mapM parseHeader (B8.lines headerBytes)
  status <- case [v | (k, v) <- parsed, k == "Status"] of
    [] -> pure status200
    [value] -> case B8.words value of
      code : reason -> case readMaybe (B8.unpack code) of
        Just n | n >= 200 && n <= 599 -> pure (mkStatus n (B8.unwords reason))
        _ -> invalid
      _ -> invalid
    _ -> invalid
  let allowed = [hContentType, hCacheControl, hExpires, hPragma, hVary]
  pure $ responseLBS status [(k, v) | (k, v) <- parsed, k `elem` allowed] (BL.fromStrict body)
 where
  invalid = throwIO (AppError status502 "Invalid Git CGI status")
  parseHeader line = do
    let clean = if "\r" `BS.isSuffixOf` line then BS.init line else line
        (name, value) = B8.break (== ':') clean
    unless (not (BS.null name) && not (BS.null value) && BS.all (\c -> c >= 33 && c <= 126 && c /= 58) name && BS.all (\c -> c == 9 || c >= 32 && c < 127) (BS.drop 1 value)) $ throwIO (AppError status502 "Invalid Git CGI headers")
    pure (CI.mk name, B8.dropWhile (== ' ') (BS.drop 1 value))

parseCommand :: Text -> IO (Bool, Text, Text, Bool)
parseCommand command = do
  (receive, argument) <- case T.breakOn " " command of
    ("git-upload-pack", rest) -> pure (False, T.drop 1 rest)
    ("git-receive-pack", rest) -> pure (True, T.drop 1 rest)
    _ -> forbidden "Only Git upload-pack and receive-pack are allowed"
  let path = if T.length argument >= 2 && T.head argument == '\'' && T.last argument == '\'' then T.init (T.tail argument) else argument
  unless (not (T.null path) && T.all (\c -> c >= '!' && c <= '~' && c `notElem` ("'\"\\" :: String)) path) $ forbidden "Invalid SSH Git path"
  let routedPath = fromMaybe path (T.stripPrefix "/" path)
  case T.splitOn "/" routedPath of
    [owner, file] -> case T.stripSuffix ".git" file of
      Just name | not (T.null owner) && not (T.null name) -> case T.stripSuffix ".tracker" name of
        Just source -> pure (receive, owner, source, True)
        Nothing -> pure (receive, owner, name, False)
      _ -> forbidden "SSH paths must be owner/repository.git or owner/repository.tracker.git"
    _ -> forbidden "Invalid SSH repository path"

copyBounded :: Int -> Handle -> Handle -> IO ()
copyBounded limit source destination = go 0
 where
  go count = do
    bytes <- BS.hGetSome source 32768
    unless (BS.null bytes) $ do
      let total = count + BS.length bytes
      when (total > limit) $ throwIO (AppError status413 "SSH Git transfer exceeds server limit")
      BS.hPut destination bytes
      hFlush destination
      go total

runSSH :: Env -> Text -> Text -> IO ()
runSSH env token command = do
  actor <- lookupActor env token >>= maybe (throwIO (AppError status401 "Invalid SSH credential")) pure
  (receive, owner, name, tracker) <- parseCommand command
  repo <- lookupRepo env owner name
  when (receive && tracker) $ forbidden "Tracker Git repositories are read-only"
  requireAccess env (Just actor) repo (if receive then WriteAccess else ReadAccess)
  let path = if tracker then trackerPath env repo else codePath env repo
      run = deadline 600
        $ withCreateProcess
          ( (proc (envGit env) (secureArguments ++ [if receive then "receive-pack" else "upload-pack", path]))
              { env = Just (secureEnvironment env)
              , cwd = Just (envRoot env)
              , std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              , create_group = True
              }
          )
        $ \hin hout herr ph -> case (hin, hout, herr) of
          (Just inputHandle, Just outputHandle, Just errorHandle) -> do
            mapM_ (`hSetBinaryMode` True) [stdin, stdout, inputHandle, outputHandle, errorHandle]
            -- Git clients keep stdin open until the server closes stdout. Race-free
            -- lifetime is owned by process completion, not client EOF.
            (_, exitCode) <-
              concurrently
                ( concurrently_
                    (copyBounded (256 * 1024 * 1024) outputHandle stdout)
                    (void (bounded (1024 * 1024) (BS.hGetSome errorHandle 32768)))
                )
                (withInput inputHandle ph)
            unless (exitCode == ExitSuccess) $ throwIO (AppError status502 "SSH Git process failed")
          _ -> throwIO (AppError status500 "SSH process pipes unavailable")
      withInput inputHandle ph = do
        -- The input pump is cancelled as soon as the finite child exits.
        raceInput (copyBounded 67108864 stdin inputHandle >> hClose inputHandle) (waitForProcess ph)
  if receive then withRepoLock env repo (requireAccess env (Just actor) repo WriteAccess >> recoverRepoLocked env repo >> run) else run

raceInput :: IO () -> IO ExitCode -> IO ExitCode
raceInput pump waitChild = do
  -- A completed input stream does not mean the server has finished responding.
  withAsync pump $ \worker -> do
    link worker
    waitChild
