module Chilin.Repository
  ( openEnv
  , bootstrapAdmin
  , lookupActor
  , createUser
  , createRepo
  , listRepos
  , lookupRepo
  , requireAccess
  , grantAccess
  , allRepos
  , actorExists
  ) where

import Chilin.Git (codePath, initBare, withRepoLock)
import Chilin.Store (initialTracker)
import Chilin.Types
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (mask, onException, throwIO)
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (constEq, convert)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.HTTP.Types (status500)
import Numeric (showHex)
import System.Directory
import System.Environment (lookupEnv)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

openEnv :: FilePath -> IO Env
openEnv root = do
  -- myque's public filesystem loader uses the process text encoding.
  setLocaleEncoding utf8
  createDirectoryIfMissing True root
  absolute <- canonicalizePath root
  setFileMode absolute 0o700
  createDirectoryIfMissing True (absolute </> "repos")
  candidate <- lookupEnv "CHILIN_GIT"
  executable <- maybe (findExecutable "git") findExecutable candidate
  git <- maybe (throwIO (AppError status500 "Git executable is unavailable")) canonicalizePath executable
  db <- open (absolute </> "registry.sqlite")
  setFileMode (absolute </> "registry.sqlite") 0o600
  execute_ db "PRAGMA foreign_keys = ON"
  execute_ db "PRAGMA busy_timeout = 10000"
  _ <- query_ db "PRAGMA journal_mode = WAL" :: IO [Only Text]
  withTransaction db $ do
    execute_ db "CREATE TABLE IF NOT EXISTS users (name TEXT PRIMARY KEY, admin INTEGER NOT NULL CHECK(admin IN (0,1)))"
    execute_ db "CREATE TABLE IF NOT EXISTS tokens (digest BLOB PRIMARY KEY, user_name TEXT NOT NULL REFERENCES users(name) ON DELETE CASCADE)"
    execute_ db "CREATE TABLE IF NOT EXISTS repositories (id TEXT PRIMARY KEY, owner TEXT NOT NULL REFERENCES users(name), name TEXT NOT NULL, public INTEGER NOT NULL CHECK(public IN (0,1)), state TEXT NOT NULL CHECK(state IN ('pending','ready')), UNIQUE(owner,name))"
    execute_ db "CREATE TABLE IF NOT EXISTS permissions (repo_id TEXT NOT NULL REFERENCES repositories(id) ON DELETE CASCADE, user_name TEXT NOT NULL REFERENCES users(name) ON DELETE CASCADE, access INTEGER NOT NULL CHECK(access BETWEEN 0 AND 2), PRIMARY KEY(repo_id,user_name))"
  Env absolute git <$> newMVar db <*> newMVar Map.empty

validateName :: Text -> IO ()
validateName name = unless (not (T.null name) && T.length name <= 100 && T.all allowed name && name /= "." && name /= "..") $ badRequest "Names must contain 1..100 ASCII letters, digits, hyphens, underscores or dots"
 where
  allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)

tokenDigest :: Text -> BS.ByteString
tokenDigest token = convert (hash (TE.encodeUtf8 token) :: Digest SHA256)

validateToken :: Text -> IO ()
validateToken token = unless (T.length token >= 32 && T.length token <= 4096 && T.all (\c -> c >= '!' && c <= '~') token) $ badRequest "Tokens must contain 32..4096 printable non-space ASCII characters"

bootstrapAdmin :: Env -> Text -> Text -> IO ()
bootstrapAdmin env name token = do
  validateName name
  validateToken token
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    users <- query db "SELECT admin FROM users WHERE name = ?" (Only name) :: IO [Only Bool]
    tokens <- query db "SELECT digest,user_name FROM tokens WHERE digest = ?" (Only (tokenDigest token)) :: IO [(BS.ByteString, Text)]
    case (users, tokens) of
      ([Only True], [(digest, owner)]) | owner == name && constEq digest (tokenDigest token) -> pure ()
      ([], []) -> do
        execute db "INSERT INTO users(name,admin) VALUES (?,1)" (Only name)
        execute db "INSERT INTO tokens(digest,user_name) VALUES (?,?)" (tokenDigest token, name)
      _ -> conflict "Bootstrap identity or token conflicts with the registry"

lookupActor :: Env -> Text -> IO (Maybe Actor)
lookupActor env token
  | T.length token < 32 || T.length token > 4096 = pure Nothing
  | otherwise = withMVar (envDatabase env) $ \db -> do
      rows <- query db "SELECT u.name,u.admin,t.digest FROM tokens t JOIN users u ON u.name=t.user_name WHERE t.digest=?" (Only digest) :: IO [(Text, Bool, BS.ByteString)]
      pure $ case rows of
        [(name, admin, stored)] | constEq stored digest -> Just (Actor name admin)
        _ -> Nothing
 where
  digest = tokenDigest token

actorExists :: Env -> Text -> IO Bool
actorExists env name = withMVar (envDatabase env) $ \db -> do
  rows <- query db "SELECT name FROM users WHERE name=?" (Only name) :: IO [Only Text]
  pure (not (null rows))

isAdmin :: Connection -> Actor -> IO Bool
isAdmin db actor = do
  rows <- query db "SELECT admin FROM users WHERE name=?" (Only (actorId actor)) :: IO [Only Bool]
  pure (rows == [Only True])

createUser :: Env -> Actor -> Text -> Text -> IO Actor
createUser env actor name token = do
  validateName name
  validateToken token
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    allowed <- isAdmin db actor
    unless allowed $ forbidden "Administrator access required"
    names <- query db "SELECT name FROM users WHERE name=?" (Only name) :: IO [Only Text]
    tokens <- query db "SELECT user_name FROM tokens WHERE digest=?" (Only (tokenDigest token)) :: IO [Only Text]
    unless (null names && null tokens) $ conflict "User or token already exists"
    execute db "INSERT INTO users(name,admin) VALUES (?,0)" (Only name)
    execute db "INSERT INTO tokens(digest,user_name) VALUES (?,?)" (tokenDigest token, name)
    pure (Actor name False)

newRepoId :: IO Text
newRepoId = do
  bytes <- getRandomBytes 16 :: IO BS.ByteString
  let raw = concatMap (\n -> let s = showHex n "" in if length s == 1 then '0' : s else s) (BS.unpack bytes)
      -- Random UUID v4; all physical paths come from this identifier, never names.
      uuid = take 8 raw ++ "-" ++ take 4 (drop 8 raw) ++ "-4" ++ take 3 (drop 13 raw) ++ "-" ++ ["89ab" !! (fromIntegral (BS.index bytes 8) `mod` 4)] ++ take 3 (drop 17 raw) ++ "-" ++ drop 20 raw
  pure (T.pack uuid)

createRepo :: Env -> Actor -> Text -> Text -> Bool -> IO Repo
createRepo env actor owner name public = do
  validateName owner
  validateName name
  when (T.isSuffixOf ".git" name || T.isSuffixOf ".tracker" name) $ badRequest "Repository names cannot end in .git or .tracker"
  withFileLock (envRoot env </> "registry.lock") Exclusive $ \_ -> mask $ \restore -> do
    ident <- newRepoId
    let repo = Repo ident owner name public
        directory = envRoot env </> "repos" </> T.unpack ident
    withMVar (envDatabase env) $ \db -> withTransaction db $ do
      admin <- isAdmin db actor
      unless (admin || owner == actorId actor) $ forbidden "Cannot create repositories for another owner"
      owners <- query db "SELECT name FROM users WHERE name=?" (Only owner) :: IO [Only Text]
      when (null owners) $ notFound "Owner does not exist"
      previous <- query db "SELECT id,state FROM repositories WHERE owner=? AND name=?" (owner, name) :: IO [(Text, Text)]
      -- Only canonical generated UUID paths may be removed during crash recovery.
      case previous of
        [(_, "ready")] -> conflict "Repository already exists"
        [(oldId, "pending")] -> do
          removePendingDirectory env oldId
          execute db "DELETE FROM repositories WHERE owner=? AND name=? AND state='pending'" (owner, name)
        [] -> pure ()
        _ -> conflict "Repository registry is inconsistent"
      execute db "INSERT INTO repositories(id,owner,name,public,state) VALUES (?,?,?,?,'pending')" (ident, owner, name, public)
    let abandon = do
          withMVar (envDatabase env) $ \db -> execute db "DELETE FROM repositories WHERE id=? AND state='pending'" (Only ident)
          exists <- doesDirectoryExist directory
          when exists $ removePathForcibly directory
    ( do
        restore $ do
          createDirectory directory
          initBare env (codePath env repo)
          initialTracker env repo
        withMVar (envDatabase env) $ \db -> execute db "UPDATE repositories SET state='ready' WHERE id=? AND state='pending'" (Only ident)
        pure repo
      )
      `onException` abandon

removePendingDirectory :: Env -> Text -> IO ()
removePendingDirectory env ident = do
  let parts = T.splitOn "-" ident
      hex c = isDigit c || c >= 'a' && c <= 'f'
  unless (map T.length parts == [8, 4, 4, 4, 12] && all (T.all hex) parts) $
    conflict "Invalid pending repository identity"
  let directory = envRoot env </> "repos" </> T.unpack ident
  exists <- doesPathExist directory
  when exists $ do
    symbolic <- pathIsSymbolicLink directory
    when symbolic $ conflict "Pending repository path is a symbolic link"
    removePathForcibly directory

rowRepo :: (Text, Text, Text, Bool) -> Repo
rowRepo (ident, owner, name, public) = Repo ident owner name public

allRepos :: Env -> IO [Repo]
allRepos env = withMVar (envDatabase env) $ \db -> map rowRepo <$> query_ db "SELECT id,owner,name,public FROM repositories WHERE state='ready' ORDER BY owner,name"

listRepos :: Env -> Maybe Actor -> IO [Repo]
listRepos env actor = withMVar (envDatabase env) $ \db -> do
  admin <- maybe (pure False) (isAdmin db) actor
  if admin
    then map rowRepo <$> query_ db "SELECT id,owner,name,public FROM repositories WHERE state='ready' ORDER BY owner,name"
    else case actor of
      Nothing -> map rowRepo <$> query_ db "SELECT id,owner,name,public FROM repositories WHERE state='ready' AND public=1 ORDER BY owner,name"
      Just user -> map rowRepo <$> query db "SELECT r.id,r.owner,r.name,r.public FROM repositories r WHERE r.state='ready' AND (r.public=1 OR r.owner=? OR EXISTS (SELECT 1 FROM permissions p WHERE p.repo_id=r.id AND p.user_name=?)) ORDER BY r.owner,r.name" (actorId user, actorId user)

lookupRepo :: Env -> Text -> Text -> IO Repo
lookupRepo env owner name = do
  validateName owner
  validateName name
  withMVar (envDatabase env) $ \db -> do
    rows <- query db "SELECT id,owner,name,public FROM repositories WHERE owner=? AND name=? AND state='ready'" (owner, name)
    case rows of
      [row] -> pure (rowRepo row)
      _ -> notFound "Repository not found"

accessRank :: Access -> Int
accessRank ReadAccess = 0
accessRank WriteAccess = 1
accessRank AdminAccess = 2

requireAccess :: Env -> Maybe Actor -> Repo -> Access -> IO ()
requireAccess env actor repo access = withMVar (envDatabase env) $ \db -> do
  rows <- query db "SELECT owner,public FROM repositories WHERE id=? AND state='ready'" (Only (repoId repo)) :: IO [(Text, Bool)]
  allowed <- case rows of
    [(owner, public)] -> case actor of
      Nothing -> pure (public && access == ReadAccess)
      Just user -> do
        admin <- isAdmin db user
        grants <- query db "SELECT access FROM permissions WHERE repo_id=? AND user_name=?" (repoId repo, actorId user) :: IO [Only Int]
        pure (admin || owner == actorId user || public && access == ReadAccess || any (\(Only rank) -> rank >= accessRank access) grants)
    _ -> pure False
  unless allowed $ if access == ReadAccess then notFound "Repository not found" else forbidden "Repository access denied"

grantAccess :: Env -> Actor -> Repo -> Text -> Access -> IO ()
grantAccess env actor repo user access = withRepoLock env repo $ do
  requireAccess env (Just actor) repo AdminAccess
  validateName user
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    users <- query db "SELECT name FROM users WHERE name=?" (Only user) :: IO [Only Text]
    when (null users) $ notFound "User does not exist"
    execute db "INSERT INTO permissions(repo_id,user_name,access) VALUES (?,?,?) ON CONFLICT(repo_id,user_name) DO UPDATE SET access=excluded.access" (repoId repo, user, accessRank access)
