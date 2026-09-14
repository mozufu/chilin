module Chilin.Repository
  ( openEnv
  , openEnvWith
  , bootstrapAdmin
  , bootstrapIdentity
  , lookupActor
  , lookupIdentity
  , createUser
  , listTokens
  , createToken
  , revokeToken
  , listIdentities
  , linkIdentity
  , unlinkIdentity
  , listSshKeys
  , addSshKey
  , removeSshKey
  , lookupSshKey
  , createRepo
  , listRepos
  , lookupRepo
  , requireAccess
  , grantAccess
  , listPermissions
  , revokeAccess
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
import Data.ByteString.Base64 qualified as B64
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
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
openEnv root = openEnvWith root Nothing

openEnvWith :: FilePath -> Maybe ForwardAuth -> IO Env
openEnvWith root forward = do
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
    -- Presence of a row is the allowlist; proxy-asserted subjects without one
    -- are rejected. 'subject' is opaque, letting a provider migrate from a
    -- mutable login to a stable numeric id without a schema change.
    execute_ db "CREATE TABLE IF NOT EXISTS identities (provider TEXT NOT NULL, subject TEXT NOT NULL, user_name TEXT NOT NULL REFERENCES users(name) ON DELETE CASCADE, PRIMARY KEY(provider,subject))"
    -- Fingerprint is the primary key: sshd looks a key up by fingerprint, and
    -- the same key must never resolve to two accounts.
    execute_ db "CREATE TABLE IF NOT EXISTS ssh_keys (fingerprint TEXT PRIMARY KEY, user_name TEXT NOT NULL REFERENCES users(name) ON DELETE CASCADE, id TEXT NOT NULL UNIQUE, label TEXT NOT NULL, algorithm TEXT NOT NULL, key_blob TEXT NOT NULL, created_at TEXT NOT NULL)"
    columns <- query_ db "SELECT name FROM pragma_table_info('tokens')" :: IO [Only Text]
    let present name = Only name `elem` columns
    unless (present "id") $ do
      execute_ db "ALTER TABLE tokens ADD COLUMN id TEXT"
      execute_ db "UPDATE tokens SET id = lower(hex(randomblob(16))) WHERE id IS NULL"
      execute_ db "CREATE UNIQUE INDEX IF NOT EXISTS tokens_id ON tokens(id)"
    unless (present "label") $ execute_ db "ALTER TABLE tokens ADD COLUMN label TEXT NOT NULL DEFAULT 'legacy'"
    unless (present "created_at") $ execute_ db "ALTER TABLE tokens ADD COLUMN created_at TEXT NOT NULL DEFAULT ''"
  Env absolute git <$> newMVar db <*> newMVar Map.empty <*> pure forward

validateName :: Text -> IO ()
validateName name = unless (not (T.null name) && T.length name <= 100 && T.all allowed name && name /= "." && name /= "..") $ badRequest "Names must contain 1..100 ASCII letters, digits, hyphens, underscores or dots"
 where
  allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)

tokenDigest :: Text -> BS.ByteString
tokenDigest token = convert (hash (TE.encodeUtf8 token) :: Digest SHA256)

validateToken :: Text -> IO ()
validateToken token = unless (T.length token >= 32 && T.length token <= 4096 && T.all (\c -> c >= '!' && c <= '~') token) $ badRequest "Tokens must contain 32..4096 printable non-space ASCII characters"

validateLabel :: Text -> IO ()
validateLabel label =
  unless (not (T.null label) && T.length label <= 100 && T.all (\c -> c >= ' ' && c <= '~') label) $
    badRequest "Labels must contain 1..100 printable ASCII characters"

validateSubject :: Text -> IO ()
validateSubject subject =
  unless (not (T.null subject) && T.length subject <= 255 && T.all (\c -> c > ' ' && c <= '~') subject) $
    badRequest "Subjects must contain 1..255 printable non-space ASCII characters"

validateProvider :: Text -> IO ()
validateProvider provider =
  unless (not (T.null provider) && T.length provider <= 32 && T.all (\c -> isAsciiLower c || isDigit c || c == '-') provider) $
    badRequest "Providers must contain 1..32 lowercase ASCII letters, digits, or hyphens"

hex :: BS.ByteString -> Text
hex = T.pack . concatMap (\n -> let s = showHex n "" in if length s == 1 then '0' : s else s) . BS.unpack

-- 32 bytes of CSPRNG output rendered as 64 hex characters, satisfying
-- validateToken without depending on it.
newSecret :: IO Text
newSecret = hex <$> (getRandomBytes 32 :: IO BS.ByteString)

newTokenId :: IO Text
newTokenId = hex <$> (getRandomBytes 16 :: IO BS.ByteString)

timestamp :: IO Text
timestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime

insertToken :: Connection -> Text -> Text -> Text -> IO TokenInfo
insertToken db user secret label = do
  ident <- newTokenId
  now <- timestamp
  execute
    db
    "INSERT INTO tokens(digest,user_name,id,label,created_at) VALUES (?,?,?,?,?)"
    (tokenDigest secret, user, ident, label, now)
  pure (TokenInfo ident label now)

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
        _ <- insertToken db name token "bootstrap"
        pure ()
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

-- The secret is generated here and returned exactly once; only its digest is
-- persisted, so an administrator can never recover or choose a user's token.
createUser :: Env -> Actor -> Text -> IO (Actor, Text, TokenInfo)
createUser env actor name = do
  validateName name
  secret <- newSecret
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    allowed <- isAdmin db actor
    unless allowed $ forbidden "Administrator access required"
    names <- query db "SELECT name FROM users WHERE name=?" (Only name) :: IO [Only Text]
    unless (null names) $ conflict "User already exists"
    execute db "INSERT INTO users(name,admin) VALUES (?,0)" (Only name)
    info <- insertToken db name secret "initial"
    pure (Actor name False, secret, info)

listTokens :: Env -> Actor -> IO [TokenInfo]
listTokens env actor = withMVar (envDatabase env) $ \db -> do
  rows <- query db "SELECT id,label,created_at FROM tokens WHERE user_name=? ORDER BY created_at,id" (Only (actorId actor))
  pure [TokenInfo ident label createdAt | (ident, label, createdAt) <- rows]

-- Returns the secret alongside its metadata. Callers must surface it once and
-- never persist it; no later read path can reproduce it.
createToken :: Env -> Actor -> Text -> IO (Text, TokenInfo)
createToken env actor label = do
  validateLabel label
  secret <- newSecret
  info <- withMVar (envDatabase env) $ \db -> withTransaction db $ do
    count <- query db "SELECT COUNT(*) FROM tokens WHERE user_name=?" (Only (actorId actor)) :: IO [Only Int]
    when (count >= [Only 64]) $ conflict "Token limit reached; revoke an existing token first"
    insertToken db (actorId actor) secret label
  pure (secret, info)

-- A user may only revoke their own credentials, and never their last one:
-- Git access has no other authenticator.
revokeToken :: Env -> Actor -> Text -> IO ()
revokeToken env actor ident = withMVar (envDatabase env) $ \db -> withTransaction db $ do
  rows <- query db "SELECT id FROM tokens WHERE user_name=? AND id=?" (actorId actor, ident) :: IO [Only Text]
  when (null rows) $ notFound "Token not found"
  remaining <- query db "SELECT COUNT(*) FROM tokens WHERE user_name=?" (Only (actorId actor)) :: IO [Only Int]
  when (remaining <= [Only 1]) $ conflict "Cannot revoke the only remaining token"
  execute db "DELETE FROM tokens WHERE user_name=? AND id=?" (actorId actor, ident)

-- The allowlist gate. A proxy-asserted subject resolves to an account only
-- when an administrator has already linked it.
lookupIdentity :: Env -> Text -> Text -> IO (Maybe Actor)
lookupIdentity env provider subject = withMVar (envDatabase env) $ \db -> do
  rows <-
    query
      db
      "SELECT u.name,u.admin FROM identities i JOIN users u ON u.name=i.user_name WHERE i.provider=? AND i.subject=?"
      (provider, subject)
  pure $ case rows of
    [(name, admin)] -> Just (Actor name admin)
    _ -> Nothing

listIdentities :: Env -> Actor -> IO [IdentityInfo]
listIdentities env actor = withMVar (envDatabase env) $ \db -> do
  admin <- isAdmin db actor
  rows <-
    if admin
      then query_ db "SELECT provider,subject,user_name FROM identities ORDER BY provider,subject"
      else query db "SELECT provider,subject,user_name FROM identities WHERE user_name=? ORDER BY provider,subject" (Only (actorId actor))
  pure [IdentityInfo provider subject user | (provider, subject, user) <- rows]

linkIdentity :: Env -> Actor -> Text -> Text -> Text -> IO IdentityInfo
linkIdentity env actor provider subject user = do
  validateProvider provider
  validateSubject subject
  validateName user
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    allowed <- isAdmin db actor
    unless allowed $ forbidden "Administrator access required"
    users <- query db "SELECT name FROM users WHERE name=?" (Only user) :: IO [Only Text]
    when (null users) $ notFound "User does not exist"
    existing <- query db "SELECT user_name FROM identities WHERE provider=? AND subject=?" (provider, subject) :: IO [Only Text]
    unless (null existing) $ conflict "Identity is already linked"
    execute db "INSERT INTO identities(provider,subject,user_name) VALUES (?,?,?)" (provider, subject, user)
    pure (IdentityInfo provider subject user)

-- Forward auth only admits subjects that are already linked, and linking over
-- HTTP needs an authenticated administrator, so a freshly provisioned host has
-- no way in. This is the offline counterpart to bootstrapAdmin: it runs as the
-- operator on the machine holding the data, and is idempotent so it can sit in
-- a declarative activation script that reruns on every deploy.
bootstrapIdentity :: Env -> Text -> Text -> Text -> IO ()
bootstrapIdentity env provider subject user = do
  validateProvider provider
  validateSubject subject
  validateName user
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    users <- query db "SELECT name FROM users WHERE name=?" (Only user) :: IO [Only Text]
    when (null users) $ notFound "User does not exist"
    existing <- query db "SELECT user_name FROM identities WHERE provider=? AND subject=?" (provider, subject) :: IO [Only Text]
    case existing of
      [] -> execute db "INSERT INTO identities(provider,subject,user_name) VALUES (?,?,?)" (provider, subject, user)
      [Only owner] | owner == user -> pure ()
      _ -> conflict "Identity is already linked to a different user"

-- An authorized_keys entry is "<algorithm> <base64 blob> [comment]". The blob
-- re-declares its own algorithm in its first field; a mismatch means the entry
-- is malformed or crafted, so it is rejected rather than normalised.
parsePublicKey :: Text -> IO (Text, Text, Text)
parsePublicKey raw = do
  let stripped = T.strip raw
  when (T.length stripped > 16384) $ badRequest "Public key is too large"
  (algorithm, rest) <- case T.break (== ' ') stripped of
    (a, r) | not (T.null a) && not (T.null r) -> pure (a, T.stripStart r)
    _ -> badRequest "Public keys must be in authorized_keys format: <algorithm> <base64> [comment]"
  let blob = T.takeWhile (/= ' ') rest
  unless (algorithm `elem` supportedAlgorithms) $
    badRequest ("Unsupported key algorithm; expected one of " <> T.intercalate ", " supportedAlgorithms)
  decoded <- case B64.decode (TE.encodeUtf8 blob) of
    Right bytes | not (BS.null bytes) -> pure bytes
    _ -> badRequest "Public key body is not valid base64"
  declared <- maybe (badRequest "Public key body is malformed") pure (sshString decoded)
  unless (declared == TE.encodeUtf8 algorithm) $
    badRequest "Public key body does not match its declared algorithm"
  pure (algorithm, blob, fingerprintOf decoded)
 where
  -- DSA is omitted deliberately: OpenSSH rejects it by default, so accepting a
  -- key here that can never authenticate would be a trap.
  supportedAlgorithms =
    [ "ssh-ed25519"
    , "ssh-rsa"
    , "ecdsa-sha2-nistp256"
    , "ecdsa-sha2-nistp384"
    , "ecdsa-sha2-nistp521"
    , "sk-ssh-ed25519@openssh.com"
    , "sk-ecdsa-sha2-nistp256@openssh.com"
    ]

-- SSH wire format prefixes each field with a 32-bit big-endian length.
sshString :: BS.ByteString -> Maybe BS.ByteString
sshString bytes = do
  unless (BS.length bytes >= 4) Nothing
  let len = foldl' (\acc i -> acc * 256 + fromIntegral (BS.index bytes i)) (0 :: Int) [0 .. 3]
  unless (len > 0 && len <= BS.length bytes - 4) Nothing
  pure (BS.take len (BS.drop 4 bytes))

-- OpenSSH renders fingerprints as unpadded base64 of the SHA-256 digest.
fingerprintOf :: BS.ByteString -> Text
fingerprintOf decoded =
  "SHA256:" <> T.dropWhileEnd (== '=') (TE.decodeUtf8 (B64.encode (convert (hash decoded :: Digest SHA256))))

listSshKeys :: Env -> Actor -> IO [SshKeyInfo]
listSshKeys env actor = withMVar (envDatabase env) $ \db -> do
  rows <- query db "SELECT id,label,fingerprint,created_at FROM ssh_keys WHERE user_name=? ORDER BY created_at,id" (Only (actorId actor))
  pure [SshKeyInfo ident label fingerprint createdAt | (ident, label, fingerprint, createdAt) <- rows]

addSshKey :: Env -> Actor -> Text -> Text -> IO SshKeyInfo
addSshKey env actor label raw = do
  validateLabel label
  (algorithm, blob, fingerprint) <- parsePublicKey raw
  ident <- newTokenId
  now <- timestamp
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    count <- query db "SELECT COUNT(*) FROM ssh_keys WHERE user_name=?" (Only (actorId actor)) :: IO [Only Int]
    when (count >= [Only 64]) $ conflict "SSH key limit reached; remove an existing key first"
    existing <- query db "SELECT user_name FROM ssh_keys WHERE fingerprint=?" (Only fingerprint) :: IO [Only Text]
    -- Reusing a key across accounts would make the SSH identity ambiguous.
    unless (null existing) $ conflict "This public key is already registered"
    execute
      db
      "INSERT INTO ssh_keys(fingerprint,user_name,id,label,algorithm,key_blob,created_at) VALUES (?,?,?,?,?,?,?)"
      (fingerprint, actorId actor, ident, label, algorithm, blob, now)
    pure (SshKeyInfo ident label fingerprint now)

-- Unlike tokens, the last key may be removed: HTTP token auth still works, so
-- this cannot lock a user out.
removeSshKey :: Env -> Actor -> Text -> IO ()
removeSshKey env actor ident = withMVar (envDatabase env) $ \db -> withTransaction db $ do
  rows <- query db "SELECT id FROM ssh_keys WHERE user_name=? AND id=?" (actorId actor, ident) :: IO [Only Text]
  when (null rows) $ notFound "SSH key not found"
  execute db "DELETE FROM ssh_keys WHERE user_name=? AND id=?" (actorId actor, ident)

-- Resolves the account sshd should run the forced command as. The fingerprint
-- comes from sshd itself, never from the client.
lookupSshKey :: Env -> Text -> IO (Maybe (Actor, Text, Text))
lookupSshKey env fingerprint = withMVar (envDatabase env) $ \db -> do
  rows <-
    query
      db
      "SELECT u.name,u.admin,k.algorithm,k.key_blob FROM ssh_keys k JOIN users u ON u.name=k.user_name WHERE k.fingerprint=?"
      (Only fingerprint)
  pure $ case rows of
    [(name, admin, algorithm, blob)] -> Just (Actor name admin, algorithm, blob)
    _ -> Nothing

unlinkIdentity :: Env -> Actor -> Text -> Text -> IO ()
unlinkIdentity env actor provider subject = do
  validateProvider provider
  validateSubject subject
  withMVar (envDatabase env) $ \db -> withTransaction db $ do
    allowed <- isAdmin db actor
    unless allowed $ forbidden "Administrator access required"
    rows <- query db "SELECT user_name FROM identities WHERE provider=? AND subject=?" (provider, subject) :: IO [Only Text]
    when (null rows) $ notFound "Identity not found"
    execute db "DELETE FROM identities WHERE provider=? AND subject=?" (provider, subject)

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
      hexDigit c = isDigit c || c >= 'a' && c <= 'f'
  unless (map T.length parts == [8, 4, 4, 4, 12] && all (T.all hexDigit) parts) $
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

accessFromRank :: Int -> IO Access
accessFromRank 0 = pure ReadAccess
accessFromRank 1 = pure WriteAccess
accessFromRank 2 = pure AdminAccess
accessFromRank _ = throwIO (AppError status500 "Invalid access rank in registry")

-- The owner holds implicit administrative access and has no permissions row,
-- so it is reported explicitly; otherwise the list would suggest a repository
-- with no administrator.
listPermissions :: Env -> Repo -> IO [(Text, Access)]
listPermissions env repo = withMVar (envDatabase env) $ \db -> do
  rows <-
    query
      db
      "SELECT user_name,access FROM permissions WHERE repo_id=? ORDER BY user_name"
      (Only (repoId repo))
      :: IO [(Text, Int)]
  granted <- traverse (\(user, rank) -> (,) user <$> accessFromRank rank) rows
  pure ((repoOwner repo, AdminAccess) : filter ((/= repoOwner repo) . fst) granted)

-- The owner's access is structural, not a grant, so there is no row to delete
-- and removing it would leave the repository unadministrable.
revokeAccess :: Env -> Actor -> Repo -> Text -> IO ()
revokeAccess env actor repo user = withRepoLock env repo $ do
  requireAccess env (Just actor) repo AdminAccess
  validateName user
  when (user == repoOwner repo) $ conflict "The repository owner's access cannot be revoked"
  withMVar (envDatabase env) $ \db -> do
    rows <- query db "SELECT user_name FROM permissions WHERE repo_id=? AND user_name=?" (repoId repo, user) :: IO [Only Text]
    when (null rows) $ notFound "User has no access to this repository"
    execute db "DELETE FROM permissions WHERE repo_id=? AND user_name=?" (repoId repo, user)
