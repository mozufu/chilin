module Chilin.Types where

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status400, status403, status404, status409, status412)

-- Identity asserted by a trusted reverse proxy. Only honoured for loopback
-- peers; see Chilin.Server.forwardActor.
data ForwardAuth = ForwardAuth
  { forwardHeader :: CI ByteString
  , forwardProvider :: Text
  }
  deriving (Eq, Show)

data Env = Env
  { envRoot :: FilePath
  , envGit :: FilePath
  , envDatabase :: MVar Connection
  , envLocks :: MVar (Map Text (MVar ()))
  , envForwardAuth :: Maybe ForwardAuth
  }

data Actor = Actor
  { actorId :: Text
  , actorAdmin :: Bool
  }
  deriving (Eq, Show, Generic)

-- Field names are part of the HTTP contract, so they are written explicitly
-- rather than derived from the Haskell record selectors.
instance ToJSON Actor where
  toJSON a = object ["id" .= actorId a, "admin" .= actorAdmin a]

instance FromJSON Actor where
  parseJSON = withObject "Actor" $ \o -> Actor <$> o .: "id" <*> o .: "admin"

-- Credential metadata. The secret itself is never stored, only its digest,
-- so a token value is returned exactly once by createToken.
data TokenInfo = TokenInfo
  { tokenInfoId :: Text
  , tokenInfoLabel :: Text
  , tokenInfoCreatedAt :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON TokenInfo where
  toJSON t = object ["id" .= tokenInfoId t, "label" .= tokenInfoLabel t, "created_at" .= tokenInfoCreatedAt t]

-- An external identity permitted to assume a chilin account. Presence in this
-- table is the allowlist: proxy-asserted subjects without a row are rejected.
data IdentityInfo = IdentityInfo
  { identityProvider :: Text
  , identitySubject :: Text
  , identityUser :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON IdentityInfo where
  toJSON i = object ["provider" .= identityProvider i, "subject" .= identitySubject i, "user" .= identityUser i]

data Repo = Repo
  { repoId :: Text
  , repoOwner :: Text
  , repoName :: Text
  , repoPublic :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Repo where
  toJSON r = object ["id" .= repoId r, "owner" .= repoOwner r, "name" .= repoName r, "public" .= repoPublic r]

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \o -> Repo <$> o .: "id" <*> o .: "owner" <*> o .: "name" <*> o .: "public"

data Access = ReadAccess | WriteAccess | AdminAccess deriving (Eq, Ord, Show)

data Snapshot = Snapshot
  { snapshotRevision :: Text
  , snapshotFiles :: Map FilePath Text
  }
  deriving (Eq, Show)

data MutationResult = MutationResult
  { resultRevision :: Text
  , resultValue :: Value
  , resultReplayed :: Bool
  }
  deriving (Eq, Show)

instance ToJSON MutationResult where
  toJSON r = object ["revision" .= resultRevision r, "data" .= resultValue r, "replayed" .= resultReplayed r]

data AppError = AppError Status Text deriving (Show)
instance Exception AppError

badRequest, forbidden, notFound, conflict, stale :: Text -> IO a
badRequest = throwIO . AppError status400
forbidden = throwIO . AppError status403
notFound = throwIO . AppError status404
conflict = throwIO . AppError status409
stale = throwIO . AppError status412
