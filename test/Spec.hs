module Main (main) where

import Chilin.Git
import Chilin.Items
import Chilin.Repository
import Chilin.Server (application)
import Chilin.Store
import Chilin.Types
import Control.Exception (try)
import Data.Aeson (Value (..), decode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Item (State (Open), itemState)
import Network.HTTP.Types (Header, Method, Status, hAuthorization, hContentType, methodGet, methodPost, status200, status401, status403, statusCode)
import Network.Socket (SockAddr (..), tupleToHostAddress)
import Network.Wai (Request (..), defaultRequest, responseToStream, setRequestBodyChunks)
import Network.Wai.Internal (ResponseReceived (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

withRegistry :: Maybe ForwardAuth -> (Env -> IO a) -> IO a
withRegistry forward action = withSystemTempDirectory "chilin-test" $ \root -> do
  env <- openEnvWith root forward
  bootstrapAdmin env "alice" "alice-test-token-at-least-thirty-two-characters"
  action env

forwardAuth :: Maybe ForwardAuth
forwardAuth = Just (ForwardAuth (CI.mk "x-forwarded-user") "github")

admin :: Actor
admin = Actor "alice" True

-- Exercises the WAI application the way the proxy does, so peer address and
-- header handling are covered rather than the underlying registry calls.
call :: Env -> SockAddr -> [Header] -> [Text] -> IO (Status, Maybe Value)
call env peer headers path = callWith methodGet env peer headers path ""

callWith :: Method -> Env -> SockAddr -> [Header] -> [Text] -> BL.ByteString -> IO (Status, Maybe Value)
callWith method env peer headers path payload = do
  remaining <- newIORef (BL.toStrict payload)
  let wai =
        setRequestBodyChunks (readIORef remaining >>= \chunk -> writeIORef remaining mempty >> pure chunk) $
          defaultRequest
            { requestMethod = method
            , pathInfo = path
            , rawPathInfo = TE.encodeUtf8 ("/" <> T.intercalate "/" path)
            , requestHeaders = headers
            , remoteHost = peer
            }
  captured <- newIORef Nothing
  ResponseReceived <- application env wai $ \r -> ResponseReceived <$ writeIORef captured (Just r)
  response <- readIORef captured >>= maybe (fail "handler produced no response") pure
  let (status, _, withBody) = responseToStream response
  body <- withBody $ \streaming -> do
    reference <- newIORef mempty
    streaming (\chunk -> modifyIORef' reference (<> chunk)) (pure ())
    toLazyByteString <$> readIORef reference
  pure (status, decode body)

bearer :: Text -> Header
bearer token = (hAuthorization, "Bearer " <> TE.encodeUtf8 token)

intent :: Header
intent = ("X-Chilin-Intent", "1")

json :: Header
json = (hContentType, "application/json")

loopback, remotePeer :: SockAddr
loopback = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))
remotePeer = SockAddrInet 0 (tupleToHostAddress (203, 0, 113, 7))

withRepository :: (Env -> Actor -> Repo -> IO a) -> IO a
withRepository action = withRegistry Nothing $ \env -> do
  repo <- createRepo env admin "alice" "project" False
  action env admin repo

request :: Value
request = object ["kind" .= ("issue" :: Text), "title" .= ("Track lifecycle" :: Text)]

identifier :: MutationResult -> IO Text
identifier result = case resultValue result of
  Object fields -> case KM.lookup "item_id" fields of
    Just (String value) -> pure value
    _ -> fail "result has no item identity"
  _ -> fail "result is not an object"

main :: IO ()
main = hspec $ do
  describe "canonical collaboration transactions" $ do
    it "rejects a stale writer without losing the accepted item" $ withRepository $ \env actor repo -> do
      initial <- readSnapshot env (trackerPath env repo)
      accepted <- createItem env repo actor (snapshotRevision initial) "create-one" request
      rejected <- try (createItem env repo actor (snapshotRevision initial) "create-two" request) :: IO (Either AppError MutationResult)
      rejected `shouldSatisfy` either (const True) (const False)
      current <- readSnapshot env (trackerPath env repo)
      items <- loadItems current
      ident <- identifier accepted
      Map.keys items `shouldBe` [ident]

    it "replays a committed request but rejects reusing its key for different content" $ withRepository $ \env actor repo -> do
      initial <- readSnapshot env (trackerPath env repo)
      first <- createItem env repo actor (snapshotRevision initial) "create" request
      replay <- createItem env repo actor (snapshotRevision initial) "create" request
      resultValue replay `shouldBe` resultValue first
      resultReplayed replay `shouldBe` True
      changed <- try (createItem env repo actor (resultRevision first) "create" (object ["kind" .= ("issue" :: Text), "title" .= ("Different" :: Text)])) :: IO (Either AppError MutationResult)
      changed `shouldSatisfy` either (const True) (const False)

    it "preserves both close and reopen events while clearing the current closed timestamp" $ withRepository $ \env actor repo -> do
      initial <- readSnapshot env (trackerPath env repo)
      created <- createItem env repo actor (snapshotRevision initial) "create" request
      ident <- identifier created
      closed <- updateItem env repo actor (resultRevision created) "close" ident (object ["state" .= ("done" :: Text)])
      _ <- updateItem env repo actor (resultRevision closed) "reopen" ident (object ["state" .= ("open" :: Text)])
      current <- readSnapshot env (trackerPath env repo)
      view <- readItemView current ident
      case view of
        Object fields -> KM.lookup "closed" fields `shouldBe` Just Null
        _ -> expectationFailure "item view is not an object"
      events <- itemTimeline current ident
      let types = [kind | Object event <- events, Just (String kind) <- [KM.lookup "type" event]]
      types `shouldBe` ["item.created", "item.updated", "item.updated"]
      items <- loadItems current
      fmap itemState (Map.lookup ident items) `shouldBe` Just Open

    it "rejects a dependency cycle without publishing a partial snapshot" $ withRepository $ \env actor repo -> do
      initial <- readSnapshot env (trackerPath env repo)
      first <- createItem env repo actor (snapshotRevision initial) "first" request
      second <- createItem env repo actor (resultRevision first) "second" request
      a <- identifier first
      b <- identifier second
      linked <- updateItem env repo actor (resultRevision second) "link" a (object ["depends" .= [b]])
      rejected <- try (updateItem env repo actor (resultRevision linked) "cycle" b (object ["depends" .= [a]])) :: IO (Either AppError MutationResult)
      rejected `shouldSatisfy` either (const True) (const False)
      current <- readSnapshot env (trackerPath env repo)
      snapshotRevision current `shouldBe` resultRevision linked

  describe "proxy-asserted identity" $ do
    it "ignores the identity header from a non-loopback peer" $ withRegistry forwardAuth $ \env -> do
      _ <- linkIdentity env admin "github" "octocat" "alice"
      (status, _) <- call env remotePeer [("X-Forwarded-User", "octocat")] ["api", "me"]
      status `shouldBe` status401

    it "accepts a linked subject from loopback" $ withRegistry forwardAuth $ \env -> do
      _ <- linkIdentity env admin "github" "octocat" "alice"
      (status, body) <- call env loopback [("X-Forwarded-User", "octocat")] ["api", "me"]
      status `shouldBe` status200
      case body of
        Just (Object fields) -> KM.lookup "user" fields `shouldBe` Just (toJSON (Actor "alice" True))
        _ -> expectationFailure "expected a user document"

    it "refuses a subject that is not on the allowlist" $ withRegistry forwardAuth $ \env -> do
      (status, _) <- call env loopback [("X-Forwarded-User", "stranger")] ["api", "me"]
      status `shouldBe` status403

    it "refuses a duplicated identity header the proxy failed to strip" $ withRegistry forwardAuth $ \env -> do
      _ <- linkIdentity env admin "github" "octocat" "alice"
      (status, _) <- call env loopback [("X-Forwarded-User", "octocat"), ("X-Forwarded-User", "mallory")] ["api", "me"]
      status `shouldBe` status403

    it "ignores the identity header entirely when forward auth is disabled" $ withRegistry Nothing $ \env -> do
      _ <- linkIdentity env admin "github" "octocat" "alice"
      (status, _) <- call env loopback [("X-Forwarded-User", "octocat")] ["api", "me"]
      status `shouldBe` status401

  describe "credentials" $ do
    it "authenticates with a generated token and stops after revocation" $ withRegistry Nothing $ \env -> do
      (secret, info) <- createToken env admin "laptop"
      lookupActor env secret `shouldReturn` Just admin
      revokeToken env admin (tokenInfoId info)
      lookupActor env secret `shouldReturn` Nothing

    it "refuses to revoke the last remaining token" $ withRegistry Nothing $ \env -> do
      tokens <- listTokens env admin
      case tokens of
        [only] -> do
          outcome <- try (revokeToken env admin (tokenInfoId only)) :: IO (Either AppError ())
          outcome `shouldSatisfy` either (const True) (const False)
          lookupActor env "alice-test-token-at-least-thirty-two-characters" `shouldReturn` Just admin
        _ -> expectationFailure "bootstrap should leave exactly one token"

    it "issues a working credential to a created user without the caller choosing it" $ withRegistry Nothing $ \env -> do
      (created, secret, _) <- createUser env admin "bob"
      created `shouldBe` Actor "bob" False
      lookupActor env secret `shouldReturn` Just (Actor "bob" False)

    it "rejects a credential mutation that omits the intent header" $ withRegistry Nothing $ \env -> do
      (secret, _) <- createToken env admin "laptop"
      (status, _) <- callWith methodPost env loopback [bearer secret, json] ["api", "tokens"] "{\"label\":\"ci\"}"
      status `shouldBe` status403
      (accepted, body) <- callWith methodPost env loopback [bearer secret, json, intent] ["api", "tokens"] "{\"label\":\"ci\"}"
      statusCode accepted `shouldBe` 201
      -- The secret is transmitted exactly once, at creation.
      case body of
        Just (Object fields) -> KM.member "secret" fields `shouldBe` True
        _ -> expectationFailure "expected a credential document"
