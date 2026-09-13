module Main (main) where

import Chilin.Git
import Chilin.Items
import Chilin.Repository
import Chilin.Store
import Chilin.Types
import Control.Exception (try)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Myque.Item (State (Open), itemState)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

withRepository :: (Env -> Actor -> Repo -> IO a) -> IO a
withRepository action = withSystemTempDirectory "chilin-test" $ \root -> do
  env <- openEnv root
  bootstrapAdmin env "alice" "alice-test-token-at-least-thirty-two-characters"
  let actor = Actor "alice" True
  repo <- createRepo env actor "alice" "project" False
  action env actor repo

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
