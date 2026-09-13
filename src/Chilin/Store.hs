module Chilin.Store
  ( recordPath
  , encodeRecord
  , decodeRecord
  , readRecord
  , putRecord
  , records
  , loadItems
  , itemPath
  , validateSnapshot
  , initialTracker
  , commitMutation
  , nextTimestamp
  , readItemView
  , readItemViews
  , snapshotForRead
  , itemView
  , operationRecordId
  ) where

import Chilin.Git (initBare, readSnapshot, readSnapshotAt, trackerPath, tryGit, writeSnapshot)
import Chilin.Types
import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Myque.Frontmatter qualified as FM
import Myque.Item (WorkItem (..), decodeItem, itemTitle, keyText, kindText, stateText)
import Myque.Store qualified as MQ
import Myque.Timestamp (Timestamp, parseTimestamp, timestampText)
import Myque.Uuid (newUuidV7, uuidText)
import Myque.Validate (findingText, validate)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)

recordPath :: Text -> Text -> FilePath
recordPath category identifier = ".chilin/" <> T.unpack category <> "/" <> T.unpack identifier <> ".md"

itemPath :: Text -> FilePath
itemPath identifier = ".tasks/items/" <> T.unpack identifier <> ".md"

encodeRecord :: Text -> Value -> Text
encodeRecord schema value =
  FM.renderDocument
    ( FM.Document
        (FM.fromFields [("schema", FM.Scalar schema)])
        ("\n" <> TE.decodeUtf8 (BL.toStrict (encode value)) <> "\n")
    )

decodeRecord :: Text -> Text -> Either String Value
decodeRecord schema raw = do
  document <- FM.parseDocument raw
  unless (FM.fields (FM.docFrontmatter document) == [("schema", FM.Scalar schema)]) $ Left "Invalid Chilin record schema"
  value <- eitherDecodeStrict' (TE.encodeUtf8 (FM.docBody document))
  case value of
    Object _ -> pure value
    _ -> Left "Chilin record body must be a JSON object"

readRecord :: Snapshot -> Text -> Text -> IO Value
readRecord snapshot category identifier = do
  raw <- maybe (notFound "Record not found") pure (Map.lookup (recordPath category identifier) (snapshotFiles snapshot))
  either (badRequest . T.pack) pure (decodeRecord ("chilin-" <> category <> "/v1") raw)

putRecord :: Map FilePath Text -> Text -> Text -> Value -> Map FilePath Text
putRecord files category identifier value = Map.insert (recordPath category identifier) (encodeRecord ("chilin-" <> category <> "/v1") value) files

records :: Snapshot -> Text -> IO [(Text, Value)]
records snapshot category = forM matching $ \(path, raw) -> do
  value <- either (conflict . T.pack) pure (decodeRecord ("chilin-" <> category <> "/v1") raw)
  pure (T.pack (takeBaseName path), value)
 where
  directory = ".chilin/" <> T.unpack category
  matching = [(path, raw) | (path, raw) <- Map.toAscList (snapshotFiles snapshot), takeDirectory path == directory, takeExtension path == ".md"]

loadItems :: Snapshot -> IO (Map Text WorkItem)
loadItems snapshot =
  Map.fromList
    <$> forM
      matching
      ( \(path, raw) -> do
          item <- either (badRequest . T.pack) pure (decodeItem raw)
          let identifier = uuidText (itemId item)
          unless (path == itemPath identifier) $ badRequest "Item filename does not match its canonical UUID"
          pure (identifier, item)
      )
 where
  matching = [(path, raw) | (path, raw) <- Map.toAscList (snapshotFiles snapshot), takeDirectory path == ".tasks/items", takeExtension path == ".md"]

validateSnapshot :: Snapshot -> IO ()
validateSnapshot snapshot = do
  let files = snapshotFiles snapshot
      config = MQ.renderConfig MQ.defaultConfig
  unless (Map.lookup ".tasks/config.toml" files == Just config) $ badRequest "Tracker storage configuration is server controlled"
  forM_ (Map.toList files) $ \(path, raw) -> do
    let pieces = T.splitOn "/" (T.pack path)
    case pieces of
      [".tasks", "config.toml"] -> pure ()
      [".tasks", "items", name] | validName name -> pure ()
      [".chilin", category, name] | category `elem` categories && validName name -> do
        _ <- either (badRequest . T.pack) pure (decodeRecord ("chilin-" <> category <> "/v1") raw)
        pure ()
      _ -> badRequest "Unsupported canonical tracker path"
  _ <- loadItems snapshot
  -- Reuse myque's sole index builder through its public filesystem API;
  -- validation must retain duplicate-key and graph findings, not Map-collapse them.
  withSystemTempDirectory "chilin-validate" $ \root -> do
    forM_ [(p, raw) | (p, raw) <- Map.toList files, ".tasks/" `T.isPrefixOf` T.pack p] $ \(path, raw) -> do
      createDirectoryIfMissing True (takeDirectory (root </> path))
      BS.writeFile (root </> path) (TE.encodeUtf8 raw)
    store <- MQ.loadStore (MQ.Layout root MQ.defaultConfig)
    let findings = validate store
    unless (null findings) $ badRequest (T.intercalate "; " (map findingText findings))
 where
  categories = ["items", "pulls", "milestones", "events", "operations", "comments", "reviews", "merges", "checks"]
  validName name = case T.stripSuffix ".md" name of
    Just stem -> not (T.null stem) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.'])) stem
    Nothing -> False

initialTracker :: Env -> Repo -> IO ()
initialTracker env repo = do
  initBare env (trackerPath env repo)
  _ <- writeSnapshot env (trackerPath env repo) Nothing (Map.singleton ".tasks/config.toml" (MQ.renderConfig MQ.defaultConfig)) "Initialize myque collaboration tracker"
  pure ()

operationRecordId :: Actor -> Text -> Text
operationRecordId actor operation = T.pack (show (hash (TE.encodeUtf8 (actorId actor <> "\NUL" <> operation)) :: Digest SHA256))

field :: Text -> Value -> Maybe Value
field key (Object fields) = KM.lookup (Key.fromText key) fields
field _ _ = Nothing

nextTimestamp :: Snapshot -> IO Timestamp
nextTimestamp snapshot = do
  wall <- getCurrentTime
  now <- either (conflict . T.pack) pure (parseTimestamp (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00" wall)))
  events <- records snapshot "events"
  times <- forM events $ \(_, event) -> case field "recorded_at" event of
    Just (String raw) -> either (conflict . T.pack) pure (parseTimestamp raw)
    _ -> conflict "Event is missing its recorded_at timestamp"
  pure (maximum (now : times))

commitMutation :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> (Snapshot -> Timestamp -> IO (Map FilePath Text, Value)) -> IO MutationResult
commitMutation env repo actor expected operation eventType request mutate = do
  unless (not (T.null operation) && T.length operation <= 128 && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.', ':'])) operation) $ badRequest "Idempotency-Key must be a 1-128 character token"
  snapshot <- readSnapshot env (trackerPath env repo)
  let recordId = operationRecordId actor operation
  case Map.lookup (recordPath "operations" recordId) (snapshotFiles snapshot) of
    Just raw -> do
      saved <- either (conflict . T.pack) pure (decodeRecord "chilin-operations/v1" raw)
      unless (field "request" saved == Just request && field "type" saved == Just (String eventType) && field "actor_id" saved == Just (String (actorId actor))) $ conflict "Idempotency-Key was already used for a different operation"
      value <- maybe (conflict "Operation result is missing") pure (field "data" saved)
      pure (MutationResult (snapshotRevision snapshot) value True)
    Nothing -> do
      unless (expected == snapshotRevision snapshot) $ stale "If-Match does not match the current tracker revision"
      now <- nextTimestamp snapshot
      (candidate, value) <- mutate snapshot now
      validateSnapshot (Snapshot expected candidate)
      before <- loadItems snapshot
      after <- loadItems (Snapshot expected candidate)
      forM_ (Map.toList before) $ \(identifier, original) -> case Map.lookup identifier after of
        Nothing -> badRequest "Canonical items cannot be deleted; cancel them instead"
        Just changed -> do
          unless (itemId original == itemId changed && itemCreated original == itemCreated changed && itemKind original == itemKind changed) $ badRequest "Item identity, kind and creation time are immutable"
          when (itemUpdated changed < itemUpdated original) $ badRequest "Item update time cannot move backwards"
      forM_ (Map.toList (snapshotFiles snapshot)) $ \(path, original) ->
        when (any (`T.isPrefixOf` T.pack path) [".chilin/events/", ".chilin/operations/", ".chilin/comments/"]) $
          unless (Map.lookup path candidate == Just original) $
            badRequest "Historical records are append-only"
      eventId <- uuidText <$> newUuidV7
      events <- records snapshot "events"
      sequences <- forM events $ \(_, event) -> case field "sequence" event of
        Just (Number n) -> pure n
        _ -> conflict "Event is missing sequence"
      let sequenceNumber = maximum (0 : sequences) + 1
          event =
            object
              [ "event_id" .= eventId
              , "operation_id" .= operation
              , "sequence" .= sequenceNumber
              , "actor_id" .= actorId actor
              , "recorded_at" .= timestampText now
              , "base_revision" .= expected
              , "type" .= eventType
              , "request" .= request
              , "data" .= value
              ]
          operationValue = object ["actor_id" .= actorId actor, "operation_id" .= operation, "request" .= request, "type" .= eventType, "data" .= value]
          withEvent = putRecord candidate "events" eventId event
          complete = putRecord withEvent "operations" recordId operationValue
      revision <- writeSnapshot env (trackerPath env repo) (Just expected) complete (eventType <> " by " <> actorId actor)
      pure (MutationResult revision value False)

itemView :: WorkItem -> Value
itemView item =
  object
    [ "id" .= uuidText (itemId item)
    , "kind" .= kindText (itemKind item)
    , "key" .= fmap keyText (itemKey item)
    , "title" .= itemTitle item
    , "body" .= itemBody item
    , "state" .= stateText (itemState item)
    , "created" .= timestampText (itemCreated item)
    , "updated" .= fmap timestampText (itemUpdated item)
    , "closed" .= fmap timestampText (itemClosed item)
    , "tags" .= itemTags item
    , "parent" .= fmap uuidText (itemParent item)
    , "depends" .= map uuidText (itemDepends item)
    , "blocks" .= map uuidText (itemBlocks item)
    , "related" .= map uuidText (itemRelated item)
    , "duplicate_of" .= fmap uuidText (itemDuplicateOf item)
    , "supersedes" .= map uuidText (itemSupersedes item)
    ]

readItemViews :: Snapshot -> [Text] -> IO [Value]
readItemViews snapshot identifiers = do
  items <- loadItems snapshot
  mapM (renderItem snapshot items) identifiers

readItemView :: Snapshot -> Text -> IO Value
readItemView snapshot identifier = do
  items <- loadItems snapshot
  renderItem snapshot items identifier

renderItem :: Snapshot -> Map Text WorkItem -> Text -> IO Value
renderItem snapshot items identifier = do
  item <- maybe (notFound "Item not found") pure (Map.lookup identifier items)
  extras <- forM ["items", "pulls", "milestones"] $ \category ->
    case Map.lookup (recordPath category identifier) (snapshotFiles snapshot) of
      Nothing -> pure Nothing
      Just raw -> do
        value <- either (conflict . T.pack) pure (decodeRecord ("chilin-" <> category <> "/v1") raw)
        let displayValue = case value of
              Object fields | category == "pulls" && stateText (itemState item) == "cancelled" -> Object (KM.insert "status" (String "closed") fields)
              _ -> value
        pure (Just (Key.fromText (case category of "items" -> "metadata"; "pulls" -> "pull"; _ -> "milestone"), displayValue))
  case itemView item of
    Object fields -> pure (Object (foldr (uncurry KM.insert) fields (catMaybes extras)))
    _ -> conflict "Invalid item representation"

snapshotForRead :: Env -> Repo -> Maybe Text -> Maybe Text -> IO Snapshot
snapshotForRead env repo atRevision asOf = do
  let path = trackerPath env repo
  current <- readSnapshot env path
  case (atRevision, asOf) of
    (Nothing, Nothing) -> pure current
    (Just revision, Nothing) -> do
      unless (T.length revision == 40 && T.all (`elem` (['0' .. '9'] <> ['a' .. 'f'])) revision) $ badRequest "Invalid tracker revision"
      (exit, _, _) <- tryGit env path ["merge-base", "--is-ancestor", T.unpack revision, T.unpack (snapshotRevision current)] ""
      unless (exit == ExitSuccess) $ notFound "Revision is not in canonical tracker history"
      readSnapshotAt env path revision
    (Nothing, Just timeText) -> do
      cutoff <- either (badRequest . T.pack) pure (parseTimestamp timeText)
      events <- records current "events"
      future <- forM events $ \(_, event) -> do
        timestamp <- case field "recorded_at" event of
          Just (String raw) -> either (conflict . T.pack) pure (parseTimestamp raw)
          _ -> conflict "Event timestamp is missing"
        sequenceNumber <- case field "sequence" event of
          Just (Number n) -> pure n
          _ -> conflict "Event sequence is missing"
        base <- case field "base_revision" event of
          Just (String revision) -> pure revision
          _ -> conflict "Event base revision is missing"
        pure (timestamp > cutoff, sequenceNumber, base)
      case [(sequenceNumber, base) | (True, sequenceNumber, base) <- future] of
        [] -> pure current
        candidates -> readSnapshotAt env path (snd (minimum candidates))
    _ -> badRequest "at_revision and as_of are mutually exclusive"
