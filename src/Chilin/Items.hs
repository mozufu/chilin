module Chilin.Items
  ( createItem
  , updateItem
  , commentItem
  , itemTimeline
  , importItems
  ) where

import Chilin.Git (withRepoLock)
import Chilin.Pulls (recoverRepoLocked)
import Chilin.Repository (actorExists, requireAccess)
import Chilin.Store
import Chilin.Types
import Control.Monad (unless, when, (>=>))
import Data.Aeson (Object, Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither, (.:))
import Data.Foldable (toList)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Myque.Item
import Myque.Timestamp
import Myque.Uuid

-- Request schemas are closed. In particular identity, authorship, timestamps,
-- and pull metadata never travel through the ordinary JSON write surface.
fields :: [Text] -> Value -> IO Object
fields allowed (Object o) = do
  unless (all ((`elem` allowed) . Key.toText) (KM.keys o)) $
    badRequest "unknown or read-only request field"
  pure o
fields _ _ = badRequest "request must be an object"

fieldValue :: Text -> Object -> Maybe Value
fieldValue = KM.lookup . Key.fromText

textValue :: Value -> IO Text
textValue (String t) = pure t
textValue _ = badRequest "expected a string"

requiredText :: Text -> Object -> IO Text
requiredText name o = maybe (badRequest ("missing field: " <> name)) textValue (fieldValue name o)

optionalText :: Text -> Text -> Object -> IO Text
optionalText name fallback o = maybe (pure fallback) textValue (fieldValue name o)

parseValue :: Either String a -> IO a
parseValue = either (badRequest . T.pack) pure

arrayValue :: Value -> IO [Value]
arrayValue (Array xs) = pure (toList xs)
arrayValue _ = badRequest "expected an array"

textArray :: Value -> IO [Text]
textArray value = arrayValue value >>= traverse textValue

canonicalUuid :: Text -> IO Uuid
canonicalUuid t = do
  u <- parseValue (parseUuid t)
  unless (isUuidV7 u && uuidText u == t) $ badRequest "expected a canonical lowercase UUIDv7"
  pure u

nullable :: (Value -> IO a) -> Value -> IO (Maybe a)
nullable _ Null = pure Nothing
nullable f value = Just <$> f value

reference :: Value -> IO Uuid
reference value = textValue value >>= canonicalUuid

validTitle :: Text -> IO Text
validTitle title = do
  when (T.null (T.strip title) || T.any (`elem` ['\r', '\n']) title) $
    badRequest "title must be a nonempty single line"
  pure (T.strip title)

validateTags :: Value -> IO [Text]
validateTags value = textArray value >>= traverse (parseValue . parseTag) . nub

loadItem :: Snapshot -> Text -> IO WorkItem
loadItem snapshot ident = do
  _ <- canonicalUuid ident
  items <- loadItems snapshot
  maybe (notFound "item not found") pure (Map.lookup ident items)

extension :: Actor -> Text -> Value
extension actor ident =
  object
    [ "item_id" .= ident
    , "author" .= actorId actor
    , "assignees" .= ([] :: [Text])
    , "milestone" .= Null
    ]

-- Extensions are separately versioned documents; milestone membership is not
-- myque's parent edge, and due dates are not free-form Markdown metadata.
updateExtension :: Env -> Snapshot -> WorkItem -> Object -> Value -> IO Value
updateExtension env snapshot item request value = do
  o <- case value of
    Object existing -> pure existing
    _ -> conflict "invalid item extension"
  withMilestone <- case fieldValue "milestone" request of
    Nothing -> pure o
    Just v -> do
      target <- nullable reference v
      case target of
        Nothing -> pure ()
        Just uuid -> do
          when (uuid == itemId item) $ badRequest "item cannot belong to itself"
          milestone <- loadItem snapshot (uuidText uuid)
          unless (itemKind milestone == Milestone) $ badRequest "milestone must reference a milestone item"
      pure (KM.insert "milestone" (maybe Null (String . uuidText) target) o)
  withAssignees <- case fieldValue "assignees" request of
    Nothing -> pure withMilestone
    Just v -> do
      identities <- nub <$> textArray v
      mapM_ (actorExists env >=> \exists -> unless exists (badRequest "assignee is not a registered actor")) identities
      pure (KM.insert "assignees" (toJSON identities) withMilestone)
  pure (Object withAssignees)

writeMilestone :: WorkItem -> Object -> Map.Map FilePath Text -> IO (Map.Map FilePath Text)
writeMilestone item request files = case fieldValue "due_at" request of
  Nothing
    | itemKind item == Milestone && not (Map.member path files) -> pure (save Null)
    | otherwise -> pure files
  Just value -> do
    unless (itemKind item == Milestone) $ badRequest "due_at is only valid on milestones"
    due <- nullable (textValue >=> parseValue . parseTimestamp) value
    pure (save (maybe Null (String . timestampText) due))
 where
  ident = uuidText (itemId item)
  path = recordPath "milestones" ident
  save due = putRecord files "milestones" ident (object ["item_id" .= ident, "due_at" .= due])

writeItem :: WorkItem -> Map.Map FilePath Text -> Map.Map FilePath Text
writeItem item = Map.insert (itemPath (uuidText (itemId item))) (encodeItem item)

mutation
  :: Env
  -> Repo
  -> Actor
  -> Text
  -> Text
  -> Text
  -> Value
  -> (Snapshot -> Timestamp -> IO (Map.Map FilePath Text, Value))
  -> IO MutationResult
mutation env repo actor expected operation event fingerprint action = do
  requireAccess env (Just actor) repo WriteAccess
  withRepoLock env repo $ do
    recoverRepoLocked env repo
    requireAccess env (Just actor) repo WriteAccess
    commitMutation env repo actor expected operation event fingerprint action

createItem :: Env -> Repo -> Actor -> Text -> Text -> Value -> IO MutationResult
createItem env repo actor expected operation request = do
  o <- fields ["kind", "title", "body", "tags", "parent", "milestone", "due_at"] request
  kind <- requiredText "kind" o >>= parseValue . parseKind
  title <- requiredText "title" o >>= validTitle
  body <- optionalText "body" "" o
  tags <- maybe (pure []) validateTags (fieldValue "tags" o)
  parent <- maybe (pure Nothing) (nullable reference) (fieldValue "parent" o)
  mutation env repo actor expected operation "item.created" request $ \snapshot now -> do
    uuid <- newUuidV7
    let ident = uuidText uuid
        item =
          setTitle
            title
            ( (newWorkItem uuid kind now title)
                { itemBody = body
                , itemTags = tags
                , itemParent = parent
                }
            )
    ext <- updateExtension env snapshot item o (extension actor ident)
    files <- writeMilestone item o (putRecord (writeItem item (snapshotFiles snapshot)) "items" ident ext)
    view <- readItemView (snapshot {snapshotFiles = files}) ident
    pure (files, object ["item_id" .= ident, "item" .= view])

-- A completed pull can only be produced by the merge transaction. A merged
-- pull's canonical task remains frozen, even when importing offline edits.
guardPull :: Snapshot -> Text -> Maybe State -> IO ()
guardPull snapshot ident state = when (Map.member (recordPath "pulls" ident) (snapshotFiles snapshot)) $ do
  pull <- readRecord snapshot "pulls" ident
  status <- case pull of
    Object o -> requiredText "status" o
    _ -> conflict "invalid pull metadata"
  when (status == "merged") $ conflict "merged pull requests cannot be edited"
  when (status /= "open") $ conflict "invalid pull lifecycle"
  when (state == Just Done) $ conflict "pull requests may only be completed by merging"

applyState :: Timestamp -> WorkItem -> State -> WorkItem
applyState now item state =
  item
    { itemState = state
    , itemClosed =
        if isTerminal state
          then if itemState item == state then itemClosed item else Just now
          else Nothing
    }

updateItem :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> IO MutationResult
updateItem env repo actor expected operation ident request = do
  o <- fields ["body", "title", "state", "tags", "parent", "depends", "milestone", "due_at", "assignees"] request
  when (KM.null o) $ badRequest "empty update"
  mutation env repo actor expected operation "item.updated" (object ["item_id" .= ident, "request" .= request]) $ \snapshot now -> do
    old <- loadItem snapshot ident
    state <- maybe (pure (itemState old)) (textValue >=> parseValue . parseState) (fieldValue "state" o)
    guardPull snapshot ident (Just state)
    body <- optionalText "body" (itemBody old) o
    title <- traverse (textValue >=> validTitle) (fieldValue "title" o)
    tags <- maybe (pure (itemTags old)) validateTags (fieldValue "tags" o)
    parent <- maybe (pure (itemParent old)) (nullable reference) (fieldValue "parent" o)
    depends <- maybe (pure (itemDepends old)) (arrayValue >=> fmap nub . traverse reference) (fieldValue "depends" o)
    let changed =
          (applyState now old state)
            { itemBody = body
            , itemTags = tags
            , itemParent = parent
            , itemDepends = depends
            , itemUpdated = Just now
            }
        item = maybe changed (`setTitle` changed) title
    priorExtension <- readRecord snapshot "items" ident
    ext <- updateExtension env snapshot item o priorExtension
    files <- writeMilestone item o (putRecord (writeItem item (snapshotFiles snapshot)) "items" ident ext)
    view <- readItemView (snapshot {snapshotFiles = files}) ident
    pure (files, object ["item_id" .= ident, "previous_state" .= stateText (itemState old), "state" .= stateText state, "item" .= view])

commentItem :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> IO MutationResult
commentItem env repo actor expected operation ident request = do
  o <- fields ["body"] request
  body <- requiredText "body" o
  when (T.null (T.strip body)) $ badRequest "comment body must not be empty"
  mutation env repo actor expected operation "comment.created" (object ["item_id" .= ident, "request" .= request]) $ \snapshot now -> do
    _ <- loadItem snapshot ident
    commentId <- uuidText <$> newUuidV7
    let comment = object ["id" .= commentId, "item_id" .= ident, "author" .= actorId actor, "body" .= body, "created_at" .= timestampText now]
        files = putRecord (snapshotFiles snapshot) "comments" commentId comment
    pure (files, object ["item_id" .= ident, "comment" .= comment])

itemTimeline :: Snapshot -> Text -> IO [Value]
itemTimeline snapshot ident = do
  _ <- loadItem snapshot ident
  events <- records snapshot "events"
  selected <- traverse select events
  pure (map snd (sortOn fst (catMaybes selected)))
 where
  select (_, value) = do
    (sequenceNumber, payload) <- case parseEither (\o -> (,) <$> o .: "sequence" <*> o .: "data") =<< asObject value of
      Left err -> conflict ("invalid event: " <> T.pack err)
      Right result -> pure (result :: (Integer, Value))
    pure (if matches payload then Just (sequenceNumber, value) else Nothing)
  asObject (Object o) = Right o
  asObject _ = Left "event must be an object"
  matches (Object o) =
    fieldValue "item_id" o == Just (String ident)
      || case fieldValue "item_ids" o of
        Just (Array ids) -> String ident `elem` ids
        _ -> False
  matches _ = False

importItems :: Env -> Repo -> Actor -> Text -> Text -> Value -> IO MutationResult
importItems env repo actor expected operation request = do
  o <- fields ["items"] request
  documents <- maybe (badRequest "missing field: items") textArray (fieldValue "items" o)
  when (null documents) $ badRequest "import must contain at least one item"
  imported <- traverse (parseValue . decodeItem) documents
  let ids = map (uuidText . itemId) imported
  unless (length ids == length (nub ids)) $ conflict "import contains duplicate item identities"
  mutation env repo actor expected operation "items.imported" request $ \snapshot now -> do
    existing <- loadItems snapshot
    prepared <- traverse (prepare snapshot existing now) imported
    let itemFiles =
          foldl'
            ( \acc (item, isNew) ->
                let written = writeItem item acc
                 in if isNew then putRecord written "items" (uuidText (itemId item)) (extension actor (uuidText (itemId item))) else written
            )
            (snapshotFiles snapshot)
            prepared
        files =
          foldl'
            ( \acc (item, isNew) ->
                if isNew && itemKind item == Milestone
                  then putRecord acc "milestones" (uuidText (itemId item)) (object ["item_id" .= uuidText (itemId item), "due_at" .= Null])
                  else acc
            )
            itemFiles
            prepared
    -- Validation runs against the entire candidate, not a partially applied
    -- batch; forward references within an offline import are therefore valid.
    validateSnapshot (snapshot {snapshotFiles = files})
    views <- traverse (readItemView (snapshot {snapshotFiles = files})) ids
    let transitions = [object ["item_id" .= uuidText (itemId item), "previous_state" .= fmap (stateText . itemState) (Map.lookup (uuidText (itemId item)) existing), "state" .= stateText (itemState item)] | (item, _) <- prepared]
    pure (files, object ["item_ids" .= ids, "items" .= views, "transitions" .= transitions])
 where
  prepare snapshot existing now incoming = do
    let ident = uuidText (itemId incoming)
    mapM_ (\uuid -> unless (isUuidV7 uuid) (badRequest "relationships must reference canonical UUIDv7 identities")) (outgoingIds incoming)
    when (itemCreated incoming > now || maybe False (> now) (itemUpdated incoming) || maybe False (> now) (itemClosed incoming)) $
      conflict "import timestamps cannot be in the future"
    when (maybe False (< itemCreated incoming) (itemUpdated incoming) || maybe False (< itemCreated incoming) (itemClosed incoming)) $
      conflict "import timestamps precede creation"
    case (itemClosed incoming, itemUpdated incoming) of
      (Just closed, Just updated) | closed > updated -> conflict "closed timestamp follows updated timestamp"
      _ -> pure ()
    guardPull snapshot ident (Just (itemState incoming))
    case Map.lookup ident existing of
      Nothing -> pure (incoming {itemUpdated = Just now}, True)
      Just old -> do
        unless (itemCreated incoming == itemCreated old && itemKind incoming == itemKind old) $
          conflict "import cannot change creation time or kind of an existing identity"
        when (itemState incoming == itemState old && itemClosed incoming /= itemClosed old) $
          conflict "import cannot rewrite terminal history"
        let transitioned = applyState now old (itemState incoming)
        pure (incoming {itemCreated = itemCreated old, itemUpdated = Just now, itemClosed = itemClosed transitioned}, False)
