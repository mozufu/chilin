module Chilin.Pulls (createPull, updatePull, reviewPull, mergePull, pullDiff, recoverRepo, recoverRepoLocked) where

import Chilin.Git
import Chilin.Repository (lookupRepo, requireAccess)
import Chilin.Store
import Chilin.Types
import Control.Monad (forM_, unless, void, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Myque.Item
import Myque.Timestamp (timestampText)
import Myque.Uuid (newUuidV7, uuidText)
import System.Exit (ExitCode (..))

-- All durable state is versioned through Store, in tracker.git, never SQLite.
data Pull = Pull
  { p_item_id :: Text
  , p_author :: Text
  , p_status :: Text
  , p_draft :: Bool
  , p_source_owner :: Text
  , p_source_name :: Text
  , p_source_id :: Text
  , p_source_ref :: Text
  , p_target_ref :: Text
  , p_head_oid :: Text
  , p_target_oid :: Text
  , p_reviews :: [Review]
  , p_merge_oid :: Maybe Text
  }
  deriving (Show, Generic)
instance ToJSON Pull where toJSON = genericToJSON recordOptions
instance FromJSON Pull where parseJSON = genericParseJSON recordOptions

data Review = Review
  { r_actor :: Text
  , r_head_oid :: Text
  , r_verdict :: Text
  , r_body :: Text
  , r_created_at :: Text
  , r_operation_id :: Text
  }
  deriving (Show, Generic)
instance ToJSON Review where toJSON = genericToJSON recordOptions
instance FromJSON Review where parseJSON = genericParseJSON recordOptions

data Intent = Intent
  { m_actor :: Text
  , m_operation_id :: Text
  , m_request :: Value
  , m_item_id :: Text
  , m_target_ref :: Text
  , m_target_oid :: Text
  , m_head_oid :: Text
  , m_result_oid :: Text
  , m_status :: Text
  , m_result :: Value
  }
  deriving (Show, Generic)
instance ToJSON Intent where toJSON = genericToJSON recordOptions
instance FromJSON Intent where parseJSON = genericParseJSON recordOptions

recordOptions :: Options
recordOptions = defaultOptions {fieldLabelModifier = drop 2, rejectUnknownFields = True}

fields :: [Text] -> Value -> IO Object
fields allowed (Object o) = do
  unless (all ((`elem` allowed) . K.toText) (KM.keys o)) $ badRequest "Unknown request field"
  pure o
fields _ _ = badRequest "Expected a JSON object"

required :: FromJSON a => Object -> Text -> IO a
required o k = either (badRequest . T.pack) pure (parseEither (.: K.fromText k) o)

optional :: FromJSON a => Object -> Text -> a -> IO a
optional o k def = either (badRequest . T.pack) pure (parseEither (\v -> v .:? K.fromText k .!= def) o)

decodeStored :: FromJSON a => Value -> IO a
decodeStored v = case fromJSON v of
  Error err -> conflict ("Invalid pull lifecycle record: " <> T.pack err)
  Success x -> pure x

operationKey :: Actor -> Text -> Text
operationKey actor op = T.pack (show (hash (TE.encodeUtf8 (actorId actor <> "\0" <> op)) :: Digest SHA256))

fingerprint :: Text -> Text -> Value -> Value
fingerprint action ident request = object ["action" .= action, "item_id" .= ident, "request" .= request]

sourceRepo :: Env -> Pull -> IO Repo
sourceRepo env p = do
  source <- lookupRepo env (p_source_owner p) (p_source_name p)
  unless (repoId source == p_source_id p) $ conflict "Pull source repository identity changed"
  pure source

branchOid :: Env -> Repo -> Text -> IO Text
branchOid env repo ref = do
  validateRef env ref
  resolveRef env (codePath env repo) ref >>= maybe (notFound "Branch does not exist") pure

-- Ordered locks keep fork source verification atomic with target CAS.
withPair :: Env -> Repo -> Repo -> IO a -> IO a
withPair env target source = withRepoLocks env [target, source]

-- Retain immutable objects before publishing metadata that references them.
-- These refs are GC roots, not a mutable projection of the pull's current head.
-- A rejected metadata commit can leave harmless retained objects; publishing
-- metadata first could instead leave a committed pull whose objects were lost.
pinHead :: Env -> Repo -> Repo -> Text -> Text -> IO ()
pinHead env target source ident headOid = do
  when (repoId target /= repoId source) $
    void $
      runGit
        env
        (codePath env target)
        ["-c", "protocol.file.allow=always", "fetch", "--no-tags", "--no-write-fetch-head", "--no-recurse-submodules", "--", codePath env source, T.unpack headOid]
        BS.empty
  void $
    runGit
      env
      (codePath env target)
      ["update-ref", "refs/chilin/pulls/" <> T.unpack ident <> "/objects/" <> T.unpack headOid, T.unpack headOid]
      BS.empty

readPull :: Snapshot -> Text -> IO Pull
readPull snapshot ident = do
  p <- readRecord snapshot "pulls" ident >>= decodeStored
  unless (p_item_id p == ident) $ conflict "Pull identity mismatch"
  pure p

openPull :: Snapshot -> Pull -> IO WorkItem
openPull snapshot p = do
  unless (p_status p == "open") $ conflict "Pull is not open"
  items <- loadItems snapshot
  item <- maybe (notFound "Pull task not found") pure (Map.lookup (p_item_id p) items)
  unless (itemKind item == Task && not (isTerminal (itemState item))) $ conflict "Pull task is not open"
  pure item

createPull :: Env -> Repo -> Actor -> Text -> Text -> Value -> IO MutationResult
createPull env repo actor expected op request = do
  requireAccess env (Just actor) repo WriteAccess
  o <- fields ["title", "body", "source_repo", "source_ref", "target_ref", "draft"] request
  title <- required o "title"
  when (T.null (T.strip title) || T.any (`elem` ['\n', '\r']) title) $ badRequest "Title must be a nonempty single line"
  body <- optional o "body" ""
  sourceRef <- required o "source_ref"
  targetRef <- required o "target_ref"
  draft <- optional o "draft" False
  source <- case KM.lookup "source_repo" o of
    Nothing -> pure repo
    Just v -> do
      so <- fields ["owner", "name"] v
      owner <- required so "owner"
      name <- required so "name"
      lookupRepo env owner name
  requireAccess env (Just actor) source ReadAccess
  when (repoPublic repo && not (repoPublic source)) $ forbidden "Private source objects cannot be imported into a public repository"
  withPair env repo source $ do
    requireAccess env (Just actor) repo WriteAccess
    requireAccess env (Just actor) source ReadAccess
    recoverRepoLocked env repo
    when (repoId repo /= repoId source) $ recoverRepoLocked env source
    commitMutation env repo actor expected op "pull.created" (fingerprint "pull.create" "" request) $ \snapshot now -> do
      headOid <- branchOid env source sourceRef
      targetOid <- branchOid env repo targetRef
      when (repoId repo == repoId source && sourceRef == targetRef) $ badRequest "Source and target branches must differ"
      uuid <- newUuidV7
      let ident = uuidText uuid
          item = (newWorkItem uuid Task now title) {itemBody = "\n# " <> T.strip title <> "\n\n" <> body}
          pull =
            Pull
              ident
              (actorId actor)
              "open"
              draft
              (repoOwner source)
              (repoName source)
              (repoId source)
              sourceRef
              targetRef
              headOid
              targetOid
              []
              Nothing
          extension = object ["item_id" .= ident, "author" .= actorId actor, "assignees" .= ([] :: [Text]), "milestone" .= Null]
          files = putRecord (putRecord (Map.insert (itemPath ident) (encodeItem item) (snapshotFiles snapshot)) "items" ident extension) "pulls" ident (toJSON pull)
      pinHead env repo source ident headOid
      void $
        runGit
          env
          (codePath env repo)
          ["update-ref", "refs/chilin/pulls/" <> T.unpack ident <> "/objects/" <> T.unpack targetOid, T.unpack targetOid]
          BS.empty
      pure (files, object ["item_id" .= ident, "item" .= itemView item, "pull" .= pull])

updatePull :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> IO MutationResult
updatePull env repo actor expected op ident request = do
  requireAccess env (Just actor) repo WriteAccess
  o <- fields ["draft"] request
  draft <- required o "draft"
  withRepoLock env repo $ do
    requireAccess env (Just actor) repo WriteAccess
    recoverRepoLocked env repo
    commitMutation env repo actor expected op "pull.updated" (fingerprint "pull.update" ident request) $ \snapshot now -> do
      pull <- readPull snapshot ident
      item <- openPull snapshot pull
      let updated = pull {p_draft = draft}
          updatedItem = item {itemUpdated = Just now}
          files = putRecord (Map.insert (itemPath ident) (encodeItem updatedItem) (snapshotFiles snapshot)) "pulls" ident (toJSON updated)
      pure (files, object ["item_id" .= ident, "item" .= itemView updatedItem, "pull" .= updated])

-- Read authorization belongs to the caller. Both operands are recorded OIDs;
-- branch movement cannot silently alter the diff a review refers to.
pullDiff :: Env -> Repo -> Text -> IO Value
pullDiff env repo ident = do
  snapshot <- readSnapshot env (trackerPath env repo)
  pull <- readPull snapshot ident
  unless (validOid (p_target_oid pull) && validOid (p_head_oid pull)) $ conflict "Invalid pinned pull OID"
  patch <-
    runGit
      env
      (codePath env repo)
      ["diff", "--no-ext-diff", "--no-textconv", "--binary", T.unpack (p_target_oid pull), T.unpack (p_head_oid pull), "--"]
      BS.empty
  pure $
    object
      [ "item_id" .= ident
      , "base_oid" .= p_target_oid pull
      , "head_oid" .= p_head_oid pull
      , "diff" .= TE.decodeUtf8With (\_ _ -> Just '\xfffd') patch
      ]

reviewPull :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> IO MutationResult
reviewPull env repo actor expected op ident request = do
  requireAccess env (Just actor) repo WriteAccess
  o <- fields ["head_oid", "verdict", "body"] request
  requestedHead <- required o "head_oid"
  verdict <- required o "verdict"
  unless (verdict `elem` ["approve", "request_changes", "comment"]) $ badRequest "Invalid review verdict"
  body <- optional o "body" ""
  initial <- readSnapshot env (trackerPath env repo) >>= (`readPull` ident)
  source <- sourceRepo env initial
  requireAccess env (Just actor) source ReadAccess
  withPair env repo source $ do
    requireAccess env (Just actor) repo WriteAccess
    requireAccess env (Just actor) source ReadAccess
    when (repoPublic repo && not (repoPublic source)) $ forbidden "Private source objects cannot be imported into a public repository"
    recoverRepoLocked env repo
    when (repoId repo /= repoId source) $ recoverRepoLocked env source
    commitMutation env repo actor expected op "pull.reviewed" (fingerprint "pull.review" ident request) $ \snapshot now -> do
      pull <- readPull snapshot ident
      item <- openPull snapshot pull
      current <- branchOid env source (p_source_ref pull)
      unless (current == requestedHead) $ stale "Source head has changed"
      pinHead env repo source ident current
      let review = Review (actorId actor) current verdict body (timestampText now) op
          updated = pull {p_head_oid = current, p_reviews = p_reviews pull <> [review]}
          updatedItem = item {itemUpdated = Just now}
          files = putRecord (Map.insert (itemPath ident) (encodeItem updatedItem) (snapshotFiles snapshot)) "pulls" ident (toJSON updated)
      pure (files, object ["item_id" .= ident, "review" .= review, "pull" .= updated])

mergePull :: Env -> Repo -> Actor -> Text -> Text -> Text -> Value -> IO MutationResult
mergePull env repo actor expected op ident request = do
  requireAccess env (Just actor) repo WriteAccess
  o <- fields ["head_oid", "target_oid"] request
  requestedHead <- required o "head_oid"
  requestedTarget <- required o "target_oid"
  initial <- readSnapshot env (trackerPath env repo) >>= (`readPull` ident)
  source <- sourceRepo env initial
  requireAccess env (Just actor) source ReadAccess
  withPair env repo source $ do
    requireAccess env (Just actor) repo WriteAccess
    requireAccess env (Just actor) source ReadAccess
    recoverRepoLocked env repo
    when (repoId repo /= repoId source) $ recoverRepoLocked env source
    snapshot <- readSnapshot env (trackerPath env repo)
    let key = operationKey actor op
        fp = fingerprint "pull.merge" ident request
    case Map.lookup (recordPath "merges" key) (snapshotFiles snapshot) of
      Just _ -> do
        intent <- readRecord snapshot "merges" key >>= decodeStored
        unless (m_actor intent == actorId actor && m_operation_id intent == op && m_request intent == fp) $ conflict "Idempotency key reused with different request"
        case m_status intent of
          "completed" -> pure (MutationResult (snapshotRevision snapshot) (m_result intent) True)
          "aborted" -> conflict "Merge was aborted before target update; use a new operation key and current tracker revision"
          _ -> conflict "Merge is not completed"
      Nothing -> do
        prepared <- commitMutation env repo actor expected op "pull.merge.prepared" fp $ \current _ -> do
          pull <- readPull current ident
          void $ openPull current pull
          when (p_draft pull) $ conflict "Draft pull cannot be merged"
          headOid <- branchOid env source (p_source_ref pull)
          targetOid <- branchOid env repo (p_target_ref pull)
          unless (requestedHead == headOid && p_head_oid pull == headOid) $ stale "Pull head is stale; submit a review at the current source head"
          unless (requestedTarget == targetOid) $ stale "Target branch has changed"
          when (headOid == targetOid) $ conflict "Source and target already name the same commit"
          let decisions = Map.fromList [(r_actor r, r_verdict r) | r <- p_reviews pull, r_head_oid r == headOid, r_verdict r /= "comment"]
          when ("request_changes" `elem` Map.elems decisions) $ conflict "Outstanding changes request at this head"
          unless (any (\(reviewer, decision) -> reviewer /= p_author pull && decision == "approve") (Map.toList decisions)) $
            conflict "A non-author approval at the exact head is required"
          (exit, output, _) <-
            tryGit
              env
              (codePath env repo)
              ["merge-tree", "--write-tree", "--messages", T.unpack targetOid, T.unpack headOid]
              BS.empty
          unless (exit == ExitSuccess) $ conflict "Pull cannot be merged cleanly"
          tree <- case T.lines (TE.decodeUtf8 output) of
            first : _ | validOid first -> pure first
            _ -> conflict "Git returned an invalid merge tree"
          resultOid <-
            T.strip . TE.decodeUtf8
              <$> runGit
                env
                (codePath env repo)
                ["commit-tree", T.unpack tree, "-p", T.unpack targetOid, "-p", T.unpack headOid]
                (TE.encodeUtf8 ("Merge pull " <> ident <> "\n"))
          unless (validOid resultOid) $ conflict "Git returned an invalid merge commit"
          void $ runGit env (codePath env repo) ["update-ref", "refs/chilin/merges/" <> T.unpack key, T.unpack resultOid] BS.empty
          let result = object ["item_id" .= ident, "status" .= ("completed" :: Text), "merge_oid" .= resultOid, "head_oid" .= headOid, "target_oid" .= targetOid]
              intent = Intent (actorId actor) op fp ident (p_target_ref pull) targetOid headOid resultOid "prepared" result
          pure (putRecord (snapshotFiles current) "merges" key (toJSON intent), object ["status" .= ("prepared" :: Text), "item_id" .= ident])
        when (resultReplayed prepared) $ conflict "Operation key belongs to a different lifecycle operation"
        stored <- readSnapshot env (trackerPath env repo)
        intent <- readRecord stored "merges" key >>= decodeStored
        -- The source lock and target lock remain held across preparation and CAS.
        void $
          runGit
            env
            (codePath env repo)
            ["update-ref", T.unpack (m_target_ref intent), T.unpack (m_result_oid intent), T.unpack (m_target_oid intent)]
            BS.empty
        finishIntent env repo key intent

validOid :: Text -> Bool
validOid oid = T.length oid `elem` [40, 64] && T.all (`elem` (['0' .. '9'] <> ['a' .. 'f'])) oid

-- No target ref is ever changed by recovery. An unchanged target aborts, rather
-- than unexpectedly merging a previously interrupted request during startup.
recoverRepo :: Env -> Repo -> IO ()
recoverRepo env repo = withRepoLock env repo (recoverRepoLocked env repo)

recoverRepoLocked :: Env -> Repo -> IO ()
recoverRepoLocked env repo = do
  snapshot <- readSnapshot env (trackerPath env repo)
  intents <- records snapshot "merges"
  forM_ intents $ \(key, value) -> do
    intent <- decodeStored value
    case m_status intent of
      "completed" -> pure ()
      "aborted" -> pure ()
      "prepared" -> do
        validateRef env (m_target_ref intent)
        current <- resolveRef env (codePath env repo) (m_target_ref intent)
        if current == Just (m_result_oid intent)
          then void $ finishIntent env repo key intent
          else
            if current == Just (m_target_oid intent)
              then abortIntent env repo key intent
              else conflict "Ambiguous prepared merge: target is neither old nor prepared result; repository writes are blocked"
      _ -> conflict "Invalid merge intent state; repository writes are blocked"

systemActor :: Actor
systemActor = Actor "chilin:merge-recovery" True

finishIntent :: Env -> Repo -> Text -> Intent -> IO MutationResult
finishIntent env repo key intent = do
  snapshot <- readSnapshot env (trackerPath env repo)
  commitMutation
    env
    repo
    systemActor
    (snapshotRevision snapshot)
    ("merge-finalize-" <> key)
    "pull.completed"
    (toJSON intent)
    $ \current now -> do
      stored <- readRecord current "merges" key >>= decodeStored
      unless (m_status stored == "prepared" && m_result_oid stored == m_result_oid intent) $ conflict "Merge intent changed"
      pull <- readPull current (m_item_id intent)
      item <- openPull current pull
      unless (p_head_oid pull == m_head_oid intent && p_target_ref pull == m_target_ref intent) $ conflict "Pull changed during prepared merge"
      let completedItem = item {itemState = Done, itemUpdated = Just now, itemClosed = Just now}
          completedPull = pull {p_status = "merged", p_merge_oid = Just (m_result_oid intent), p_target_oid = m_target_oid intent}
          files =
            putRecord
              ( putRecord
                  (Map.insert (itemPath (m_item_id intent)) (encodeItem completedItem) (snapshotFiles current))
                  "pulls"
                  (m_item_id intent)
                  (toJSON completedPull)
              )
              "merges"
              key
              (toJSON intent {m_status = "completed"})
      pure (files, m_result intent)

abortIntent :: Env -> Repo -> Text -> Intent -> IO ()
abortIntent env repo key intent = do
  snapshot <- readSnapshot env (trackerPath env repo)
  void
    $ commitMutation
      env
      repo
      systemActor
      (snapshotRevision snapshot)
      ("merge-abort-" <> key)
      "pull.merge.aborted"
      (toJSON intent)
    $ \current _ ->
      pure
        ( putRecord (snapshotFiles current) "merges" key (toJSON intent {m_status = "aborted"})
        , object ["item_id" .= m_item_id intent, "status" .= ("aborted" :: Text), "merge_oid" .= m_result_oid intent]
        )
