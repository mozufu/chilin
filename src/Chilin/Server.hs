module Chilin.Server (application, serve) where

import Chilin.Git qualified as Git
import Chilin.Items qualified as Items
import Chilin.Pulls qualified as Pulls
import Chilin.Repository qualified as Repository
import Chilin.Store qualified as Store
import Chilin.Transport qualified as Transport
import Chilin.Types
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import Control.Monad (forM, unless, when)
import Data.Aeson (Object, Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.Char (isAlphaNum, isAscii, isAsciiUpper, isDigit, isHexDigit)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Myque.Item qualified as Myque
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow, hETag, hIfMatch, hWWWAuthenticate)
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.IO (hPutStrLn, stderr)

serve :: Env -> String -> Int -> IO ()
serve env host port = do
  Repository.allRepos env >>= mapM_ (Pulls.recoverRepo env)
  Warp.runSettings (Warp.setHost (fromString host) $ Warp.setPort port Warp.defaultSettings) (application env)

application :: Env -> Application
application env request respond = do
  started <- newIORef False
  let send response = writeIORef started True >> respond response
      onError :: SomeException -> IO ResponseReceived
      onError exception = do
        sent <- readIORef started
        case fromException exception :: Maybe SomeAsyncException of
          Just _ -> throwIO exception
          Nothing | sent -> throwIO exception
          Nothing -> case fromException exception of
            Just (AppError status message) ->
              send $ errorResponse status (if statusCode status >= 500 then "internal server error" else message)
            Nothing -> do
              -- Exception rendering can contain credentials, Git input, or disk paths.
              hPutStrLn stderr "chilin: unhandled HTTP request failure"
              send $ errorResponse status500 "internal server error"
  dispatch env request send `catch` onError

errorResponse :: Status -> Text -> Response
errorResponse status message =
  jsonResponse status headers $
    object
      ["error" .= object ["status" .= statusCode status, "message" .= message]]
 where
  headers = [(hWWWAuthenticate, "Basic realm=\"Chilin\", charset=\"UTF-8\"") | status == status401]

jsonResponse :: Status -> ResponseHeaders -> Value -> Response
jsonResponse status headers = responseLBS status ((hContentType, "application/json; charset=utf-8") : (hCacheControl, "no-store") : headers) . encode

etag :: Text -> ResponseHeaders
etag revision = [(hETag, TE.encodeUtf8 $ "\"" <> revision <> "\"")]

dispatch :: Env -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
dispatch env request send = case pathInfo request of
  ["health"] -> route [methodGet] $ do
    noQuery request
    send $ jsonResponse status200 [] $ object ["status" .= ("ok" :: Text)]
  "api" : rest -> do
    actor <- authenticate env request
    api actor rest
  owner : filename : suffix
    | Just (name, tracker) <- gitName filename -> do
        actor <- authenticate env request
        repo <-
          Repository.lookupRepo env owner name `catch` \(err :: AppError) ->
            if isNothing actor then unauthorized else throwIO err
        when (isNothing actor && not (repoPublic repo)) unauthorized
        Repository.requireAccess env actor repo ReadAccess
        Transport.gitHttp env actor repo tracker suffix request send
  _ -> notFound "route not found"
 where
  route methods action
    | requestMethod request `elem` methods = action
    | otherwise =
        send $
          jsonResponse status405 [(hAllow, B8.intercalate ", " methods)] $
            object ["error" .= object ["status" .= (405 :: Int), "message" .= ("method not allowed" :: Text)]]
  api actor = \case
    ["users"] -> route [methodPost] $ do
      noQuery request
      user <- authenticated actor
      unless (actorAdmin user) $ forbidden "administrator access required"
      body <- jsonBody request standardLimit >>= closedObject ["name", "token"]
      name <- textField "name" body
      token <- textField "token" body
      created <- Repository.createUser env user name token
      send $ jsonResponse status201 [] $ object ["user" .= created]
    ["repos"] -> route [methodGet, methodPost] $ do
      noQuery request
      if requestMethod request == methodGet
        then do
          repos <- Repository.listRepos env actor
          send $ jsonResponse status200 [] $ object ["repositories" .= repos]
        else do
          user <- authenticated actor
          body <- jsonBody request standardLimit >>= closedObject ["owner", "name", "public"]
          owner <- textField "owner" body
          name <- textField "name" body
          public <- case KM.lookup "public" body of
            Nothing -> pure False
            Just (Bool value) -> pure value
            _ -> badRequest "public must be a boolean"
          repo <- Repository.createRepo env user owner name public
          snapshot <- Git.readSnapshot env (Git.trackerPath env repo)
          send $
            jsonResponse status201 (etag $ snapshotRevision snapshot) $
              object
                ["repository" .= repo, "revision" .= snapshotRevision snapshot]
    "repos" : owner : name : rest -> do
      repo <- Repository.lookupRepo env owner name
      Repository.requireAccess env actor repo ReadAccess
      repoApi actor repo rest
    _ -> notFound "route not found"
  repoApi actor repo = \case
    [] -> route [methodGet] $ do
      snapshot <- readSnapshotQuery env repo request
      items <- Store.loadItems snapshot
      views <- Store.readItemViews snapshot (Map.keys items)
      send $
        jsonResponse status200 (etag $ snapshotRevision snapshot) $
          object
            [ "repository" .= repo
            , "revision" .= snapshotRevision snapshot
            , "files" .= snapshotFiles snapshot
            , "items" .= views
            ]
    ["permissions"] -> route [methodPost] $ do
      noQuery request
      user <- authenticated actor
      Repository.requireAccess env actor repo AdminAccess
      body <- jsonBody request standardLimit >>= closedObject ["user", "access"]
      target <- textField "user" body
      access <-
        textField "access" body >>= \case
          "read" -> pure ReadAccess
          "write" -> pure WriteAccess
          "admin" -> pure AdminAccess
          _ -> badRequest "access must be read, write, or admin"
      Pulls.recoverRepo env repo
      Repository.grantAccess env user repo target access
      send $ jsonResponse status200 [] $ object ["user" .= target, "access" .= accessText access]
    ["imports"] ->
      route [methodPost] $
        mutate actor repo importLimit (Items.importItems env repo)
    ["operations", operation] -> route [methodGet] $ do
      noQuery request
      user <- authenticated actor
      validateOperation operation
      snapshot <- Git.readSnapshot env (Git.trackerPath env repo)
      value <- Store.readRecord snapshot "operations" (Store.operationRecordId user operation)
      case value of
        Object record
          | KM.lookup "actor_id" record == Just (String $ actorId user)
          , KM.lookup "operation_id" record == Just (String operation) ->
              if KM.lookup "type" record == Just (String "pull.merge.prepared")
                then do
                  merge <- Store.readRecord snapshot "merges" (Store.operationRecordId user operation)
                  case merge of
                    Object intent
                      | KM.lookup "actor" intent == Just (String $ actorId user)
                      , KM.lookup "operation_id" intent == Just (String operation) ->
                          case KM.lookup "status" intent of
                            Just (String "completed")
                              | Just result <- KM.lookup "result" intent ->
                                  sendSnapshot snapshot $ object ["status" .= ("completed" :: Text), "result" .= result]
                            Just (String status)
                              | status `elem` ["prepared", "aborted"] ->
                                  sendSnapshot snapshot $ object ["status" .= status]
                            _ -> throwIO $ AppError status500 "invalid merge operation state"
                    _ -> notFound "operation not found"
                else case KM.lookup "data" record of
                  Just result -> sendSnapshot snapshot result
                  Nothing -> throwIO $ AppError status500 "invalid operation record"
        _ -> notFound "operation not found"
    [collection]
      | isCollection collection ->
          route [methodGet, methodPost] $
            if requestMethod request == methodGet
              then listItems repo collection
              else
                if collection == "pulls"
                  then mutate actor repo standardLimit (Pulls.createPull env repo)
                  else mutate actor repo standardLimit $ \user revision operation body -> do
                    normalized <- collectionBody collection body
                    Items.createItem env repo user revision operation normalized
    [collection, ident]
      | isCollection collection ->
          route [methodGet, methodPatch] $
            if requestMethod request == methodGet
              then do
                snapshot <- readSnapshotQuery env repo request
                requireCollection snapshot collection ident
                Store.readItemView snapshot ident >>= sendSnapshot snapshot
              else mutate actor repo standardLimit $ \user revision operation body -> do
                snapshot <- Git.readSnapshot env (Git.trackerPath env repo)
                requireCollection snapshot collection ident
                if collection == "pulls"
                  then Pulls.updatePull env repo user revision operation ident body
                  else Items.updateItem env repo user revision operation ident body
    ["milestones", ident, "progress"] -> route [methodGet] $ do
      snapshot <- readSnapshotQuery env repo request
      requireCollection snapshot "milestones" ident
      items <- Store.loadItems snapshot
      metadata <- Store.records snapshot "items"
      members <- forM metadata $ \(itemId, value) -> case value of
        Object fields
          | KM.lookup "milestone" fields == Just (String ident) ->
              Just <$> maybe (throwIO $ AppError status500 "invalid milestone membership") pure (Map.lookup itemId items)
        _ -> pure Nothing
      let states = [Myque.itemState item | Just item <- members]
          done = length (filter (== Myque.Done) states)
          cancelled = length (filter (== Myque.Cancelled) states)
      sendSnapshot snapshot $
        object
          [ "milestone_id" .= ident
          , "total" .= length states
          , "done" .= done
          , "cancelled" .= cancelled
          , "remaining" .= (length states - done - cancelled)
          ]
    ["pulls", ident, "diff"] -> route [methodGet] $ do
      noQuery request
      value <- Pulls.pullDiff env repo ident
      send $ jsonResponse status200 [] value
    [collection, ident, "comments"] | isCollection collection -> route [methodPost] $
      mutate actor repo standardLimit $ \user revision operation body -> do
        snapshot <- Git.readSnapshot env (Git.trackerPath env repo)
        requireCollection snapshot collection ident
        Items.commentItem env repo user revision operation ident body
    [collection, ident, "timeline"] | isCollection collection -> route [methodGet] $ do
      snapshot <- readSnapshotQuery env repo request
      requireCollection snapshot collection ident
      events <- Items.itemTimeline snapshot ident
      sendSnapshot snapshot (object ["events" .= events])
    ["pulls", ident, "reviews"] ->
      route [methodGet, methodPost] $
        if requestMethod request == methodGet
          then do
            snapshot <- readSnapshotQuery env repo request
            value <- Store.readRecord snapshot "pulls" ident
            case value of
              Object record
                | Just reviews@(Array _) <- KM.lookup "reviews" record ->
                    sendSnapshot snapshot (object ["reviews" .= reviews])
              _ -> throwIO $ AppError status500 "invalid pull record"
          else mutate actor repo standardLimit $ \user revision operation body ->
            Pulls.reviewPull env repo user revision operation ident body
    ["pulls", ident, "merge"] -> route [methodPost] $
      mutate actor repo standardLimit $ \user revision operation body ->
        Pulls.mergePull env repo user revision operation ident body
    _ -> notFound "route not found"
  sendSnapshot snapshot value =
    send $
      jsonResponse status200 (etag $ snapshotRevision snapshot) $
        object ["revision" .= snapshotRevision snapshot, "data" .= value]
  mutate actor repo limit action = do
    noQuery request
    user <- authenticated actor
    Repository.requireAccess env actor repo WriteAccess
    (revision, operation) <- preconditions request
    body <- jsonBody request limit
    Pulls.recoverRepo env repo
    result <- action user revision operation body
    send $
      jsonResponse status200 (etag $ resultRevision result) $
        object
          ["revision" .= resultRevision result, "data" .= resultValue result, "replayed" .= resultReplayed result]
  listItems repo collection = do
    validateQuery request ["kind", "state", "limit", "cursor", "at_revision", "as_of"]
    filters <- queryFilters request
    limit <- pageLimit request
    cursor <- queryParameter request "cursor" >>= traverse parseCursor
    (revision, asOf) <- temporalQuery request
    snapshot <- case cursor of
      Nothing -> Store.snapshotForRead env repo revision asOf
      Just (pinned, _) -> do
        when (maybe False ((/= pinned) . T.toLower) revision) $ badRequest "cursor and at_revision disagree"
        case asOf of
          Nothing -> Store.snapshotForRead env repo (Just pinned) Nothing
          Just _ -> do
            historical <- Store.snapshotForRead env repo Nothing asOf
            unless (snapshotRevision historical == pinned) $ badRequest "cursor and as_of disagree"
            pure historical
    items <- Store.loadItems snapshot
    pulls <- if collection == "pulls" then Map.fromList <$> Store.records snapshot "pulls" else pure Map.empty
    let selected ident item =
          collectionMatches collection item (Map.member ident pulls)
            && maybe True (== Myque.itemKind item) (fst filters)
            && maybe True (== Myque.itemState item) (snd filters)
        identifiers = Map.keys $ Map.filterWithKey selected items
    remaining <- case cursor of
      Nothing -> pure identifiers
      Just (_, after) -> case dropWhile (/= after) identifiers of
        [] -> badRequest "cursor does not identify an item in this result"
        _ : rest -> pure rest
    let page = take limit remaining
        next =
          if length (take (limit + 1) remaining) > limit
            then Just (snapshotRevision snapshot <> ":" <> last page)
            else Nothing
    views <- Store.readItemViews snapshot page
    sendSnapshot snapshot $ object ["items" .= views, "next_cursor" .= next]

standardLimit, importLimit :: Int
standardLimit = 1024 * 1024
importLimit = 4 * standardLimit

gitName :: Text -> Maybe (Text, Bool)
gitName filename = case T.stripSuffix ".tracker.git" filename of
  Just name | not (T.null name) -> Just (name, True)
  _ -> do
    name <- T.stripSuffix ".git" filename
    if T.null name then Nothing else Just (name, False)

authenticate :: Env -> Request -> IO (Maybe Actor)
authenticate env request = case [value | (name, value) <- requestHeaders request, name == hAuthorization] of
  [] -> pure Nothing
  [header] -> do
    tokenBytes <- case B8.words header of
      [scheme, value] | B8.map lowerAscii scheme == "bearer" -> pure value
      [scheme, value] | B8.map lowerAscii scheme == "basic" -> case Base64.decode value of
        Right decoded -> case B8.break (== ':') decoded of
          (_, password) | not (BS.null password) -> pure (BS.drop 1 password)
          _ -> unauthorized
        Left _ -> unauthorized
      _ -> unauthorized
    token <- either (const unauthorized) pure (TE.decodeUtf8' tokenBytes)
    when (T.null token) unauthorized
    Repository.lookupActor env token >>= maybe unauthorized (pure . Just)
  _ -> unauthorized
 where
  lowerAscii c
    | isAsciiUpper c = toEnum (fromEnum c + 32)
    | otherwise = c

unauthorized :: IO a
unauthorized = throwIO $ AppError status401 "authentication required"

authenticated :: Maybe Actor -> IO Actor
authenticated = maybe unauthorized pure

preconditions :: Request -> IO (Text, Text)
preconditions request = do
  rawRevision <- requiredHeader hIfMatch
  revision <- utf8 rawRevision
  let unquoted = T.dropEnd 1 (T.drop 1 revision)
  unless
    ( T.length revision `elem` [42, 66]
        && T.isPrefixOf "\"" revision
        && T.isSuffixOf "\"" revision
        && T.all (\c -> isAscii c && isHexDigit c) unquoted
    )
    $ badRequest "If-Match must be one quoted full Git object ID"
  operation <- requiredHeader "Idempotency-Key" >>= utf8
  validateOperation operation
  pure (T.toLower unquoted, operation)
 where
  requiredHeader name = case [value | (key, value) <- requestHeaders request, key == name] of
    [] -> throwIO $ AppError (mkStatus 428 "Precondition Required") "If-Match and Idempotency-Key are required"
    [value] -> pure value
    _ -> badRequest "duplicate mutation precondition header"

validateOperation :: Text -> IO ()
validateOperation operation =
  unless
    ( not (T.null operation)
        && T.length operation <= 128
        && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("-_.:" :: String))) operation
    )
    $ badRequest "Idempotency-Key must contain 1 to 128 ASCII letters, digits, or -_.:"

utf8 :: BS.ByteString -> IO Text
utf8 = either (const $ badRequest "invalid UTF-8") pure . TE.decodeUtf8'

jsonBody :: Request -> Int -> IO Value
jsonBody request limit = do
  case [value | (key, value) <- requestHeaders request, key == hContentType] of
    [contentType] | B8.map lowerAscii (B8.takeWhile (/= ';') contentType) == "application/json" -> pure ()
    _ -> throwIO $ AppError status415 "Content-Type must be application/json"
  case requestBodyLength request of
    KnownLength lengthBytes | lengthBytes > fromIntegral limit -> tooLarge
    _ -> pure ()
  chunks <- collect 0 []
  either (const $ badRequest "invalid JSON body") pure (eitherDecodeStrict' $ BS.concat $ reverse chunks)
 where
  tooLarge = throwIO $ AppError status413 "request body too large"
  collect size chunks = do
    chunk <- getRequestBodyChunk request
    if BS.null chunk
      then pure chunks
      else do
        let next = size + BS.length chunk
        when (next > limit) tooLarge
        collect next (chunk : chunks)
  lowerAscii c
    | isAsciiUpper c = toEnum (fromEnum c + 32)
    | otherwise = c

closedObject :: [Text] -> Value -> IO Object
closedObject allowed = \case
  Object value -> do
    unless (all ((`elem` allowed) . Key.toText) (KM.keys value)) $ badRequest "unknown JSON field"
    pure value
  _ -> badRequest "JSON body must be an object"

textField :: Key.Key -> Object -> IO Text
textField name value = case KM.lookup name value of
  Just (String text) | not (T.null text) -> pure text
  _ -> badRequest $ Key.toText name <> " must be a nonempty string"

noQuery :: Request -> IO ()
noQuery request = unless (null $ queryString request) $ badRequest "query parameters are not supported on this route"

queryFilters :: Request -> IO (Maybe Myque.Kind, Maybe Myque.State)
queryFilters request = do
  kind <- field "kind" Myque.parseKind
  state <- field "state" Myque.parseState
  pure (kind, state)
 where
  field name parse = case [value | (key, value) <- queryString request, key == name] of
    [] -> pure Nothing
    [Just value] -> do
      text <- utf8 value
      Just <$> either (const $ badRequest "invalid filter value") pure (parse text)
    _ -> badRequest "filter must occur once with a value"

validateQuery :: Request -> [BS.ByteString] -> IO ()
validateQuery request allowed =
  unless (all ((`elem` allowed) . fst) (queryString request)) $ badRequest "unknown query parameter"

queryParameter :: Request -> BS.ByteString -> IO (Maybe Text)
queryParameter request name = case [value | (key, value) <- queryString request, key == name] of
  [] -> pure Nothing
  [Just value] | not (BS.null value) -> Just <$> utf8 value
  _ -> badRequest "query parameter must occur once with a nonempty value"

temporalQuery :: Request -> IO (Maybe Text, Maybe Text)
temporalQuery request = do
  revision <- queryParameter request "at_revision"
  asOf <- queryParameter request "as_of"
  when (isJust revision && isJust asOf) $ badRequest "at_revision and as_of are mutually exclusive"
  pure (revision, asOf)

readSnapshotQuery :: Env -> Repo -> Request -> IO Snapshot
readSnapshotQuery env repo request = do
  validateQuery request ["at_revision", "as_of"]
  (revision, asOf) <- temporalQuery request
  Store.snapshotForRead env repo revision asOf

pageLimit :: Request -> IO Int
pageLimit request =
  queryParameter request "limit" >>= \case
    Nothing -> pure 50
    Just value
      | T.length value <= 3 && T.all isDigit value ->
          let count = T.foldl' (\n c -> n * 10 + fromEnum c - fromEnum '0') 0 value
           in if count >= 1 && count <= 100 then pure count else badRequest "limit must be between 1 and 100"
    _ -> badRequest "limit must be between 1 and 100"

parseCursor :: Text -> IO (Text, Text)
parseCursor value = case T.splitOn ":" value of
  [revision, ident]
    | T.length revision == 40
    , T.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) revision
    , T.length ident == 36
    , T.all (\c -> isAscii c && (isHexDigit c || c == '-')) ident ->
        pure (T.toLower revision, ident)
  _ -> badRequest "invalid cursor"

isCollection :: Text -> Bool
isCollection collection = collection `elem` ["items", "issues", "milestones", "pulls"]

collectionMatches :: Text -> Myque.WorkItem -> Bool -> Bool
collectionMatches "issues" item _ = Myque.itemKind item == Myque.Issue || Myque.itemKind item == Myque.Bug
collectionMatches "milestones" item _ = Myque.itemKind item == Myque.Milestone
collectionMatches "pulls" _ pull = pull
collectionMatches _ _ _ = True

requireCollection :: Snapshot -> Text -> Text -> IO ()
requireCollection snapshot collection ident = do
  items <- Store.loadItems snapshot
  item <- maybe (notFound "item not found") pure (Map.lookup ident items)
  pull <- if collection == "pulls" then isJust . lookup ident <$> Store.records snapshot "pulls" else pure False
  unless (collectionMatches collection item pull) $ notFound "item not found"

collectionBody :: Text -> Value -> IO Value
collectionBody "items" body = pure body
collectionBody collection (Object body) = do
  let defaultKind = if collection == "milestones" then "milestone" else "issue"
  case KM.lookup "kind" body of
    Nothing -> pure $ Object $ KM.insert "kind" (String defaultKind) body
    Just (String kind) | kind == defaultKind || (collection == "issues" && kind == "bug") -> pure $ Object body
    _ -> badRequest "kind does not match collection"
collectionBody _ _ = badRequest "JSON body must be an object"

accessText :: Access -> Text
accessText ReadAccess = "read"
accessText WriteAccess = "write"
accessText AdminAccess = "admin"
