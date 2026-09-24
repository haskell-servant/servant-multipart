{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
-- | @multipart/form-data@ server-side support for servant.
--   See servant-multipart-api for the API definitions.
module Servant.Multipart
  ( MultipartForm
  , MultipartForm'
  , MultipartData(..)
  , FromMultipart(..)
  , lookupInput
  , lookupFile
  , lookupAllInputs
  , lookupAllFiles 
  , lookupInputAs 
  , lookupAllInputsAs 
  , CheckError(..)
  , LimitExceeded(..)
  , InvalidUtf8(..)
  , MultipartOptions(..)
  , defaultMultipartOptions
  , MultipartBackend(..)
  , Tmp
  , TmpBackendOptions(..)
  , Mem
  , defaultTmpBackendOptions
  , Input(..)
  , FileData(..)
  -- * servant-docs
  , ToMultipartSample(..)
  , LookupContext(..)
  ) where

import Servant.Multipart.API

import Control.DeepSeq (NFData (rnf))
import Control.Lens ((<>~), (&), view, (.~))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bifunctor (first)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest (PayloadTooLarge, RequestHeaderFieldsTooLarge))
import Network.Wai.Parse
import Servant hiding (contentType)
import Servant.API.Modifiers (FoldLenient)
import Servant.Docs hiding (samples)
import Servant.Foreign hiding (contentType)
import Servant.Server.Internal
import System.Directory

import qualified Control.Exception        as E
import qualified Data.ByteString          as SBS
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE

fromRaw :: forall tag. ([Network.Wai.Parse.Param], [File (MultipartResult tag)])
        -> Either CheckError (MultipartData tag)
fromRaw (inputs, files) =
  MultipartData <$> traverse toInput inputs <*> traverse toFile files

  where toInput (iname, val) =
          Input <$> decInput "name" iname iname
                <*> decInput "value" iname val

        toFile :: File (MultipartResult tag) -> Either CheckError (FileData tag)
        toFile (iname, fileinfo) =
          FileData <$> decFile "name" iname iname
                   <*> decFile "file name" iname (fileName fileinfo)
                   <*> decFile "content type" iname (fileContentType fileinfo)
                   <*> pure (fileContent fileinfo)

        decInput = dec "input"
        decFile  = dec "file input"

        dec :: String -> String -> SBS.ByteString -> SBS.ByteString
            -> Either CheckError Text
        dec kind part iname raw =
          case decodeUtf8' raw of
            Right text -> Right text
            Left _     -> Left $ DecodeError $ InvalidUtf8 part kind iname

class MultipartBackend tag where
    type MultipartBackendOptions tag :: *

    backend :: Proxy tag
            -> MultipartBackendOptions tag
            -> InternalState
            -> ignored1
            -> ignored2
            -> IO SBS.ByteString
            -> IO (MultipartResult tag)

    defaultBackendOptions :: Proxy tag -> MultipartBackendOptions tag

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
---  servant-server will hand a value of type @a@ to your handler
--   assuming the request body's content type is
--   @multipart/form-data@, the form's names, values, file names and
--   content types are valid UTF-8, and the call to 'fromMultipart'
--   succeeds.
instance ( FromMultipart tag a
         , MultipartBackend tag
         , LookupContext config (MultipartOptions tag)
         , LookupContext config ErrorFormatters
         , SBoolI (FoldLenient mods)
         , HasServer sublayout config )
      => HasServer (MultipartForm' mods tag a :> sublayout) config where

  type ServerT (MultipartForm' mods tag a :> sublayout) m =
    If (FoldLenient mods) (Either CheckError a) a -> ServerT sublayout m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy sublayout) pc nt . s

  route Proxy config subserver =
    route psub config subserver'
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      popts = Proxy :: Proxy (MultipartOptions tag)
      multipartOpts = fromMaybe (defaultMultipartOptions pbak)
                    $ lookupContext popts config
      subserver' = addMultipartHandling @tag @a @mods @config pbak multipartOpts config subserver

check :: MultipartBackend tag
      => Proxy tag
      -> MultipartOptions tag
      -> DelayedIO (Either CheckError (MultipartData tag))
check pTag tag = withRequest $ \request -> do
  st <- liftResourceT getInternalState
  let parse = fromRaw <$> parseRequestBodyEx parseOpts (backend pTag (backendOptions tag) st) request
  liftIO $
    E.catchJust invalidRequestLimit
      (E.handle (pure . Left . LimitError . requestParseLimit) parse)
      (pure . Left . LimitError)
  where parseOpts = generalOptions tag

-- | Why a @multipart/form-data@ request body was not decoded. Under
--   'Servant.API.Modifiers.Lenient', the handler is passed this instead of
--   the request being rejected.
data CheckError
  = ParseError String
  | DecodeError InvalidUtf8
    -- ^ The form's text is not valid UTF-8, or 'fromMultipart' failed.
  | LimitError LimitExceeded
    -- ^ The form exceeds one of the 'generalOptions' limits.
  deriving (Eq, Show)

instance NFData CheckError where
  rnf (ParseError message) = rnf message
  rnf (DecodeError limit) = rnf limit
  rnf (LimitError limit) = rnf limit

-- | A @multipart/form-data@ request body that exceeds one of the
--   'generalOptions' limits.
data LimitExceeded = LimitExceeded
  { statusOverride :: Maybe (Int, String)
    -- ^ The status code and reason phrase that the rejection responds with
    --   in place of the one from the 'ErrorFormatters', if any.
  , limitMessage   :: String
  } deriving (Eq, Show)

instance NFData LimitExceeded where
  rnf (LimitExceeded override message) = rnf override `seq` rnf message

data InvalidUtf8 = InvalidUtf8
  { kind :: String
  , part :: String
  , iname :: SBS.ByteString
  } deriving (Eq, Show)

instance NFData InvalidUtf8 where
  rnf (InvalidUtf8 {..}) = rnf kind `seq` rnf part `seq` rnf iname

requestParseLimit :: RequestParseException -> LimitExceeded
requestParseLimit e = case e of
  MaxParamSizeExceeded _ -> LimitExceeded payloadTooLarge "the form exceeds a size limit"
  ParamNameTooLong _ maxLength ->
    LimitExceeded Nothing $ "an input name exceeds " <> show maxLength <> " bytes"
  FilenameTooLong _ maxLength ->
    LimitExceeded Nothing $ "a file input name exceeds " <> show maxLength <> " bytes"
  MaxFileNumberExceeded maxFiles ->
    LimitExceeded Nothing $ "the form has more than " <> show maxFiles <> " files"
  TooManyHeaderLines _ -> LimitExceeded headerFieldsTooLarge "a part has too many header lines"

invalidRequestLimit :: InvalidRequest -> Maybe LimitExceeded
invalidRequestLimit e = case e of
  PayloadTooLarge -> Just $ LimitExceeded payloadTooLarge "the form exceeds a size limit"
  RequestHeaderFieldsTooLarge ->
    Just $ LimitExceeded headerFieldsTooLarge "a part header line exceeds the length limit"
  _ -> Nothing

payloadTooLarge :: Maybe (Int, String)
payloadTooLarge = Just (errHTTPCode err413, errReasonPhrase err413)

headerFieldsTooLarge :: Maybe (Int, String)
headerFieldsTooLarge = Just (431, "Request Header Fields Too Large")

-- Add multipart extraction support to a Delayed.
addMultipartHandling :: forall tag multipart (mods :: [*]) config env a.
                     ( FromMultipart tag multipart
                     , MultipartBackend tag
                     , LookupContext config ErrorFormatters
                     )
                     => SBoolI (FoldLenient mods)
                     => Proxy tag
                     -> MultipartOptions tag
                     -> Context config
                     -> Delayed env (If (FoldLenient mods) (Either CheckError multipart) multipart -> a)
                     -> Delayed env a
addMultipartHandling pTag opts config subserver =
  addBodyCheck subserver contentCheck bodyCheck
  where
    contentCheck = withRequest $ \request ->
      fuzzyMultipartCTCheck (contentTypeH request)

    bodyCheck () = withRequest $ \ request -> do
      checked <- check pTag opts
      case (sbool :: SBool (FoldLenient mods), checked >>= first ParseError . fromMultipart @tag @multipart) of
        (SFalse, Left (ParseError msg)) -> liftRouteResult $ FailFatal $ formatError request msg
        (SFalse, Left (LimitError LimitExceeded {..})) ->
          liftRouteResult $ FailFatal $ withStatus statusOverride (formatError request limitMessage)
        (SFalse, Left (DecodeError InvalidUtf8 {..})) ->
          liftRouteResult $ FailFatal $ formatError request $
              part <> " of " <> kind <> " " <> show iname
                   <> " is not valid UTF-8"
        (SFalse, Right x) -> return x
        (STrue, res) -> return res

    contentTypeH req = fromMaybe "application/octet-stream" $
          lookup "Content-Type" (requestHeaders req)

    withStatus = maybe id $ \(code, phrase) err ->
      err { errHTTPCode = code, errReasonPhrase = phrase }
    defaultFormatError msg = err400 { errBody = "Could not decode multipart mime body: " <> TLE.encodeUtf8 (TL.pack msg) }
    pFormatters = Proxy :: Proxy ErrorFormatters
    rep = typeRep (Proxy :: Proxy MultipartForm')
    formatError request =
      case lookupContext pFormatters config of
        Nothing -> defaultFormatError
        Just fmts -> bodyParserErrorFormatter fmts rep request

-- Check that the content type is one of:
--   - application/x-www-form-urlencoded
--   - multipart/form-data; boundary=something
fuzzyMultipartCTCheck :: SBS.ByteString -> DelayedIO ()
fuzzyMultipartCTCheck ct
  | ctMatches = return ()
  | otherwise = delayedFailFatal err400 {
      errBody = "The content type of the request body is not in application/x-www-form-urlencoded or multipart/form-data"
      }

  where (ctype, attrs) = parseContentType ct
        ctMatches = case ctype of
          "application/x-www-form-urlencoded" -> True
          "multipart/form-data" | Just _bound <- lookup "boundary" attrs -> True
          _ -> False

-- | Global options for configuring how the
--   server should handle multipart data.
--
--   'generalOptions' lets you specify mostly multipart parsing
--   related options, such as the maximum file size, while
--   'backendOptions' lets you configure aspects specific to the chosen
--   backend. Note: there isn't anything to tweak in a memory
--   backend ('Mem'). Maximum file size etc. options are in
--   'ParseRequestBodyOptions'.
--
--   See haddocks for 'ParseRequestBodyOptions' and
--   'TmpBackendOptions' respectively for more information on
--   what you can tweak.
--
--   A form that exceeds one of the 'generalOptions' limits is rejected
--   before the handler runs, unless 'Servant.API.Modifiers.Lenient' is used,
--   in which case the handler is passed a 'LimitError'. The response is
--   built by the 'ErrorFormatters' in the context, if any, like other
--   request body errors, except that exceeding a size limit
--   always responds with status 413 and exceeding a part header limit
--   always responds with status 431.
data MultipartOptions tag = MultipartOptions
  { generalOptions        :: ParseRequestBodyOptions
  , backendOptions        :: MultipartBackendOptions tag
  }

instance MultipartBackend Tmp where
    type MultipartBackendOptions Tmp = TmpBackendOptions

    defaultBackendOptions _ = defaultTmpBackendOptions
    backend _ opts = tmpBackend
      where
        tmpBackend = tempFileBackEndOpts (getTmpDir opts) (filenamePat opts)

instance MultipartBackend Mem where
    type MultipartBackendOptions Mem = ()

    defaultBackendOptions _ = ()
    backend _ _ _ = lbsBackEnd

-- | Configuration for the temporary file based backend.
--
--   You can configure the way servant-multipart gets its hands
--   on a temporary directory (defaults to 'getTemporaryDirectory')
--   as well as the filename pattern used for generating the temporary files
--   (defaults to calling them /servant-multipartXXX.buf/, where /XXX/ is some
--   random number).
data TmpBackendOptions = TmpBackendOptions
  { getTmpDir   :: IO FilePath
  , filenamePat :: String
  }

-- | Default options for the temporary file backend:
--   'getTemporaryDirectory' and "servant-multipart.buf"
defaultTmpBackendOptions :: TmpBackendOptions
defaultTmpBackendOptions = TmpBackendOptions
  { getTmpDir = getTemporaryDirectory
  , filenamePat = "servant-multipart.buf"
  }

-- | Default configuration for multipart handling.
--
--   Uses 'defaultParseRequestBodyOptions' with a maximum size of 25 MiB
--   per file, and 'defaultBackendOptions'. Use 'setMaxRequestFileSize' or
--   'noLimitParseRequestBodyOptions' on 'generalOptions' to change the limit.
defaultMultipartOptions :: MultipartBackend tag => Proxy tag -> MultipartOptions tag
defaultMultipartOptions pTag = MultipartOptions
  { generalOptions = setMaxRequestFileSize (25 * 1024 * 1024) defaultParseRequestBodyOptions
  , backendOptions = defaultBackendOptions pTag
  }

-- Utility class that's like HasContextEntry
-- but allows the lookup to fail, to make a context
-- entry for upload config optional (hence using
-- some default configuration when missing)
class LookupContext ctx a where
  lookupContext :: Proxy a -> Context ctx -> Maybe a

instance LookupContext '[] a where
  lookupContext _ _ = Nothing

instance {-# OVERLAPPABLE #-}
         LookupContext cs a => LookupContext (c ': cs) a where
  lookupContext p (_ :. cxts) =
    lookupContext p cxts

instance {-# OVERLAPPING #-}
         LookupContext cs a => LookupContext (a ': cs) a where
  lookupContext _ (c :. _) = Just c

-- | The 'ToMultipartSample' class allows you to create sample 'MultipartData'
-- inputs for your type for use with "Servant.Docs".  This is used by the
-- 'HasDocs' instance for 'MultipartForm'.
--
-- Given the example @User@ type and 'FromMultipart' instance from the
-- 'MultipartForm' documentation, here is a corresponding 'ToMultipartSample'
-- instance:
--
-- @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance 'ToMultipartSample' 'Tmp' User where
--     'toMultipartSamples' proxy =
--       [ ( \"sample 1\"
--         , 'MultipartData'
--             [ 'Input' \"username\" \"Elvis Presley\" ]
--             [ 'FileData'
--                 \"pic\"
--                 \"playing_guitar.jpeg\"
--                 \"image/jpeg\"
--                 \"/tmp/servant-multipart000.buf\"
--             ]
--         )
--       ]
-- @
class ToMultipartSample tag a where
  toMultipartSamples :: Proxy a -> [(Text, MultipartData tag)]

-- | Format an 'Input' into a markdown list item.
multipartInputToItem :: Input -> Text
multipartInputToItem (Input name val) =
  "        - *" <> name <> "*: " <> "`" <> val <> "`"

-- | Format a 'FileData' into a markdown list item.
multipartFileToItem :: FileData tag -> Text
multipartFileToItem (FileData name _ contentType _) =
  "        - *" <> name <> "*, content-type: " <> "`" <> contentType <> "`"

-- | Format a description and a sample 'MultipartData' into a markdown list
-- item.
multipartSampleToDesc
  :: Text -- ^ The description for the sample.
  -> MultipartData tag -- ^ The sample 'MultipartData'.
  -> Text -- ^ A markdown list item.
multipartSampleToDesc desc (MultipartData inputs files) =
  "- " <> desc <> "\n" <>
  "    - textual inputs (any `<input>` type but file):\n" <>
  foldMap (\input -> multipartInputToItem input <> "\n") inputs <>
  "    - file inputs (any HTML input that looks like `<input type=\"file\" name=\"somefile\" />`):\n" <>
  foldMap (\file -> multipartFileToItem file <> "\n") files

-- | Format a list of samples generated with 'ToMultipartSample' into sections
-- of markdown.
toMultipartDescriptions
  :: forall tag a.
     ToMultipartSample tag a
  => Proxy tag -> Proxy a -> [Text]
toMultipartDescriptions _ proxyA = fmap (uncurry multipartSampleToDesc) samples
  where
    samples :: [(Text, MultipartData tag)]
    samples = toMultipartSamples proxyA

-- | Create a 'DocNote' that represents samples for this multipart input.
toMultipartNotes
  :: ToMultipartSample tag a
  => Int -> Proxy tag -> Proxy a -> DocNote
toMultipartNotes maxSamples' proxyTag proxyA =
  let sampleLines = take maxSamples' $ toMultipartDescriptions proxyTag proxyA
      body =
        [ "This endpoint takes `multipart/form-data` requests. " <>
          "The following is a list of sample requests:"
        , foldMap (<> "\n") sampleLines
        ]
  in DocNote "Multipart Request Samples" $ fmap unpack body

-- | Declare an instance of 'ToMultipartSample' for your 'MultipartForm' type
-- to be able to use this 'HasDocs' instance.
instance (HasDocs api, ToMultipartSample tag a) => HasDocs (MultipartForm' mods tag a :> api) where
  docsFor
    :: Proxy (MultipartForm' mods tag a :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor _ (endpoint, action) opts =
    let newAction =
          action
            & notes <>~
                [ toMultipartNotes
                    (view maxSamples opts)
                    (Proxy :: Proxy tag)
                    (Proxy :: Proxy a)
                ]
    in docsFor (Proxy :: Proxy api) (endpoint, newAction) opts

instance (HasForeignType lang ftype a, HasForeign lang ftype api)
      => HasForeign lang ftype (MultipartForm' mods t a :> api) where
  type Foreign ftype (MultipartForm' mods t a :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy @api) $
      req & reqBody .~ Just t
          & reqBodyContentType .~ ReqBodyMultipart
    where
      t = typeFor lang ftype (Proxy @a)
