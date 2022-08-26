{-# LANGUAGE CPP #-}
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
  , LookupContext(..)
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
  ) where

import Servant.Multipart.API

import Control.Lens ((<>~), (&), view, (.~))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.List (find)
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import Network.Wai
import Network.Wai.Parse
import Servant hiding (contentType)
import Servant.API.Modifiers (FoldLenient)
import Servant.Docs hiding (samples)
import Servant.Foreign hiding (contentType)
import Servant.Server.Internal
import System.Directory

import qualified Data.ByteString      as SBS

-- | Lookup a textual input with the given @name@ attribute.
lookupInput :: Text -> MultipartData tag -> Either String Text
lookupInput iname =
  maybe (Left $ "Field " <> cs iname <> " not found") (Right . iValue)
  . find ((==iname) . iName)
  . inputs

-- | Lookup a file input with the given @name@ attribute.
lookupFile :: Text -> MultipartData tag -> Either String (FileData tag)
lookupFile iname =
  maybe (Left $ "File " <> cs iname <> " not found") Right
  . find ((==iname) . fdInputName)
  . files

fromRaw :: forall tag. ([Network.Wai.Parse.Param], [File (MultipartResult tag)])
        -> MultipartData tag
fromRaw (inputs, files) = MultipartData is fs

  where is = map (\(name, val) -> Input (dec name) (dec val)) inputs
        fs = map toFile files

        toFile :: File (MultipartResult tag) -> FileData tag
        toFile (iname, fileinfo) =
          FileData (dec iname)
                   (dec $ fileName fileinfo)
                   (dec $ fileContentType fileinfo)
                   (fileContent fileinfo)

        dec = decodeUtf8

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
--   @multipart/form-data@ and the call to 'fromMultipart' succeeds.
instance ( FromMultipart tag a
         , MultipartBackend tag
         , LookupContext config (MultipartOptions tag)
#if MIN_VERSION_servant_server(0,18,0)
         , LookupContext config ErrorFormatters
#endif
         , SBoolI (FoldLenient mods)
         , HasServer sublayout config )
      => HasServer (MultipartForm' mods tag a :> sublayout) config where

  type ServerT (MultipartForm' mods tag a :> sublayout) m =
    If (FoldLenient mods) (Either String a) a -> ServerT sublayout m

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy sublayout) pc nt . s
#endif

  route Proxy config subserver =
    route psub config subserver'
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      popts = Proxy :: Proxy (MultipartOptions tag)
      multipartOpts = fromMaybe (defaultMultipartOptions pbak)
                    $ lookupContext popts config
      subserver' = addMultipartHandling @tag @a @mods @config pbak multipartOpts config subserver

-- Try and extract the request body as multipart/form-data,
-- returning the data as well as the resourcet InternalState
-- that allows us to properly clean up the temporary files
-- later on.
check :: MultipartBackend tag
      => Proxy tag
      -> MultipartOptions tag
      -> DelayedIO (MultipartData tag)
check pTag tag = withRequest $ \request -> do
  st <- liftResourceT getInternalState
  rawData <- liftIO
      $ parseRequestBodyEx
          parseOpts
          (backend pTag (backendOptions tag) st)
          request
  return (fromRaw rawData)
  where parseOpts = generalOptions tag

-- Add multipart extraction support to a Delayed.
addMultipartHandling :: forall tag multipart (mods :: [*]) config env a.
                     ( FromMultipart tag multipart
                     , MultipartBackend tag
#if MIN_VERSION_servant_server(0,18,0)
                     , LookupContext config ErrorFormatters
#endif
                     )
                     => SBoolI (FoldLenient mods)
                     => Proxy tag
                     -> MultipartOptions tag
                     -> Context config
                     -> Delayed env (If (FoldLenient mods) (Either String multipart) multipart -> a)
                     -> Delayed env a
addMultipartHandling pTag opts _config subserver =
  addBodyCheck subserver contentCheck bodyCheck
  where
    contentCheck = withRequest $ \request ->
      fuzzyMultipartCTCheck (contentTypeH request)

    bodyCheck () = withRequest $ \ request -> do
      mpd <- check pTag opts :: DelayedIO (MultipartData tag)
      case (sbool :: SBool (FoldLenient mods), fromMultipart @tag @multipart mpd) of
        (SFalse, Left msg) -> liftRouteResult $ FailFatal $ formatError request msg
        (SFalse, Right x) -> return x
        (STrue, res) -> return $ either (Left . cs) Right res

    contentTypeH req = fromMaybe "application/octet-stream" $
          lookup "Content-Type" (requestHeaders req)

    defaultFormatError msg = err400 { errBody = "Could not decode multipart mime body: " <> cs msg }
#if MIN_VERSION_servant_server(0,18,0)
    pFormatters = Proxy :: Proxy ErrorFormatters
    rep = typeRep (Proxy :: Proxy MultipartForm')
    formatError request =
      case lookupContext pFormatters _config of
        Nothing -> defaultFormatError
        Just fmts -> bodyParserErrorFormatter fmts rep request
#else
    formatError _ = defaultFormatError
#endif

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
--   Uses 'defaultParseRequestBodyOptions' and
--   'defaultBackendOptions' respectively.
defaultMultipartOptions :: MultipartBackend tag => Proxy tag -> MultipartOptions tag
defaultMultipartOptions pTag = MultipartOptions
  { generalOptions = defaultParseRequestBodyOptions
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
-- Given the example 'User' type and 'FromMultipart' instance above, here is a
-- corresponding 'ToMultipartSample' instance:
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
instance (HasDocs api, ToMultipartSample tag a) => HasDocs (MultipartForm tag a :> api) where
  docsFor
    :: Proxy (MultipartForm tag a :> api)
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
      => HasForeign lang ftype (MultipartForm t a :> api) where
  type Foreign ftype (MultipartForm t a :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy @api) $
      req & reqBody .~ Just t
          & reqBodyContentType .~ ReqBodyMultipart
    where
      t = typeFor lang ftype (Proxy @a)
