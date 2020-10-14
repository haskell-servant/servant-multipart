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
-- | @multipart/form-data@ support for servant.
--
--   This is mostly useful for adding file upload support to
--   an API. See haddocks of 'MultipartForm' for an introduction.
module Servant.Multipart
  ( MultipartForm
  , MultipartForm'
  , MultipartData(..)
  , FromMultipart(..)
  , lookupInput
  , lookupFile
  , MultipartOptions(..)
  , defaultMultipartOptions
  , MultipartBackend(..)
  , Tmp
  , TmpBackendOptions(..)
  , Mem
  , defaultTmpBackendOptions
  , Input(..)
  , FileData(..)
  -- * servant-client
  , genBoundary
  , ToMultipart(..)
  , multipartToBody
  -- * servant-docs
  , ToMultipartSample(..)
  ) where

import Control.Lens ((<>~), (&), view, (.~))
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Array (listArray, (!))
import Data.List (find, foldl')
import Data.Maybe
import Data.Monoid
import Data.String.Conversions (cs)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Network.HTTP.Media.MediaType ((//), (/:))
import Network.Wai
import Network.Wai.Parse
import Servant hiding (contentType)
import Servant.API.Modifiers (FoldLenient)
import Servant.Client.Core (HasClient(..), RequestBody(RequestBodySource), setRequestBody)
import Servant.Docs hiding (samples)
import Servant.Foreign hiding (contentType)
import Servant.Server.Internal
import Servant.Types.SourceT (SourceT(..), source, StepT(..), fromActionStep)
import System.Directory
import System.IO (IOMode(ReadMode), withFile)
import System.Random (getStdRandom, Random(randomR))

import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS

-- | Combinator for specifying a @multipart/form-data@ request
--   body, typically (but not always) issued from an HTML @\<form\>@.
--
--   @multipart/form-data@ can't be made into an ordinary content
--   type for now in servant because it doesn't just decode the
--   request body from some format but also performs IO in the case
--   of writing the uploaded files to disk, e.g in @/tmp@, which is
--   not compatible with servant's vision of a content type as things
--   stand now. This also means that 'MultipartForm' can't be used in
--   conjunction with 'ReqBody' in an endpoint.
--
--   The 'tag' type parameter instructs the function to handle data
--   either as data to be saved to temporary storage ('Tmp') or saved to
--   memory ('Mem').
--
--   The 'a' type parameter represents the Haskell type to which
--   you are going to decode the multipart data to, where the
--   multipart data consists in all the usual form inputs along
--   with the files sent along through @\<input type="file"\>@
--   fields in the form.
--
--   One option provided out of the box by this library is to decode
--   to 'MultipartData'.
--
--   Example:
--
--   @
--   type API = MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String
--
--   api :: Proxy API
--   api = Proxy
--
--   server :: MultipartData Tmp -> Handler String
--   server multipartData = return str
--
--     where str = "The form was submitted with "
--              ++ show nInputs ++ " textual inputs and "
--              ++ show nFiles  ++ " files."
--           nInputs = length (inputs multipartData)
--           nFiles  = length (files multipartData)
--   @
--
--   You can alternatively provide a 'FromMultipart' instance
--   for some type of yours, allowing you to regroup data
--   into a structured form and potentially selecting
--   a subset of the entire form data that was submitted.
--
--   Example, where we only look extract one input, /username/,
--   and one file, where the corresponding input field's /name/
--   attribute was set to /pic/:
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart multipartData =
--       User \<$\> lookupInput "username" multipartData
--            \<*\> fmap fdPayload (lookupFile "pic" multipartData)
--
--   type API = MultipartForm Tmp User :> Post '[PlainText] String
--
--   server :: User -> Handler String
--   server usr = return str
--
--     where str = username usr ++ "'s profile picture"
--              ++ " got temporarily uploaded to "
--              ++ pic usr ++ " and will be removed from there "
--              ++ " after this handler has run."
--   @
--
--   Note that the behavior of this combinator is configurable,
--   by using 'serveWith' from servant-server instead of 'serve',
--   which takes an additional 'Context' argument. It simply is an
--   heterogeneous list where you can for example store
--   a value of type 'MultipartOptions' that has the configuration that
--   you want, which would then get picked up by servant-multipart.
--
--   __Important__: as mentionned in the example above,
--   the file paths point to temporary files which get removed
--   after your handler has run, if they are still there. It is
--   therefore recommended to move or copy them somewhere in your
--   handler code if you need to keep the content around.
type MultipartForm tag a = MultipartForm' '[] tag a

-- | 'MultipartForm' which can be modified with 'Servant.API.Modifiers.Lenient'.
data MultipartForm' (mods :: [*]) tag a

-- | What servant gets out of a @multipart/form-data@ form submission.
--
--   The type parameter 'tag' tells if 'MultipartData' is stored as a
--   temporary file or stored in memory. 'tag' is type of either 'Mem'
--   or 'Tmp'.
--
--   The 'inputs' field contains a list of textual 'Input's, where
--   each input for which a value is provided gets to be in this list,
--   represented by the input name and the input value. See haddocks for
--   'Input'.
--
--   The 'files' field contains a list of files that were sent along with the
--   other inputs in the form. Each file is represented by a value of type
--   'FileData' which among other things contains the path to the temporary file
--   (to be removed when your handler is done running) with a given uploaded
--   file's content. See haddocks for 'FileData'.
data MultipartData tag = MultipartData
  { inputs :: [Input]
  , files  :: [FileData tag]
  }

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

-- | Representation for an uploaded file, usually resulting from
--   picking a local file for an HTML input that looks like
--   @\<input type="file" name="somefile" /\>@.
data FileData tag = FileData
  { fdInputName :: Text     -- ^ @name@ attribute of the corresponding
                            --   HTML @\<input\>@
  , fdFileName  :: Text     -- ^ name of the file on the client's disk
  , fdFileCType :: Text     -- ^ MIME type for the file
  , fdPayload   :: MultipartResult tag
                            -- ^ path to the temporary file that has the
                            --   content of the user's original file. Only
                            --   valid during the execution of your handler as
                            --   it gets removed right after, which means you
                            --   really want to move or copy it in your handler.
  }

deriving instance Eq (MultipartResult tag) => Eq (FileData tag)
deriving instance Show (MultipartResult tag) => Show (FileData tag)

-- | Lookup a file input with the given @name@ attribute.
lookupFile :: Text -> MultipartData tag -> Either String (FileData tag)
lookupFile iname =
  maybe (Left $ "File " <> cs iname <> " not found") Right
  . find ((==iname) . fdInputName)
  . files

-- | Representation for a textual input (any @\<input\>@ type but @file@).
--
--   @\<input name="foo" value="bar"\ />@ would appear as @'Input' "foo" "bar"@.
data Input = Input
  { iName  :: Text -- ^ @name@ attribute of the input
  , iValue :: Text -- ^ value given for that input
  } deriving (Eq, Show)

-- | Lookup a textual input with the given @name@ attribute.
lookupInput :: Text -> MultipartData tag -> Either String Text
lookupInput iname =
  maybe (Left $ "Field " <> cs iname <> " not found") (Right . iValue)
  . find ((==iname) . iName)
  . inputs

-- | 'MultipartData' is the type representing
--   @multipart/form-data@ form inputs. Sometimes
--   you may instead want to work with a more structured type
--   of yours that potentially selects only a fraction of
--   the data that was submitted, or just reshapes it to make
--   it easier to work with. The 'FromMultipart' class is exactly
--   what allows you to tell servant how to turn "raw" multipart
--   data into a value of your nicer type.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart form =
--       User \<$\> lookupInput "username" (inputs form)
--            \<*\> fmap fdPayload (lookupFile "pic" $ files form)
--   @
class FromMultipart tag a where
  -- | Given a value of type 'MultipartData', which consists
  --   in a list of textual inputs and another list for
  --   files, try to extract a value of type @a@. When
  --   extraction fails, servant errors out with status code 400.
  fromMultipart :: MultipartData tag -> Either String a

instance FromMultipart tag (MultipartData tag) where
  fromMultipart = Right

-- | Allows you to tell servant how to turn a more structured type
--   into a 'MultipartData', which is what is actually sent by the
--   client.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance toMultipart Tmp User where
--       toMultipart user = MultipartData [Input "username" $ username user]
--                                        [FileData "pic"
--                                                  (pic user)
--                                                  "image/png"
--                                                  (pic user)
--                                        ]
--   @
class ToMultipart tag a where
  -- | Given a value of type 'a', convert it to a
  -- 'MultipartData'.
  toMultipart :: a -> MultipartData tag

instance ToMultipart tag (MultipartData tag) where
  toMultipart = id

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

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
--   servant-client will take a parameter of type @(LBS.ByteString, a)@,
--   where the bytestring is the boundary to use (see 'genBoundary'), and
--   replace the request body with the contents of the form.
instance (ToMultipart tag a, HasClient m api, MultipartBackend tag)
      => HasClient m (MultipartForm' mods tag a :> api) where

  type Client m (MultipartForm' mods tag a :> api) =
    (LBS.ByteString, a) -> Client m api

  clientWithRoute pm _ req (boundary, param) =
      clientWithRoute pm (Proxy @api) $ setRequestBody newBody newMedia req
    where
      newBody = multipartToBody boundary $ toMultipart @tag param
      newMedia = "multipart" // "form-data" /: ("boundary", LBS.toStrict boundary)

  hoistClientMonad pm _ f cl = \a ->
      hoistClientMonad pm (Proxy @api) f (cl a)

-- | Generates a boundary to be used to separate parts of the multipart.
-- Requires 'IO' because it is randomized.
genBoundary :: IO LBS.ByteString
genBoundary = LBS.pack
            . map (validChars !)
            <$> indices
  where
    -- the standard allows up to 70 chars, but most implementations seem to be
    -- in the range of 40-60, so we pick 55
    indices = replicateM 55 . getStdRandom $ randomR (0,61)
    -- Following Chromium on this one:
    -- > The RFC 2046 spec says the alphanumeric characters plus the
    -- > following characters are legal for boundaries:  '()+_,-./:=?
    -- > However the following characters, though legal, cause some sites
    -- > to fail: (),./:=+
    -- https://github.com/chromium/chromium/blob/6efa1184771ace08f3e2162b0255c93526d1750d/net/base/mime_util.cc#L662-L670
    validChars = listArray (0 :: Int, 61)
                           -- 0-9
                           [ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
                           , 0x38, 0x39, 0x41, 0x42
                           -- A-Z, a-z
                           , 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a
                           , 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52
                           , 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a
                           , 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68
                           , 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70
                           , 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78
                           , 0x79, 0x7a
                           ]

-- | Given a bytestring for the boundary, turns a `MultipartData` into
-- a 'RequestBody'
multipartToBody :: forall tag.
                MultipartBackend tag
                => LBS.ByteString
                -> MultipartData tag
                -> RequestBody
multipartToBody boundary mp = RequestBodySource $ files' <> source ["--", boundary, "--"]
  where
    -- at time of writing no Semigroup or Monoid instance exists for SourceT and StepT
    -- in releases of Servant; they are in master though
    (SourceT l) `mappend'` (SourceT r) = SourceT $ \k ->
                                                   l $ \lstep ->
                                                   r $ \rstep ->
                                                   k (appendStep lstep rstep)
    appendStep Stop        r = r
    appendStep (Error err) _ = Error err
    appendStep (Skip s)    r = appendStep s r
    appendStep (Yield x s) r = Yield x (appendStep s r)
    appendStep (Effect ms) r = Effect $ (flip appendStep r <$> ms)
    mempty' = SourceT ($ Stop)
    crlf = "\r\n"
    lencode = LBS.fromStrict . encodeUtf8
    renderInput input = renderPart (lencode . iName $ input)
                                   "text/plain"
                                   ""
                                   (source . pure . lencode . iValue $ input)
    inputs' = foldl' (\acc x -> acc `mappend'` renderInput x) mempty' (inputs mp)
    renderFile :: FileData tag -> SourceIO LBS.ByteString
    renderFile file = renderPart (lencode . fdInputName $ file)
                                 (lencode . fdFileCType $ file)
                                 ((flip mappend) "\"" . mappend "; filename=\""
                                                      . lencode
                                                      . fdFileName $ file)
                                 (loadFile (Proxy @tag) . fdPayload $ file)
    files' = foldl' (\acc x -> acc `mappend'` renderFile x) inputs' (files mp)
    renderPart name contentType extraParams payload =
      source [ "--"
             , boundary
             , crlf
             , "Content-Disposition: form-data; name=\""
             , name
             , "\""
             , extraParams
             , crlf
             , "Content-Type: "
             , contentType
             , crlf
             , crlf
             ] `mappend'` payload `mappend'` source [crlf]

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

class MultipartBackend tag where
    type MultipartResult tag :: *
    type MultipartBackendOptions tag :: *

    backend :: Proxy tag
            -> MultipartBackendOptions tag
            -> InternalState
            -> ignored1
            -> ignored2
            -> IO SBS.ByteString
            -> IO (MultipartResult tag)

    loadFile :: Proxy tag -> MultipartResult tag -> SourceIO LBS.ByteString

    defaultBackendOptions :: Proxy tag -> MultipartBackendOptions tag

-- | Tag for data stored as a temporary file
data Tmp

-- | Tag for data stored in memory
data Mem

instance MultipartBackend Tmp where
    type MultipartResult Tmp = FilePath
    type MultipartBackendOptions Tmp = TmpBackendOptions

    defaultBackendOptions _ = defaultTmpBackendOptions
    -- streams the file from disk
    loadFile _ fp =
        SourceT $ \k ->
        withFile fp ReadMode $ \hdl ->
        k (readHandle hdl)
      where
        readHandle hdl = fromActionStep LBS.null (LBS.hGet hdl 4096)
    backend _ opts = tmpBackend
      where
        tmpBackend = tempFileBackEndOpts (getTmpDir opts) (filenamePat opts)

instance MultipartBackend Mem where
    type MultipartResult Mem = LBS.ByteString
    type MultipartBackendOptions Mem = ()

    defaultBackendOptions _ = ()
    loadFile _ = source . pure
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

instance HasLink sub => HasLink (MultipartForm tag a :> sub) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (MultipartForm tag a :> sub) r = MkLink sub r
  toLink toA _ = toLink toA (Proxy :: Proxy sub)
#else
  type MkLink (MultipartForm tag a :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)
#endif

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
        [ "This endpoint takes `multipart/form-data` requests.  The following is " <>
          "a list of sample requests:"
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
