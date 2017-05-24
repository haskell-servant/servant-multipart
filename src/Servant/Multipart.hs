{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | @multipart/form-data@ support for servant.
--
--   This is mostly useful for adding file upload support to
--   an API. See haddocks of 'MultipartForm' for an introduction.
module Servant.Multipart
  ( MultipartForm
  , MultipartData(..)
  , FromMultipart(..)
  , lookupInput
  , lookupFile
  , MultipartOptions(..)
  , defaultMultipartOptions
  , TmpBackendOptions(..)
  , defaultTmpBackendOptions
  , Input(..)
  , FileData(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import Network.HTTP.Media ((//))
import Network.Wai
import Network.Wai.Parse
import Servant
import Servant.Server.Internal
import System.Directory
import System.IO

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
--   type API = MultipartForm MultipartData :> Post '[PlainText] String
--
--   api :: Proxy API
--   api = Proxy
--
--   server :: MultipartData -> Handler String
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
--   instance FromMultipart User where
--     fromMultipart multipartData =
--       User \<$\> lookupInput "username" multipartData
--            \<*\> fmap fileContent (lookupFile "pic" multipartData)
--
--   type API = MultipartForm User :> Post '[PlainText] String
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
data MultipartForm a

-- | What servant gets out of a @multipart/form-data@ form submission.
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
data MultipartData = MultipartData
  { inputs :: [Input]
  , files  :: [FileData]
  }

-- TODO: this is specific to Tmp. we need a version that
-- can handle Mem as well.
fromRaw :: ([Network.Wai.Parse.Param], [File FilePath]) -> MultipartData
fromRaw (inputs, files) = MultipartData is fs

  where is = map (\(name, val) -> Input (dec name) (dec val)) inputs
        fs = map toFile files

        toFile :: File FilePath -> FileData
        toFile (iname, fileinfo) =
          FileData (dec iname)
                   (dec $ fileName fileinfo)
                   (dec $ fileContentType fileinfo)
                   (fileContent fileinfo)

        dec = decodeUtf8

-- | Representation for an uploaded file, usually resulting from
--   picking a local file for an HTML input that looks like
--   @\<input type="file" name="somefile" /\>@.
data FileData = FileData
  { fdInputName :: Text     -- ^ @name@ attribute of the corresponding
                            --   HTML @\<input\>@
  , fdFileName  :: Text     -- ^ name of the file on the client's disk
  , fdFileCType :: Text     -- ^ MIME type for the file
  , fdFilePath  :: FilePath -- ^ path to the temporary file that has the
                            --   content of the user's original file. Only
                            --   valid during the execution of your handler as
                            --   it gets removed right after, which means you
                            --   really want to move or copy it in your handler.
  } deriving (Eq, Show)

-- | Lookup a file input with the given @name@ attribute.
lookupFile :: Text -> MultipartData -> Maybe FileData
lookupFile iname = find ((==iname) . fdInputName) . files

-- | Representation for a textual input (any @\<input\>@ type but @file@).
--
--   @\<input name="foo" value="bar"\ />@ would appear as @'Input' "foo" "bar"@.
data Input = Input
  { iName  :: Text -- ^ @name@ attribute of the input
  , iValue :: Text -- ^ value given for that input
  } deriving (Eq, Show)

-- | Lookup a textual input with the given @name@ attribute.
lookupInput :: Text -> MultipartData -> Maybe Text
lookupInput iname = fmap iValue . find ((==iname) . iName) . inputs

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
--   instance FromMultipart User where
--     fromMultipart form =
--       User \<$\> lookupInput "username" (inputs form)
--            \<*\> fmap fdFilePath (lookupFile "pic" $ files form)
--   @
class FromMultipart a where
  -- | Given a value of type 'MultipartData', which consists
  --   in a list of textual inputs and another list for
  --   files, try to extract a value of type @a@. When
  --   extraction fails, servant errors out with status code 400.
  fromMultipart :: MultipartData -> Maybe a

instance FromMultipart MultipartData where
  fromMultipart = Just

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
---  servant-server will hand a value of type @a@ to your handler
--   assuming the request body's content type is
--   @multipart/form-data@ and the call to 'fromMultipart' succeeds.
instance ( FromMultipart a
         , LookupContext config MultipartOptions
         , HasServer sublayout config )
      => HasServer (MultipartForm a :> sublayout) config where

  type ServerT (MultipartForm a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy config subserver =
    route psub config subserver'
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      popts = Proxy :: Proxy MultipartOptions
      multipartOpts = fromMaybe defaultMultipartOptions
                    $ lookupContext popts config
      subserver' = addMultipartHandling multipartOpts subserver

-- Try and extract the request body as multipart/form-data,
-- returning the data as well as the resourcet InternalState
-- that allows us to properly clean up the temporary files
-- later on.
check :: MultipartOptions -> DelayedIO MultipartData
check opts = withRequest $ \request -> do
  st <- liftResourceT getInternalState
  rawData <- liftIO $ parseRequestBodyEx parseOpts (tmpBackend opts st) request
  return (fromRaw rawData)
  where parseOpts = generalOptions opts

-- Add multipart extraction support to a Delayed.
addMultipartHandling :: FromMultipart multipart
                     => MultipartOptions
                     -> Delayed env (multipart -> a)
                     -> Delayed env a
addMultipartHandling opts subserver =
  addBodyCheck subserver contentCheck bodyCheck
  where
    contentCheck = withRequest $ \request ->
      fuzzyMultipartCTCheck (contentTypeH request)

    bodyCheck () = do
      mpd <- check opts :: DelayedIO MultipartData
      case fromMultipart mpd of
        Nothing -> liftRouteResult $ FailFatal
          err400 { errBody = "fromMultipart returned Nothing" }
        Just x -> return x

    contentTypeH req = fromMaybe "application/octet-stream" $
          lookup "Content-Type" (requestHeaders req)

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

tmpBackend :: MultipartOptions
           -> InternalState
           -> ignored1
           -> ignored2
           -> IO SBS.ByteString
           -> IO FilePath
tmpBackend opts =
    tempFileBackEndOpts (getTmpDir tmpOpts) (filenamePat tmpOpts)
  where
    tmpOpts = tmpOptions opts

-- | Global options for configuring how the
--   server should handle multipart data.
--
--   'generalOptions' lets you specify mostly multipart parsing
--   related options, such as the maximum file size, while
--   'tmpOptions' lets you configure aspects specific to
--   the temporary file backend. See haddocks for
--   'ParseRequestBodyOptions' and 'TmpBackendOptions' respectively
--   for more information on what you can tweak.
data MultipartOptions = MultipartOptions
  { generalOptions :: ParseRequestBodyOptions
  , tmpOptions     :: TmpBackendOptions
  }

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
--   'defaultTmpBackendOptions' respectively.
defaultMultipartOptions :: MultipartOptions
defaultMultipartOptions = MultipartOptions
  { generalOptions = defaultParseRequestBodyOptions
  , tmpOptions = defaultTmpBackendOptions
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
  lookupContext p (c :. cs) =
    lookupContext p cs

instance {-# OVERLAPPING #-}
         LookupContext cs a => LookupContext (a ': cs) a where
  lookupContext _ (c :. _) = Just c

instance HasLink sub => HasLink (MultipartForm a :> sub) where
  type MkLink (MultipartForm a :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)
