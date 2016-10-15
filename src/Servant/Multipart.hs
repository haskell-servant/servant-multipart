{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.Multipart
  ( module Servant.Multipart
  , Param
  , File
  , FileInfo(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.Maybe
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
--   stand now.
--
--   The 'a' type parameter represents the Haskell type to which
--   you are going to decode the multipart data to, where the
--   multipart data consists in all the usual form inputs along
--   with the files sent along through @\<input type="file"\>@
--   fields in the form.
--
--   One option provided out of the box by this library is to decode
--   to 'MultipartData':
--
--   @
--   type MultipartData = ([Param], [File FilePath])
--   @
--
--   Example:
--
--   @
--   type API = MultipartForm MultipartData :> Post '[PlainText] String
--   api :: Proxy API
--   api = Proxy
--
--   server :: MultipartData -> Handler String
--   server (inputs, files) = return str
--
--     where str = "The form was submitted with "
--              ++ show nInputs ++ " textual inputs and "
--              ++ show nFiles  ++ " files."
--           nInputs = length inputs
--           nFiles  = length files
--   @
--
--   You can alternatively provide a 'FromMultipart' instance
--   for some type of yours, allowing you to regroup data
--   into a structured form and potentially selecting
--   a subset of the entire form data that was submitted.
--
--   Example:
--
--   @
--   data User = User { username :: String, pic :: FilePath }
--
--   instance FromMultipart User where
--     fromMultipart (inputs, files) =
--       User \<$\> BS.unpack (lookup "username" inputs)
--            \<*\> fmap fileContent (lookup "pic" files)
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
--   __Important__: as mentionned in the example above,
--   the file paths point to temporary files which get removed
--   after your handler has run, if they are still there. It is
--   therefore recommended to move or copy them somewhere in your
--   handler if you need to keep the content around.
data MultipartForm a

-- | What servant gets out of a @multiart/form-data@ form submission.
--
--   The @[Param]@ component contains a list of
--   @(inputname, inputvalue)@ pairs that correspond
--   to all the non-file inputs of the form, while
--   the @[File FilePath]@ component contains a list
--   of uploaded files. See the haddocks for 'File' to
--   see how you can access e.g the file path for the
--   temporary file (removed after the handler is done)
--   or the file size.
type MultipartData = ([Param], [File FilePath])

class FromMultipart a where
  fromMultipart :: MultipartData -> Maybe a

instance FromMultipart ([Param], [File FilePath]) where
  fromMultipart = Just

instance (FromMultipart a, HasServer sublayout config)
      => HasServer (MultipartForm a :> sublayout) config where

  type ServerT (MultipartForm a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy config subserver =
    route psub config subserver'
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      subserver' = addMultipartHandling subserver

check :: DelayedIO (MultipartData, InternalState)
check = withRequest $ \request -> liftIO $ do
  st <- createInternalState
  (params, files) <- parseRequestBody (tmpBackend st) request
  return ((params, files), st)

  where f (input, fileinfo) = (input, g fileinfo)
        g FileInfo{..} = FileInfo{fileContent = snd fileContent, ..}

cleanup :: (MultipartData, InternalState) -> IO ()
cleanup (multipartData, internalState) = do
  let filepaths = map (fileContent . snd) (snd multipartData)
  removeFileKeys <- forM filepaths $ \fp ->
    runInternalState
      (register $ do
          exists <- doesFileExist fp
          when exists $ removeFile fp
      )
      internalState
  mapM_ release removeFileKeys
  closeInternalState internalState

-- TODO: check the content-type as well
-- (with addAcceptCheck)
addMultipartHandling :: FromMultipart multipart
                     => Delayed env (multipart -> a)
                     -> Delayed env a
addMultipartHandling Delayed{..} =
  Delayed { bodyD     = withRequest $ \request -> do
              fuzzyMultipartCTCheck (contentTypeH request)
              b <- bodyD
              b' <- check
              return (b, b')
          , serverD   = \cs a (b, (multipartData, st)) req ->
              case fromMultipart multipartData of
                Nothing -> FailFatal $
                  err400 { errBody = "fromMultipart returned Nothing" }
                Just x  -> fmap ($ x) $
                  serverD cs a b req
          , cleanupD  = \(_, multipartStuffs) -> cleanup multipartStuffs
          , capturesD = capturesD
          , methodD   = methodD
          , authD     = authD
          }

  where contentTypeH req = fromMaybe "application/octed-stream" $
          lookup "Content-Type" (requestHeaders req)

fuzzyMultipartCTCheck :: SBS.ByteString -> DelayedIO ()
fuzzyMultipartCTCheck ct
  | ctMatches = return ()
  | otherwise = delayedFailFatal err400 {
      errBody = "The content type of the request body is not in application/x-www-form-urlencoded or multipart/form-data"
      }

  where (ctype, attrs) = parseContentType ct
        ctMatches = case ctype of
          "application/x-www-form-urlencoded" -> True
          "multipart/form-data" | Just bound <- lookup "boundary" attrs -> True
          _ -> False

        -- ^ replace with a list of actual content types for
        --   multipart form data

-- TODO: make file size limit, temp dir, filename pattern
-- etc configurable -- using Context?
tmpBackend :: InternalState
           -> ignored1
           -> ignored2
           -> IO SBS.ByteString
           -> IO FilePath
tmpBackend st _ _ popper = do
  (hcloseReleaseKey, (fp, h)) <-
    flip runInternalState st $ allocate tmpFile (hClose . snd)
  fix $ \loop -> do
    bs <- popper
    unless (SBS.null bs) $ do
      SBS.hPut h bs
      loop
  -- make sure the file is closed by now
  release hcloseReleaseKey
  return fp

  where tmpFile = do
          tmpdir <- getTemporaryDirectory
          openBinaryTempFile tmpdir "servant-multipart.buf"
