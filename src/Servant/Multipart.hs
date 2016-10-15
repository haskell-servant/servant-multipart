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

data MultipartForm a

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
