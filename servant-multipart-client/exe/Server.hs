{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Control.Monad.IO.Class
import Network.Socket           (withSocketsDo)
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart

import qualified Data.ByteString.Lazy as LBS

-- Our API, which consists in a single POST endpoint at /
-- that takes a multipart/form-data request body and
-- pretty-prints the data it got to stdout before returning 0.
type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

-- The handler for our single endpoint.
-- Its concrete type is:
--   MultipartData -> Handler Integer
--
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = liftIO $ do
  putStrLn "Inputs:"
  forM_ (inputs multipartData) $ \input ->
    putStrLn $ "  " ++ show (iName input)
          ++ " -> " ++ show (iValue input)

  forM_ (files multipartData) $ \file -> do
    let content = fdPayload file
    putStrLn $ "Content of " ++ show (fdFileName file)
    LBS.putStr content
  return 0

startServer :: IO ()
startServer = run 8080 $ serve api upload

main :: IO ()
main = withSocketsDo startServer
