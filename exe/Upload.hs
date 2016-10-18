{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Network (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart

type API = MultipartForm MultipartData :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

logging :: Middleware
logging app req resp = do
  print req
  app req resp

upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      content <- readFile (fdFilePath file)
      putStrLn $ "Content of " ++ show (fdFileName file)
      putStrLn content
  return 0

startServer :: IO ()
startServer = run 8080 (serve api upload)

main = withSocketsDo $ do
  forkIO startServer
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/"
  resp <- flip httpLbs manager =<< formDataBody form req
  print resp

  where form =
          [ partBS "title" "World"
          , partBS "text" $ encodeUtf8 "Hello"
          , partFileSource "file" "./servant-multipart.cabal"
          , partFileSource "otherfile" "./Setup.hs"
          ]
