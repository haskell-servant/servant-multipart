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
upload (inputs, files) = do
  liftIO $ do
    putStrLn "Inputs:"
    mapM_ print inputs
    putStrLn "Files:"
    mapM_ print files
    forM_ files $ \(fileInputName, file) -> do
      content <- readFile (fileContent file)
      putStrLn $ "Content of " ++ show (fileName file)
      putStrLn content
  return 0

startServer :: IO ()
startServer = run 8080 (logging $ serve api upload)

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
