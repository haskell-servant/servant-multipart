{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network.Socket (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import System.Environment (getArgs)
import Servant.Client (client, runClientM, mkClientEnv)
import Servant.Client.Core (BaseUrl(BaseUrl), Scheme(Http))

import qualified Data.ByteString.Lazy as LBS

-- Our API, which consists in a single POST endpoint at /
-- that takes a multipart/form-data request body and
-- pretty-prints the data it got to stdout before returning 0.
type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

-- We want to load our file from disk, so we need to convert
-- the 'Mem's in the serverside API to 'Tmp's
type family MemToTmp api where
  MemToTmp (a :<|> b) = MemToTmp a :<|> MemToTmp b
  MemToTmp (a :> b) = MemToTmp a :> MemToTmp b
  MemToTmp (MultipartForm Mem (MultipartData Mem)) = MultipartForm Tmp (MultipartData Tmp)
  MemToTmp a = a

api :: Proxy API
api = Proxy

clientApi :: Proxy (MemToTmp API)
clientApi = Proxy

-- The handler for our single endpoint.
-- Its concrete type is:
--   MultipartData -> Handler Integer
--
-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
upload :: Server API
upload multipartData = do
  liftIO $ do
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
startServer = run 8080 (serve api upload)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run":_) -> withSocketsDo $ do
      _ <- forkIO startServer
      -- we fork the server in a separate thread and send a test
      -- request to it from the main thread.
      manager <- newManager defaultManagerSettings
      boundary <- genBoundary
      let burl = BaseUrl Http "localhost" 8080 ""
          runC cli = runClientM cli (mkClientEnv manager burl)
      resp <- runC $ client clientApi (boundary, form)
      print resp
    _ -> putStrLn "Pass run to run"

  where form = MultipartData [ Input "title" "World"
                             , Input "text" "Hello"
                             ]
                             [ FileData "file" "./servant-multipart.cabal"
                                        "text/plain" "./servant-multipart.cabal"
                             , FileData "otherfile" "./Setup.hs" "text/plain" "./Setup.hs"
                             ]
