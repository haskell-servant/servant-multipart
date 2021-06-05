{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Proxy
import Network.HTTP.Client      hiding (Proxy)
import Network.Socket           (withSocketsDo)
import Servant.API
import Servant.Client           (client, mkClientEnv, runClientM)
import Servant.Client.Core      (BaseUrl (BaseUrl), Scheme (Http))
import Servant.Multipart.API
import Servant.Multipart.Client

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

clientApi :: Proxy (MemToTmp API)
clientApi = Proxy

main :: IO ()
main = withSocketsDo $ do
  manager <- newManager defaultManagerSettings
  boundary <- genBoundary
  let burl = BaseUrl Http "localhost" 8080 ""
      runC cli = runClientM cli (mkClientEnv manager burl)
  resp <- runC $ client clientApi (boundary, form)
  print resp

  where form = MultipartData [ Input "title" "World"
                             , Input "text" "Hello"
                             ]
                             [ FileData "file" "./servant-multipart/servant-multipart.cabal"
                                        "text/plain" "./servant-multipart/servant-multipart.cabal"
                             , FileData "otherfile" "./servant-multipart/Setup.hs" "text/plain" "./servant-multipart/Setup.hs"
                             ]
