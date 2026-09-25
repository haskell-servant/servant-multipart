{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class     (liftIO)
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8)
import Network.HTTP.Types.Header  (hContentType)
import Servant
import Servant.Client.Core        (RequestBody (RequestBodySource))
import Servant.Multipart
import Servant.Multipart.Client
import Servant.Types.SourceT      (runSourceT)
import Test.Tasty
import Test.Tasty.Wai

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

main :: IO ()
main = defaultMain $ testGroup "servant-multipart-client"
  [ testGroup "part header values round-trip to the server"
      [ roundTrip "plain names" "name" "file" "f.txt" "name" "file" "f.txt"
      , roundTrip "double quotes" "a\"b" "c\"d" "e\"f.txt" "a\"b" "c\"d" "e\"f.txt"
      , roundTrip "backslashes" "x\\" "y\\" "C:\\Users\\a\\f.txt" "x\\" "y\\" "C:\\Users\\a\\f.txt"
      , roundTrip "injected header" "a\"\r\nX: y" "b" "c.txt" "a\"%0D%0AX: y" "b" "c.txt"
      , roundTrip "carriage returns and line feeds" "a\rb" "c\nd" "e\r\nf.txt" "a%0Db" "c%0Ad" "e%0D%0Af.txt"
      ]
  ]

type EchoAPI = "echo" :> MultipartForm Mem (MultipartData Mem) :> Post '[PlainText] Text

echoApp :: Application
echoApp = serve @EchoAPI Proxy $ \md -> return . T.pack . show $ formNames md

formNames :: MultipartData tag -> ([(Text, Text)], [(Text, Text, Text)])
formNames md =
  ( map (\i -> (iName i, iValue i)) (inputs md)
  , map (\f -> (fdInputName f, fdFileName f, fdFileCType f)) (files md)
  )

roundTrip :: TestName -> Text -> Text -> Text -> Text -> Text -> Text -> TestTree
roundTrip name inputName fileInputName fileName expectedInputName expectedFileInputName expectedFileName =
  testWai echoApp name $ do
    let form = MultipartData
          [Input inputName "value"]
          [FileData fileInputName fileName "text/plain" "contents"]
        expected = MultipartData @Mem
          [Input expectedInputName "value"]
          [FileData expectedFileInputName expectedFileName "text/plain" "contents"]
    body <- liftIO $ renderBody "XX" form
    res <- srequest $ buildRequestWithHeaders POST "/echo" body
      [(hContentType, "multipart/form-data; boundary=XX")]
    assertStatus 200 res
    assertBody (LBS.fromStrict . encodeUtf8 . T.pack . show $ formNames expected) res

renderBody :: LBS.ByteString -> MultipartData Mem -> IO LBS.ByteString
renderBody boundary form =
  case multipartToBody boundary form of
    RequestBodySource src -> do
      chunks <- runExceptT (runSourceT src)
      either fail (return . LBS.concat) chunks
    _ -> fail "multipartToBody did not produce a streaming body"
