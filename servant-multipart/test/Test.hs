{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

import Data.ByteString           as BS (ByteString)
import Data.ByteString.Lazy      as BSL (ByteString, toStrict)
import Data.List                 (intersperse)
import Data.Monoid
import Data.Text                 (Text)
import Data.Text.Encoding        (decodeUtf8)
import Network.HTTP.Types.Header (HeaderName, hContentType)

import Test.Tasty
import Test.Tasty.Wai
import Test.Tasty.HUnit

import Servant
import Servant.Multipart

main :: IO ()
main = defaultMain $ testGroup "servant-multipart"
  [ testGroup "strict handler with FromMultipart"
      [ testWai testApp "correct body" testBlogPostStrictHandler
      , testWai testApp "empty body" testBlogPostStrictHandlerEmptyBody
      , testWai testApp "partial body" testBlogPostStrictHandlerPartialBody
      ]
  , testGroup "lenient handler with FromMultipart"
      [ testWai testApp "correct body" testBlogPostLenientHandler
      , testWai testApp "partial body" testBlogPostLenientHandlerPartialBody
      ]
  , testGroup "strict handler with raw MultipartData"
      [ testWai testApp "correct body" testBlogPostRawHandler
      ]
  , lookupTests
  ]

sampleFileData :: FileData Tmp
sampleFileData = FileData "file" "doc1.pdf" "application/pdf" "payload1"

lookupTests :: TestTree
lookupTests = testGroup "Lookup function tests"
  [ testCase "lookupAllInputs - found" $ do
       let md = MultipartData [ Input "color" "red"
                              , Input "color" "blue"
                              ] []
       lookupAllInputs "color" md @?= ["red", "blue"]

  , testCase "lookupAllInputs - missing field" $ do
       let md = MultipartData [ Input "color" "red"
                              , Input "color" "blue"
                              ] []
       lookupAllInputs "size" md @?= []

  , testCase "lookupAllFiles - found" $ do
       let file1 = sampleFileData
           file2 = FileData "file" "doc2.pdf" "application/pdf" "/tmp/doc2.buf" :: FileData Tmp
           md = MultipartData [] [file1, file2]
       lookupAllFiles "file" md @?= [file1, file2]

  , testCase "lookupAllFiles - missing file" $ do
       let file1 = sampleFileData
           md = MultipartData [] [file1]
       length (lookupAllFiles "image" md) @?= 0

  , testCase "lookupInputAs - parsed successfully" $ do
       let md = MultipartData [ Input "age" "30" ] []
       lookupInputAs @Int "age" md @?= Right 30

  , testCase "lookupInputAs - missing input" $ do
       let md = MultipartData [ Input "age" "30" ] []
       case lookupInputAs @Bool "isAdmin" md of
         Left err -> err @?= "Field isAdmin not found"
         Right _  -> assertFailure "Expected failure on missing input"

  , testCase "lookupAllInputsAs - parsing list of numbers" $ do
       let md = MultipartData [ Input "nums" "1"
                              , Input "nums" "2"
                              , Input "nums" "3"
                              ] []
       lookupAllInputsAs @Int "nums" md @?= Right [1,2,3]
  ]

data BlogPost
  = BlogPost
      { title :: Text
      , body  :: Text
      }

instance FromMultipart Mem BlogPost where
  fromMultipart md =
    BlogPost
      <$> lookupInput "title" md
      <*> fmap (decodeUtf8 . BSL.toStrict . fdPayload) (lookupFile "body" md)

type TestAPI
  =    "blogPostStrict" :> MultipartForm Mem BlogPost :> Post '[PlainText] Text
  :<|> "blogPostLenient" :> MultipartForm' '[Lenient] Mem BlogPost :> Post '[JSON] Bool
  :<|> "blogPostRaw" :> MultipartForm Mem (MultipartData Mem) :> Post '[PlainText] Text

blogPostStrictHandler :: BlogPost -> Handler Text
blogPostStrictHandler bp = return $ title bp <> "\n" <> body bp

blogPostLenientHandler :: Either String BlogPost -> Handler Bool
blogPostLenientHandler eitherBP =
  case eitherBP of
    Left _  -> return False
    Right _ -> return True

blogPostRawHandler :: MultipartData Mem -> Handler Text
blogPostRawHandler md =
  return $ mconcat $ intersperse " "
    $ map iName (inputs md) <> map fdInputName (files md)

testApp :: Application
testApp = serve @TestAPI Proxy $ blogPostStrictHandler :<|> blogPostLenientHandler :<|> blogPostRawHandler

multipartHeaders :: [(HeaderName, BS.ByteString)]
multipartHeaders = [(hContentType, "multipart/form-data; boundary=XX")]

testBlogPostStrictHandler :: Session ()
testBlogPostStrictHandler = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostStrict" correctBody multipartHeaders
  assertStatus 200 res
  assertBody "Foo post\nFoo body\n" res

testBlogPostStrictHandlerEmptyBody :: Session ()
testBlogPostStrictHandlerEmptyBody = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostStrict" "" multipartHeaders
  assertStatus 400 res
  assertBody "Could not decode multipart mime body: Field title not found" res

testBlogPostStrictHandlerPartialBody :: Session ()
testBlogPostStrictHandlerPartialBody = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostStrict" partialBody multipartHeaders
  assertStatus 400 res
  assertBody "Could not decode multipart mime body: File body not found" res

testBlogPostLenientHandler :: Session ()
testBlogPostLenientHandler = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostLenient" correctBody multipartHeaders
  assertStatus 200 res
  assertBody "true" res

testBlogPostLenientHandlerPartialBody :: Session ()
testBlogPostLenientHandlerPartialBody = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostLenient" partialBody multipartHeaders
  assertStatus 200 res
  assertBody "false" res

testBlogPostRawHandler :: Session ()
testBlogPostRawHandler = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" correctBody multipartHeaders
  assertStatus 200 res
  assertBody "title body" res

correctBody :: BSL.ByteString
correctBody = mconcat $ intersperse "\n"
  [ "--XX"
  , "Content-Disposition: form-data; name=\"title\""
  , ""
  , "Foo post"
  , "--XX"
  , "Content-Disposition: form-data; name=\"body\"; filename=\"body.md\""
  , ""
  , "Foo body"
  , ""
  , "--XX--"
  ]

partialBody :: BSL.ByteString
partialBody = mconcat $ intersperse "\n"
  [ "--XX"
  , "Content-Disposition: form-data; name=\"title\""
  , ""
  , "Foo post"
  , ""
  , "--XX--"
  ]
