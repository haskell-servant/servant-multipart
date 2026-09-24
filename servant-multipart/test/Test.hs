{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

import Data.ByteString           as BS (ByteString)
import Data.ByteString.Lazy      as BSL (ByteString, toStrict)
import qualified Data.ByteString.Lazy as BSL (replicate)
import qualified Data.ByteString.Lazy.Char8 as BSL8 (pack)
import Data.List                 (intersperse)
import Data.Monoid
import Data.Text                 (Text, pack)
import Data.Text.Encoding        (decodeUtf8)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import Network.Wai.Parse         (defaultParseRequestBodyOptions, setMaxRequestFileSize)

import Test.Tasty
import Test.Tasty.Wai

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
  , testGroup "form limits"
      [ testWai testApp "field name too long" testFieldNameTooLong
      , testWai testApp "too many files" testTooManyFiles
      , testWai testApp "too many files with lenient handler" testTooManyFilesLenient
      , testWai testApp "part header line too long" testPartHeaderLineTooLong
      , testWai testApp "too many part header lines" testTooManyPartHeaderLines
      , testWai limitedApp "file under size limit" testFileUnderSizeLimit
      , testWai limitedApp "file over size limit" testFileOverSizeLimit
      ]
  , testGroup "form limits with custom ErrorFormatters"
      [ testWai customFormatterApp "too many files keeps formatter status" testTooManyFilesCustomFormatter
      , testWai customFormatterApp "file over size limit is 413" testFileOverSizeLimitCustomFormatter
      ]
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
  :<|> "blogPostLenient" :> MultipartForm' '[Lenient] Mem BlogPost :> Post '[PlainText] Text
  :<|> "blogPostRaw" :> MultipartForm Mem (MultipartData Mem) :> Post '[PlainText] Text

blogPostStrictHandler :: BlogPost -> Handler Text
blogPostStrictHandler bp = return $ title bp <> "\n" <> body bp

blogPostLenientHandler :: Either CheckError BlogPost -> Handler Text
blogPostLenientHandler eitherBP =
  return $ case eitherBP of
    Left (ParseError msg) -> "parse error: " <> pack msg
    Left (LimitError limit) -> "limit exceeded: " <> pack (limitMessage limit)
    Right bp -> title bp

blogPostRawHandler :: MultipartData Mem -> Handler Text
blogPostRawHandler md =
  return $ mconcat $ intersperse " "
    $ map iName (inputs md) <> map fdInputName (files md)

testServer :: Server TestAPI
testServer = blogPostStrictHandler :<|> blogPostLenientHandler :<|> blogPostRawHandler

testApp :: Application
testApp = serve @TestAPI Proxy testServer

limitedOptions :: MultipartOptions Mem
limitedOptions = (defaultMultipartOptions (Proxy @Mem))
  { generalOptions = setMaxRequestFileSize 100 defaultParseRequestBodyOptions }

limitedApp :: Application
limitedApp = serveWithContext @TestAPI Proxy (limitedOptions :. EmptyContext) testServer

customFormatterApp :: Application
customFormatterApp =
  serveWithContext @TestAPI Proxy (limitedOptions :. customFormatters :. EmptyContext) testServer
  where
    customFormatters = defaultErrorFormatters
      { bodyParserErrorFormatter = \_ _ msg -> err422 { errBody = "custom: " <> BSL8.pack msg } }

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
  assertBody "Foo post" res

testBlogPostLenientHandlerPartialBody :: Session ()
testBlogPostLenientHandlerPartialBody = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostLenient" partialBody multipartHeaders
  assertStatus 200 res
  assertBody "parse error: File body not found" res

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

testFieldNameTooLong :: Session ()
testFieldNameTooLong = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [fieldPart (BSL.replicate 33 0x61)]) multipartHeaders
  assertStatus 400 res
  assertBody "Could not decode multipart mime body: an input name exceeds 32 bytes" res

testTooManyFiles :: Session ()
testTooManyFiles = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" elevenFiles multipartHeaders
  assertStatus 400 res
  assertBody "Could not decode multipart mime body: the form has more than 10 files" res

testTooManyFilesLenient :: Session ()
testTooManyFilesLenient = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostLenient" elevenFiles multipartHeaders
  assertStatus 200 res
  assertBody "limit exceeded: the form has more than 10 files" res

testPartHeaderLineTooLong :: Session ()
testPartHeaderLineTooLong = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [fieldPart (BSL.replicate 9000 0x61)]) multipartHeaders
  assertStatus 431 res
  assertBody "Could not decode multipart mime body: a part header line exceeds the length limit" res

testTooManyPartHeaderLines :: Session ()
testTooManyPartHeaderLines = do
  let manyHeaders = "--XX" : replicate 40 "X-Extra: 1" <> drop 1 (fieldPart "title")
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [manyHeaders]) multipartHeaders
  assertStatus 431 res
  assertBody "Could not decode multipart mime body: a part has too many header lines" res

testFileUnderSizeLimit :: Session ()
testFileUnderSizeLimit = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [filePart "file" (BSL.replicate 20 0x78)]) multipartHeaders
  assertStatus 200 res
  assertBody "file" res

testFileOverSizeLimit :: Session ()
testFileOverSizeLimit = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [filePart "file" (BSL.replicate 200 0x78)]) multipartHeaders
  assertStatus 413 res
  assertBody "Could not decode multipart mime body: the form exceeds a size limit" res

testTooManyFilesCustomFormatter :: Session ()
testTooManyFilesCustomFormatter = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" elevenFiles multipartHeaders
  assertStatus 422 res
  assertBody "custom: the form has more than 10 files" res

testFileOverSizeLimitCustomFormatter :: Session ()
testFileOverSizeLimitCustomFormatter = do
  res <- srequest $ buildRequestWithHeaders POST "/blogPostRaw" (formBody [filePart "file" (BSL.replicate 200 0x78)]) multipartHeaders
  assertStatus 413 res
  assertBody "custom: the form exceeds a size limit" res

elevenFiles :: BSL.ByteString
elevenFiles = formBody (replicate 11 (filePart "file" "contents"))

fieldPart :: BSL.ByteString -> [BSL.ByteString]
fieldPart name =
  [ "--XX"
  , "Content-Disposition: form-data; name=\"" <> name <> "\""
  , ""
  , "value"
  ]

filePart :: BSL.ByteString -> BSL.ByteString -> [BSL.ByteString]
filePart name contents =
  [ "--XX"
  , "Content-Disposition: form-data; name=\"" <> name <> "\"; filename=\"file.txt\""
  , ""
  , contents
  ]

formBody :: [[BSL.ByteString]] -> BSL.ByteString
formBody parts = mconcat $ intersperse "\n" (concat parts <> ["--XX--"])
