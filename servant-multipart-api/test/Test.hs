{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Servant.Multipart.API

main :: IO ()
main = defaultMain lookupTests

sampleFileData :: FileData Tmp
sampleFileData = FileData "file" "doc1.pdf" "application/pdf" "payload1"

lookupTests :: TestTree
lookupTests = testGroup "Lookup function tests"
  [ testCase "lookupInput - found" $ do
       let md = MultipartData [ Input "title" "hello" ] []
       lookupInput "title" md @?= Right "hello"

  , testCase "lookupInput - missing" $ do
       let md = MultipartData [ Input "title" "hello" ] []
       lookupInput "author" md @?= Left "Field author not found"

  , testCase "lookupFile - found" $ do
       let md = MultipartData [] [ sampleFileData ]
       lookupFile "file" md @?= Right sampleFileData

  , testCase "lookupFile - missing" $ do
       let md = MultipartData [] [ sampleFileData ]
       lookupFile "image" md @?= Left "File image not found"

  , testCase "lookupAllInputs - found" $ do
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
       lookupAllFiles "image" md @?= []

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

  , testCase "lookupAllInputsAs - missing field returns empty list" $ do
       let md = MultipartData [ Input "nums" "1" ] []
       lookupAllInputsAs @Int "other" md @?= Right []
  ]
