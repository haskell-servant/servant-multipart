{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
-- | @multipart/form-data@ client-side support for servant.
--   See servant-multipart-api for the API definitions.
module Servant.Multipart.Client
  ( genBoundary
  , ToMultipart(..)
  , multipartToBody
  ) where

import Servant.Multipart.API

import Control.Monad (replicateM)
import Data.Array (listArray, (!))
import Data.List (foldl')
import Data.List.Compat (singleton)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text.Encoding           (encodeUtf8)
import Data.Typeable
import Network.HTTP.Media.MediaType ((//), (/:))
import Servant.API
import Servant.Client.Core          (HasClient (..), RequestBody (RequestBodySource),
                                     setRequestBody)
import Servant.Types.SourceT        (SourceT (..), StepT (..), fromActionStep, source)
import System.IO                    (IOMode (ReadMode), withFile)
import System.Random                (getStdRandom, randomR)

import qualified Data.ByteString.Lazy as LBS

-- | Upon seeing @MultipartForm a :> ...@ in an API type,
--   servant-client will take a parameter of type @(LBS.ByteString, a)@,
--   where the bytestring is the boundary to use (see 'genBoundary'), and
--   replace the request body with the contents of the form.
instance (ToMultipart tag a, HasClient m api, MultipartClient tag)
      => HasClient m (MultipartForm' mods tag a :> api) where

  type Client m (MultipartForm' mods tag a :> api) =
    (LBS.ByteString, a) -> Client m api

  clientWithRoute pm _ req (boundary, param) =
      clientWithRoute pm (Proxy @api) $ setRequestBody newBody newMedia req
    where
      newBody = multipartToBody boundary $ toMultipart @tag param
      newMedia = "multipart" // "form-data" /: ("boundary", LBS.toStrict boundary)

  hoistClientMonad pm _ f cl = \a ->
      hoistClientMonad pm (Proxy @api) f (cl a)

class MultipartClient tag where
    loadFile :: Proxy tag -> MultipartResult tag -> SourceIO LBS.ByteString

instance MultipartClient Tmp where
    -- streams the file from disk
    loadFile _ fp =
        SourceT $ \k ->
        withFile fp ReadMode $ \hdl ->
        k (readHandle hdl)
      where
        readHandle hdl = fromActionStep LBS.null (LBS.hGet hdl 4096)

instance MultipartClient Mem where
    loadFile _ = source . singleton

-- | Generates a boundary to be used to separate parts of the multipart.
-- Requires 'IO' because it is randomized.
genBoundary :: IO LBS.ByteString
genBoundary = LBS.pack
            . map (validChars !)
            <$> indices
  where
    -- the standard allows up to 70 chars, but most implementations seem to be
    -- in the range of 40-60, so we pick 55
    indices = replicateM 55 . getStdRandom $ randomR (0,61)
    -- Following Chromium on this one:
    -- > The RFC 2046 spec says the alphanumeric characters plus the
    -- > following characters are legal for boundaries:  '()+_,-./:=?
    -- > However the following characters, though legal, cause some sites
    -- > to fail: (),./:=+
    -- https://github.com/chromium/chromium/blob/6efa1184771ace08f3e2162b0255c93526d1750d/net/base/mime_util.cc#L662-L670
    validChars = listArray (0 :: Int, 61)
                           -- 0-9
                           [ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37
                           , 0x38, 0x39, 0x41, 0x42
                           -- A-Z, a-z
                           , 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a
                           , 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52
                           , 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a
                           , 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68
                           , 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70
                           , 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78
                           , 0x79, 0x7a
                           ]

-- | Given a bytestring for the boundary, turns a `MultipartData` into
-- a 'RequestBody'
multipartToBody :: forall tag
                .  MultipartClient tag
                => LBS.ByteString
                -> MultipartData tag
                -> RequestBody
multipartToBody boundary mp = RequestBodySource $ files' <> source ["--", boundary, "--"]
  where
    -- at time of writing no Semigroup or Monoid instance exists for SourceT and StepT
    -- in releases of Servant; they are in master though
    (SourceT l) `mappend'` (SourceT r) = SourceT $ \k ->
                                                   l $ \lstep ->
                                                   r $ \rstep ->
                                                   k (appendStep lstep rstep)
    appendStep Stop        r = r
    appendStep (Error err) _ = Error err
    appendStep (Skip s)    r = appendStep s r
    appendStep (Yield x s) r = Yield x (appendStep s r)
    appendStep (Effect ms) r = Effect $ (flip appendStep r <$> ms)
    mempty' = SourceT ($ Stop)
    crlf = "\r\n"
    lencode = LBS.fromStrict . encodeUtf8
    renderInput input = renderPart (lencode . iName $ input)
                                   "text/plain"
                                   ""
                                   (source . singleton . lencode . iValue $ input)
    inputs' = foldl' (\acc x -> acc `mappend'` renderInput x) mempty' (inputs mp)
    renderFile :: FileData tag -> SourceIO LBS.ByteString
    renderFile file = renderPart (lencode . fdInputName $ file)
                                 (lencode . fdFileCType $ file)
                                 ((flip mappend) "\"" . mappend "; filename=\""
                                                      . lencode
                                                      . fdFileName $ file)
                                 (loadFile (Proxy @tag) . fdPayload $ file)
    files' = foldl' (\acc x -> acc `mappend'` renderFile x) inputs' (files mp)
    renderPart name contentType extraParams payload =
      source [ "--"
             , boundary
             , crlf
             , "Content-Disposition: form-data; name=\""
             , name
             , "\""
             , extraParams
             , crlf
             , "Content-Type: "
             , contentType
             , crlf
             , crlf
             ] `mappend'` payload `mappend'` source [crlf]
