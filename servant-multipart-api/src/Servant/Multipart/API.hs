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
-- | @multipart/form-data@ Servant API support for servant.
--   see servant-multipart and servant-multipart-client for server- and client-
--   definitions.
--
--   This is mostly useful for adding file upload support to
--   an API. See haddocks of 'MultipartForm' for an introduction.
module Servant.Multipart.API
  ( MultipartForm
  , MultipartForm'
  , MultipartData(..)
  , ToMultipart(..)
  , FromMultipart(..)
  , MultipartResult
  , Tmp
  , Mem
  , Input(..)
  , FileData(..)
  , lookupInput
  , lookupFile
  , lookupAllInputs
  , lookupAllFiles
  , lookupInputAs
  , lookupAllInputsAs
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.List (find)
import Data.Text (Text, unpack)
import Data.Typeable
import Servant.API

import qualified Data.ByteString.Lazy as LBS

-- | Combinator for specifying a @multipart/form-data@ request
--   body, typically (but not always) issued from an HTML @\<form\>@.
--
--   @multipart/form-data@ can't be made into an ordinary content
--   type for now in servant because it doesn't just decode the
--   request body from some format but also performs IO in the case
--   of writing the uploaded files to disk, e.g in @/tmp@, which is
--   not compatible with servant's vision of a content type as things
--   stand now. This also means that 'MultipartForm' can't be used in
--   conjunction with 'ReqBody' in an endpoint.
--
--   The 'tag' type parameter instructs the function to handle data
--   either as data to be saved to temporary storage ('Tmp') or saved to
--   memory ('Mem').
--
--   The 'a' type parameter represents the Haskell type to which
--   you are going to decode the multipart data to, where the
--   multipart data consists in all the usual form inputs along
--   with the files sent along through @\<input type="file"\>@
--   fields in the form.
--
--   One option provided out of the box by this library is to decode
--   to 'MultipartData'.
--
--   Example:
--
--   @
--   type API = MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String
--
--   api :: Proxy API
--   api = Proxy
--
--   server :: MultipartData Tmp -> Handler String
--   server multipartData = return str
--
--     where str = "The form was submitted with "
--              ++ show nInputs ++ " textual inputs and "
--              ++ show nFiles  ++ " files."
--           nInputs = length (inputs multipartData)
--           nFiles  = length (files multipartData)
--   @
--
--   You can alternatively provide a 'FromMultipart' instance
--   for some type of yours, allowing you to regroup data
--   into a structured form and potentially selecting
--   a subset of the entire form data that was submitted.
--
--   Example, where we only look extract one input, /username/,
--   and one file, where the corresponding input field's /name/
--   attribute was set to /pic/:
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart multipartData =
--       User \<$\> lookupInput "username" multipartData
--            \<*\> fmap fdPayload (lookupFile "pic" multipartData)
--
--   type API = MultipartForm Tmp User :> Post '[PlainText] String
--
--   server :: User -> Handler String
--   server usr = return str
--
--     where str = username usr ++ "'s profile picture"
--              ++ " got temporarily uploaded to "
--              ++ pic usr ++ " and will be removed from there "
--              ++ " after this handler has run."
--   @
--
--   Note that the behavior of this combinator is configurable,
--   by using 'serveWithContext' from servant-server instead of 'serve',
--   which takes an additional 'Context' argument. It simply is an
--   heterogeneous list where you can for example store
--   a value of type 'MultipartOptions' that has the configuration that
--   you want, which would then get picked up by servant-multipart.
--
--   __Important__: as mentioned in the example above,
--   the file paths point to temporary files which get removed
--   after your handler has run, if they are still there. It is
--   therefore recommended to move or copy them somewhere in your
--   handler code if you need to keep the content around.
--
--   __Important__: the input names, input values, file names and file
--   content types of the submitted form must all be valid UTF-8, including
--   those of the parts that your 'FromMultipart' instance ignores. A form
--   that carries any other encoding is rejected with a 400 response before
--   your handler runs, unless 'Servant.API.Modifiers.Lenient' is used. The
--   contents of the uploaded files are not decoded and may be arbitrary
--   bytes.
type MultipartForm tag a = MultipartForm' '[] tag a

-- | 'MultipartForm' which can be modified with 'Servant.API.Modifiers.Lenient'.
--
--   Under 'Servant.API.Modifiers.Lenient', the handler is passed an
--   @'Either' 'String' a@ rather than the request being rejected, so it is
--   handed the message from a failed 'fromMultipart' call, or from a form
--   whose text is not valid UTF-8.
data MultipartForm' (mods :: [*]) tag a

-- | What servant gets out of a @multipart/form-data@ form submission.
--
--   The type parameter 'tag' tells if 'MultipartData' is stored as a
--   temporary file or stored in memory. 'tag' is type of either 'Mem'
--   or 'Tmp'.
--
--   The 'inputs' field contains a list of textual 'Input's, where
--   each input for which a value is provided gets to be in this list,
--   represented by the input name and the input value. See haddocks for
--   'Input'.
--
--   The 'files' field contains a list of files that were sent along with the
--   other inputs in the form. Each file is represented by a value of type
--   'FileData' which among other things contains the path to the temporary file
--   (to be removed when your handler is done running) with a given uploaded
--   file's content. See haddocks for 'FileData'.
data MultipartData tag = MultipartData
  { inputs :: [Input]
  , files  :: [FileData tag]
  }

deriving instance Eq (MultipartResult tag) => Eq (MultipartData tag)
deriving instance Show (MultipartResult tag) => Show (MultipartData tag)

instance Semigroup (MultipartData tag) where
  a <> b =
    MultipartData
      { inputs = inputs a <> inputs b
      , files  = files a <> files b
      }

instance Monoid (MultipartData tag) where
  mempty =
    MultipartData
      { inputs = []
      , files  = []
      }

instance NFData (MultipartResult tag) => NFData (MultipartData tag) where
  rnf (MultipartData is fs) = rnf is `seq` rnf fs

-- | Lookup a textual input with the given @name@ attribute.
--
-- Takes linear time with respect to the number of inputs.
lookupInput :: Text -> MultipartData tag -> Either String Text
lookupInput iname =
  maybe (Left $ "Field " <> unpack iname <> " not found") (Right . iValue)
  . find ((==iname) . iName)
  . inputs

-- | Lookup a file input with the given @name@ attribute.
--
-- Takes linear time with respect to the number of files.
lookupFile :: Text -> MultipartData tag -> Either String (FileData tag)
lookupFile iname =
  maybe (Left $ "File " <> unpack iname <> " not found") Right
  . find ((==iname) . fdInputName)
  . files

-- | Lookup all textual inputs with the given @name@ attribute.
-- 
-- Takes linear time with respect to the number of inputs.
--
-- This function returns a list of all values for inputs with the specified name.
-- It is useful when handling forms that allow multiple inputs with the same name,
-- such as multiple select inputs or checkbox groups with explicit values. 
--
-- Example:
--
-- @
-- let mpd = MultipartData [Input "color" "red", Input "color" "blue"] []
-- lookupAllInputs "color" mpd == ["red", "blue"]
-- lookupAllInputs "size"  mpd == []
-- @
lookupAllInputs :: Text -> MultipartData tag -> [Text]
lookupAllInputs iname mpd = [ val | (Input name val) <- inputs mpd, name == iname ]

-- | Lookup all file inputs with the given @name@ attribute.
--
-- Takes linear time with respect to the number of files.
--
-- This function returns a list of all files uploaded under the specified name.
-- It is useful when handling forms that allow multiple file uploads with the same
-- name, such as file inputs with the @multiple@ attribute. 
--
-- Example:
--
-- @
-- let file1 = FileData "file" "doc1.pdf" "application/pdf" "/tmp/doc1"
--     file2 = FileData "file" "doc2.pdf" "application/pdf" "/tmp/doc2"
--     mpd   = MultipartData [] [file1, file2] :: MultipartData Tmp
-- lookupAllFiles "file"  mpd == [file1, file2]
-- lookupAllFiles "image" mpd == []
-- @
lookupAllFiles :: Text -> MultipartData tag -> [FileData tag]
lookupAllFiles iname mpd = [ f | f <- files mpd, fdInputName f == iname ]

-- | Lookup a textual input with the given @name@ attribute and parse it into the desired type.
--
-- Takes linear time with respect to the number of inputs.
--
-- This function returns the parsed value if the input exists and can be parsed successfully
-- using its 'FromHttpApiData' instance. If the input is not found or parsing fails, it returns
-- an error message.
--
-- Note: This function requires the field to be present in the request. Standalone HTML boolean
-- checkboxes (which submit @"on"@ when checked and are omitted by browsers when unchecked) are
-- not directly supported by 'FromHttpApiData Bool'; check for presence with 'lookupInput' or
-- 'lookupAllInputs' instead, or use a custom newtype with a 'FromHttpApiData' instance.
--
-- Example:
--
-- @
-- let mpd = MultipartData [Input "age" "30"] []
-- lookupInputAs "age"     mpd == Right (30 :: Int)
-- lookupInputAs "isAdmin" mpd == Left "Field isAdmin not found"
-- @
lookupInputAs :: FromHttpApiData a => Text -> MultipartData tag -> Either String a
lookupInputAs iname mpd = do
  val <- lookupInput iname mpd
  first unpack $ parseQueryParam val

-- | Lookup all textual inputs with the given @name@ attribute and parse them into the desired type.
--
-- Takes linear time with respect to the number of inputs.
--
-- This function returns a list of parsed values for inputs with the specified name using their
-- 'FromHttpApiData' instance. It is useful for forms with repeated fields, multiple select inputs,
-- or checkbox groups sharing the same name with explicit values.
--
-- If no inputs are found, an empty list is returned. If parsing fails for any value,
-- an error message is returned.
--
-- Example:
--
-- @
-- let mpd = MultipartData [Input "nums" "1", Input "nums" "2"] []
-- lookupAllInputsAs "nums" mpd == Right [1, 2 :: Int]
-- lookupAllInputsAs "size" mpd == Right ([] :: [Int])
-- @
lookupAllInputsAs :: FromHttpApiData a => Text -> MultipartData tag -> Either String [a]
lookupAllInputsAs iname mpd = do
  let vals = lookupAllInputs iname mpd
  first unpack $ mapM parseQueryParam vals

-- | Representation for an uploaded file, usually resulting from
--   picking a local file for an HTML input that looks like
--   @\<input type="file" name="somefile" /\>@.
data FileData tag = FileData
  { fdInputName :: Text     -- ^ @name@ attribute of the corresponding
                            --   HTML @\<input\>@
  , fdFileName  :: Text     -- ^ name of the file on the client's disk
  , fdFileCType :: Text     -- ^ MIME type for the file
  , fdPayload   :: MultipartResult tag
                            -- ^ path to the temporary file that has the
                            --   content of the user's original file. Only
                            --   valid during the execution of your handler as
                            --   it gets removed right after, which means you
                            --   really want to move or copy it in your handler.
  }

deriving instance Eq (MultipartResult tag) => Eq (FileData tag)
deriving instance Show (MultipartResult tag) => Show (FileData tag)

-- | Note that at 'Tmp' this only forces the 'FilePath'. It makes no
--   guarantees about the temporary file it names, which is still removed
--   once the handler has run.
instance NFData (MultipartResult tag) => NFData (FileData tag) where
  rnf (FileData iname fname ctype payload) =
    rnf iname `seq` rnf fname `seq` rnf ctype `seq` rnf payload

-- | Representation for a textual input (any @\<input\>@ type but @file@).
--
--   @\<input name="foo" value="bar"\ />@ would appear as @'Input' "foo" "bar"@.
data Input = Input
  { iName  :: Text -- ^ @name@ attribute of the input
  , iValue :: Text -- ^ value given for that input
  } deriving (Eq, Show)

instance NFData Input where
  rnf (Input name value) = rnf name `seq` rnf value

-- | 'MultipartData' is the type representing
--   @multipart/form-data@ form inputs. Sometimes
--   you may instead want to work with a more structured type
--   of yours that potentially selects only a fraction of
--   the data that was submitted, or just reshapes it to make
--   it easier to work with. The 'FromMultipart' class is exactly
--   what allows you to tell servant how to turn "raw" multipart
--   data into a value of your nicer type.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance FromMultipart Tmp User where
--     fromMultipart form =
--       User \<$\> lookupInput "username" (inputs form)
--            \<*\> fmap fdPayload (lookupFile "pic" $ files form)
--   @
class FromMultipart tag a where
  -- | Given a value of type 'MultipartData', which consists
  --   in a list of textual inputs and another list for
  --   files, try to extract a value of type @a@. When
  --   extraction fails, servant errors out with status code 400.
  fromMultipart :: MultipartData tag -> Either String a

instance FromMultipart tag (MultipartData tag) where
  fromMultipart = Right

-- | Allows you to tell servant how to turn a more structured type
--   into a 'MultipartData', which is what is actually sent by the
--   client.
--
--   @
--   data User = User { username :: Text, pic :: FilePath }
--
--   instance ToMultipart Tmp User where
--       toMultipart user = MultipartData [Input "username" $ username user]
--                                        [FileData "pic"
--                                                  (pic user)
--                                                  "image/png"
--                                                  (pic user)
--                                        ]
--   @
class ToMultipart tag a where
  -- | Given a value of type 'a', convert it to a
  -- 'MultipartData'.
  toMultipart :: a -> MultipartData tag

instance ToMultipart tag (MultipartData tag) where
  toMultipart = id

-- | Tag for data stored as a temporary file
data Tmp

-- | Tag for data stored in memory
data Mem

type family MultipartResult tag :: *
type instance MultipartResult Tmp = FilePath
type instance MultipartResult Mem = LBS.ByteString

instance HasLink sub => HasLink (MultipartForm tag a :> sub) where
  type MkLink (MultipartForm tag a :> sub) r = MkLink sub r
  toLink toA _ = toLink toA (Proxy :: Proxy sub)
