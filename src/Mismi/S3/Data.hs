{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data (
    Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  , (<\>)
  , (<:/>)
  , (<++>)
  , foldKey
  , parseKey
  , showAddress
  , showKey
  ) where

import           Mismi.S3.Data.Component

import qualified Data.Text as T

import           P


newtype Bucket = Bucket
    { unBucket :: T.Text
    } deriving (Eq, Show)

data Address = Address
    { bucket  :: Bucket
    , key     :: Key
    } deriving (Eq, Show)

-- NOTE: This is not a "safe" data type, and makes no guarantee about what is _actually_ supported for S3
-- https://github.com/ambiata/mismi/issues/2
infixl 5 :/
data Key =
        EmptyKey
    |   Key :/ Component
        deriving (Eq, Show)

showKey :: Key -> T.Text
showKey EmptyKey        = ""
showKey (EmptyKey :/ c) = componentText c
showKey (k :/ c)        = T.intercalate "/" [showKey k, componentText c]

foldKey :: a -> (a -> Component -> a) -> Key -> a
foldKey nil _ EmptyKey = nil
foldKey nil f (k :/ c) = f (foldKey nil f k) c

-- |
-- Right now this doesnt check for and reject invalid characters,
-- We need to wrap `Key` around something else
--
parseKey :: T.Text -> Key
parseKey = foldl' (</>) EmptyKey . parseDelimitedText

showAddress :: Address -> T.Text
showAddress (Address (Bucket b) k) = "s3://" <> b <> "/" <> showKey k


infixl 5 </>
(</>) :: Key -> Component -> Key
(</>) = (:/)

infixr 5 <\>
-- |
-- Lets you cons a component to the beginning of a key,
-- This is kind of inefficent as it means its traversing to the end of the list to put something at the bottom
--
(<\>) :: Component -> Key -> Key
(<\>) c = foldKey (EmptyKey </> c) (</>)

infixl 4 <++>
-- |
-- lets you prepend a key to the front of another
--
(<++>) :: Key -> Key -> Key
(<++>) k = foldKey k (</>)

infixl 5 <:/>
(<:/>) :: Component -> Component -> Key
c1 <:/> c2 = EmptyKey </> c1 </> c2
