module Data.Snowflake
  ( class Snowflake
  , toBase10
  , fromBase10
  , toTimestamp
  , fromTimestamp
  , StringSnowflake
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), toEnum, fromEnum)
import Data.Foldable (any)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)
import Data.Int (toNumber, ceil, fromString, pow)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodePoints (toCodePointArray, codePointFromChar, fromCodePointArray)
import Data.String.CodePoints as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import JS.BigInt as BigInt -- just change this to a second Data.Int import for Purerl :p
import Partial.Unsafe (unsafePartial)

import Data.Snowflake.Timestamp (Timestamp(..))

-- | A class for types which represent Discord "snowflake" IDs,
-- | which are inherently 64-bit integers and can be accessed as such if
-- | interfacing with the API through Erlang's External Term Format
-- | but are provided serialized as base 10 strings
-- | if interfacting with the API through JSON
-- | and are additionally used in base 10 representation in messages.

class Snowflake a where
  toBase10 :: a -> String
  fromBase10 :: String -> Maybe a
  toTimestamp :: a -> Timestamp
  -- | Should have non-timestamp bits zeroed.
  fromTimestamp :: Timestamp -> a

shift :: BigInt.BigInt
shift = BigInt.fromInt 22

-- | A universal string representation of snowflakes,
-- | for use with API endpoints which serve or receive JSON
-- | and natively use strings as is because 64 bit integers
-- | are outside the safe integer limit for doubles (as used by JS).

newtype StringSnowflake = StringSnowflake String

derive newtype instance stringSnowflakeEq :: Eq StringSnowflake
derive newtype instance stringSnowflakeOrd :: Ord StringSnowflake
derive instance stringSnowflakeShow :: Show StringSnowflake

instance Snowflake StringSnowflake where
  toBase10 (StringSnowflake s) = s

  -- ...reminds me I kinda want to actually make that safe regex library now
  fromBase10 s
    | any
      (\c -> c < codePointFromChar '0' || c > codePointFromChar '9')
      (toCodePointArray s)
      = Nothing
    | fromCodePointArray (Array.reverse $ toCodePointArray s) >= "61615590737044764481"
      = Nothing
    | otherwise = Just s

  -- | Will error at runtime if given a malformed snowflake!
  -- | Only construct snowflakes from trusted foreign data or
  -- | through `fromBase10`'s validation.
  toTimestamp (StringSnowflake s) = unsafePartial $ fromJust do
    value <- BigInt.fromString s
    let timestampBits = BigInt.shr value shift
    timestampInt <- BigInt.toInt timestampBits
    pure $ Timestamp timestampBits
  
  fromTimestamp (Timestamp t)
    = StringSnowflake
    $ BigInt.toString
    $ flip BigInt.shl shift
    $ BigInt.fromInt t
