module Distortion.Snowflake
  ( class Snowflake
  , toBase10
  , fromBase10
  , toTimestamp
  , fromTimestamp
  , SnowflakeTimestamp
  , discordEpoch
  , toUnix
  , fromUnix
  , toInstant
  , fromInstant
  , StringSnowflake
  ) where

import Prelude

import Data.DateTime.Instant as Instant
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), toEnum)
import Data.Foldable (any)
import Data.Int (toNumber, ceil, fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodePoints (toCodePointArray, codePointFromChar)
import Data.String.CodePoints as String
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)

-- | A class for types which represent Discord "snowflake" IDs,
-- | which are inherently 64-bit integers and can be accessed as such if
-- | interfacing with the API through Erlang's External Term Format
-- | but are provided serialized as base 10 strings
-- | if interfacting with the API through JSON
-- | and are additionally used in base 10 representation in messages.

class Snowflake a where
  toBase10 :: a -> String
  fromBase10 :: String -> Maybe a
  toTimestamp :: a -> SnowflakeTimestamp
  -- | Should have non-timestamp bits zeroed.
  fromTimestamp :: SnowflakeTimestamp -> a

-- | Milliseconds since the Discord Epoch, as a 42-bit unsigned integer.
-- | (This is well within the safe limits for doubles.)
newtype SnowflakeTimestamp = SnowflakeTimestamp Int

derive newtype instance eqTimestamp :: Eq SnowflakeTimestamp
derive newtype instance ordTimestamp :: Ord SnowflakeTimestamp
derive instance showTimestamp :: Show SnowflakeTimestamp

instance Bounded SnowflakeTimestamp where
  bottom = SnowflakeTimestamp 0
  top = SnowflakeTimestamp 0x3ffffffffff

instance Enum SnowflakeTimestamp where
  pred t
    | t == bottom = Nothing
  pred (SnowflakeTimestamp i) = Just $ SnowflakeTimestamp $ i - 1

  succ t
    | t == top = Nothing
  succ (SnowflakeTimestamp i) = Just $ SnowflakeTimestamp $ i + 1

instance BoundedEnum SnowflakeTimestamp where
  cardinality = Cardinality 0x40000000000
  toEnum i
    i < 0 = Nothing
    i > 0x3ffffffffff = Nothing
    otherwise = Just $ SnowflakeTimestamp i
  fromEnum (SnowflakeTimestamp i) = i

-- | 1420070400000;
-- | the number of milliseconds from the Unix epoch to
-- | the first second of 2015.
discordEpoch :: Int
discordEpoch = 1420070400000

toUnix :: SnowflakeTimestamp -> Milliseconds
toUnix (SnowflakeTimestamp i) = Milliseconds $ toNumber $ i + discordEpoch

fromUnix :: Milliseconds -> Maybe SnowflakeTimestamp
fromUnix (Milliseconds n) = toEnum $ ceil n - discordEpoch

toInstant :: SnowflakeTimestamp -> Maybe Instant.Instant
toInstant = Instant.instant <<< toUnix

fromInstant :: Instant.Instant -> Maybe SnowflakeTimestamp
fromInstant = fromUnix <<< Instant.uninstant


-- | A universal string representation of snowflakes,
-- | for use by JSON-based runners.

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
    | String.dropWhile (_ == codePointFromChar '0') s > "4398046511103" = Nothing
    | otherwise = Just s

  -- could just do this with bigints... but 1. portability and 2. sunk cost :p
  toTimestamp (StringSnowflake s) = unsafePartial $ fromJust do
    let splitPlace = 10 -- kinda arbitrary; just has to fit in JS number
    let { before: high, after: low } = String.splitAt splitPlace
