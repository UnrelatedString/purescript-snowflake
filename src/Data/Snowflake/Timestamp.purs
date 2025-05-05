module Data.Snowflake.Timestamp
  ( Timestamp(..)
  , discordEpoch
  , toUnix
  , fromUnix
  , toInstant
  , fromInstant
  ) where

import Prelude

import Data.DateTime.Instant as Instant
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), toEnum)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))

-- | Timestamps contained by snowflakes,
-- | which can be inspected to determine the time of the referenced entity's creation,
-- | or created to invent ad-hoc snowflakes for things like Discord API pagination.
newtype Timestamp = Timestamp Int

derive newtype instance eqTimestamp :: Eq Timestamp
derive newtype instance ordTimestamp :: Ord Timestamp
derive instance genericTimestamp :: Generic Timestamp _

instance showTimestamp :: Show Timestamp where
  show = genericShow

instance boundedTimestamp :: Bounded Timestamp where
  bottom = Timestamp 0
  top = Timestamp 0x3ffffffffff

instance enumTimestamp :: Enum Timestamp where
  pred t
    | t == bottom = Nothing
  pred (Timestamp i) = Just $ Timestamp $ i - 1

  succ t
    | t == top = Nothing
  succ (Timestamp i) = Just $ Timestamp $ i + 1

instance boundedEnumTimestamp :: BoundedEnum Timestamp where
  cardinality = Cardinality 0x40000000000
  toEnum i
    | i < 0 = Nothing
    | i > 0x3ffffffffff = Nothing
    | otherwise = Just $ Timestamp i
  fromEnum (Timestamp i) = i

-- | 1420070400000;
-- | the number of milliseconds from the Unix epoch to
-- | the first second of 2015.
discordEpoch :: Int
discordEpoch = 1420070400000

toUnix :: Timestamp -> Milliseconds
toUnix (Timestamp i) = Milliseconds $ toNumber $ i + discordEpoch

fromUnix :: Milliseconds -> Maybe Timestamp
fromUnix (Milliseconds n) = toEnum $ ceil n - discordEpoch

toInstant :: Timestamp -> Maybe Instant.Instant
toInstant = Instant.instant <<< toUnix

fromInstant :: Instant.Instant -> Maybe Timestamp
fromInstant = fromUnix <<< Instant.unInstant
