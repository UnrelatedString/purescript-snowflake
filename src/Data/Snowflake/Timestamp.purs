module Data.Snowflake.Timestamp
  ( Timestamp
  , discordEpoch
  , toMillisecondsFromEpoch
  , fromMillisecondsFromEpoch
  , unsafeFromMilliseconds
  , toUnix
  , fromUnix
  , toInstant
  , fromInstant
  ) where

import Prelude

import Data.DateTime.Instant as Instant
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..), negateDuration)

-- | Timestamps contained by snowflakes,
-- | which can be inspected to determine the time of the referenced entity's creation,
-- | or created to invent ad-hoc snowflakes for things like Discord API pagination.
-- |
-- | (Should always be an integer and within the safe integer bounds for double precision,
-- | but uses an underlying `Number` because `Int` is restricted to 32 bits.)
newtype Timestamp = Timestamp Milliseconds

derive newtype instance eqTimestamp :: Eq Timestamp
derive newtype instance ordTimestamp :: Ord Timestamp

instance showTimestamp :: Show Timestamp where
  show (Timestamp m) = "(Timestamp " <> show m <> ")"

instance boundedTimestamp :: Bounded Timestamp where
  bottom = Timestamp $ Milliseconds 0.0
  top = Timestamp $ Milliseconds 4398046511103.0

-- | In obedience to the non-skipping laws, and for of general coherence
-- | with whatever API you're using snowflakes with,
-- | non-integral `Timestamp`s should never be constructed.
-- | Note that no `BoundedEnum` instance is supplied, because
-- | timestamps are decidedly not "small" --
-- | they can't even be guaranteed to fit in an `Int`,
-- | so there's no sense in defining `fromEnum`!
instance enumTimestamp :: Enum Timestamp where
  pred t
    | t == bottom = Nothing
  pred (Timestamp i) = Just $ Timestamp $ i <> Milliseconds (-1.0)

  succ t
    | t == top = Nothing
  succ (Timestamp i) = Just $ Timestamp $ i <> Milliseconds 1.0

-- | 1420070400000;
-- | the number of milliseconds from the Unix epoch to
-- | the first second of 2015.
discordEpoch :: Milliseconds
discordEpoch = Milliseconds 1420070400000.0

toMillisecondsFromEpoch :: Timestamp -> Milliseconds
toMillisecondsFromEpoch (Timestamp i) = i

fromMillisecondsFromEpoch :: Milliseconds -> Maybe Timestamp
fromMillisecondsFromEpoch m@(Milliseconds n)
  | Number.remainder n 1.0 /= 0.0 = Nothing
  | m < toMillisecondsFromEpoch bottom = Nothing
  | m > toMillisecondsFromEpoch top = Nothing
  | otherwise = Just $ Timestamp m

-- Performs no range validation and does not check that the duration is integral!
unsafeFromMilliseconds :: Milliseconds -> Timestamp
unsafeFromMilliseconds = Timestamp

toUnix :: Timestamp -> Milliseconds
toUnix (Timestamp i) = i <> discordEpoch

fromUnix :: Milliseconds -> Maybe Timestamp
fromUnix m = fromMillisecondsFromEpoch $ m <> negateDuration discordEpoch

toInstant :: Timestamp -> Maybe Instant.Instant
toInstant = Instant.instant <<< toUnix

fromInstant :: Instant.Instant -> Maybe Timestamp
fromInstant = fromUnix <<< Instant.unInstant
