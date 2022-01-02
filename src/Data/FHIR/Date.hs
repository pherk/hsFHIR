{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Date
-- Copyright   : (c) 2021 Peter Herkenrath
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Peter Herkenrath <p.herkenrath@uni-koeln.de>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Data.FHIR.Date where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Attoparsec.Text as A
import Data.Bits ((.&.))
import Data.Char (isDigit, ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Maybe
import Data.Time.Calendar (Day, fromGregorianValid, showGregorian)
import Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..), fromYearQuarter)
import Data.Time.Calendar.Month.Compat (Month, fromYearMonthValid)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Local
import Data.Fixed (Fixed(MkFixed), Pico)
import Data.Time (TimeOfDay(..))
import Data.Time.Clock.Compat

import RIO
import Prelude (read)

data SDate 
  = PartialDate Text (Maybe Text) (Maybe Text)
  | FullDate Local.ZonedTime
  | LocalDate Local.LocalTime

instance Show SDate where
  show (PartialDate  y  Nothing  Nothing) = T.unpack y
  show (PartialDate  y  (Just m) Nothing) = T.unpack (y<>"-"<>m)
  show (PartialDate  y  Nothing  (Just d)) = error "Invalid IsoDate"
  show (PartialDate  y  (Just m) (Just d)) = T.unpack (y<>"-"<>m<>"-"<>d)
  show (FullDate (Local.ZonedTime (Local.LocalTime d t) tz))  = showGregorian d ++ "T" ++ show t ++ show tz
  show (LocalDate (Local.LocalTime d t))   = showGregorian d ++ "T" ++ show t


--
-- checks if year present 
-- assumes valid (partial) DateTime
--
validateDatePrec :: Text -> Maybe (Text, Int)
validateDatePrec s = do
  let id = A.parseOnly (isoDateP <* endOfInput) s
  case id of
    Right (Just d) -> Just (T.pack (show d), datePrec d)
    _              -> Nothing

datePrec (PartialDate _ Nothing  Nothing) = 4
datePrec (PartialDate _ (Just _) Nothing) = 6
datePrec (PartialDate _ Nothing  (Just _)) = 0
datePrec (PartialDate _ (Just _) (Just _)) = 8
datePrec (FullDate _ ) = 14
datePrec (LocalDate _ ) = 14

isoDateP :: A.Parser (Maybe SDate)
isoDateP = (Just <$> FullDate <$> zonedTime) <|> (Just <$> LocalDate <$> localTime) <|> partialDateP <|> pure Nothing

partialDateP :: A.Parser (Maybe SDate)
partialDateP = do
  y  <- A.count 4 A.digit
  m  <- A.option 0 (A.char '-' *> twoDigits)
  d  <- A.option 0 (A.char '-' *> twoDigits)
  return $ if isValidYear y && (m > 0 && m < 13) && isValidDay m d 
           then Just $ PartialDate (T.pack y) (toText m) (toText d)
           else if isValidYear y && (m > 0 && m < 13) then Just $ PartialDate (T.pack y) (toText m) Nothing 
           else if isValidYear y then Just $ PartialDate (T.pack y) Nothing Nothing 
           else Nothing
  where toText d = if d < 10 then Just ("0" <> (T.pack (show d))) else Just (T.pack (show d))

isValidYear :: String -> Bool
isValidYear y = y' > 1900 && y' < 2100
  where y' = read  y

isValidDay :: Int -> Int -> Bool
isValidDay m d = 
  case m of
    1 -> d > 0 && d < 32
    2 -> d > 0 && d < 30
    3 -> d > 0 && d < 32
    4 -> d > 0 && d < 31
    5 -> d > 0 && d < 32
    6 -> d > 0 && d < 31
    7 -> d > 0 && d < 32
    8 -> d > 0 && d < 32
    9 -> d > 0 && d < 31
    10 -> d > 0 && d < 32
    11 -> d > 0 && d < 31
    12 -> d > 0 && d < 32
    _  -> False

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Parser Day
day = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  m <- (twoDigits <* char '-') <|> fail "date must be of form [+,-]YYYY-MM-DD"
  d <- twoDigits <|> fail "date must be of form [+,-]YYYY-MM-DD"
  maybe (fail "invalid date") return (fromGregorianValid (absOrNeg y) m d)

-- | Parse a month of the form @[+,-]YYYY-MM@.
month :: Parser Month
month = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "month must be of form [+,-]YYYY-MM"
  m <- twoDigits <|> fail "month must be of form [+,-]YYYY-MM"
  maybe (fail "invalid month") return (fromYearMonthValid (absOrNeg y) m)

-- | Parse a quarter of the form @[+,-]YYYY-QN@.
quarter :: Parser Quarter
quarter = do
  absOrNeg <- negate <$ char '-' <|> id <$ char '+' <|> pure id
  y <- (decimal <* char '-') <|> fail "month must be of form [+,-]YYYY-MM"
  _ <- char 'q' <|> char 'Q'
  q <- parseQ
  return $! fromYearQuarter (absOrNeg y) q
  where
    parseQ = Q1 <$ char '1'
      <|> Q2 <$ char '2'
      <|> Q3 <$ char '3'
      <|> Q4 <$ char '4'


-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b


-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits
  m <- char ':' *> twoDigits
  s <- option 0 (char ':' *> seconds)
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos real t
    _ -> return $! fromIntegral real
 where
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where T n t'  = T.foldl' step (T 12 (fromIntegral a0)) t
          step ma@(T m a) c
              | m <= 0    = ma
              | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
             Just ':'           -> anyChar *> twoDigits
             Just d | isDigit d -> twoDigits
             _                  -> return 0
      let off | ch == '-' = negate off0
              | otherwise = off0
          off0 = h * 60 + m
      case undefined of
        _   | off == 0 ->
              return Nothing
            | off < -720 || off > 840 || m > 59 ->
              fail "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@.  The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Parser Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == 'T' || c == ' ')

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: Parser UTCTime
utcTime = do
  lt@(Local.LocalTime d t) <- localTime
  mtz <- timeZone
  case mtz of
    Nothing -> let !tt = Local.timeOfDayToTime t
               in return (UTCTime d tt)
    Just tz -> return $! Local.localTimeToUTC tz lt

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Parser Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

-- | Like TimeOfDay, but using a fixed-width integer for seconds.
data TimeOfDay64 = TOD {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int64

posixDayLength :: DiffTime
posixDayLength = 86400

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
diffTimeOfDay64 t
  | t >= posixDayLength = TOD 23 59 (60000000000000 + pico (t - posixDayLength))
  | otherwise = TOD (fromIntegral h) (fromIntegral m) s
    where (h,mp) = pico t `quotRem` 3600000000000000
          (m,s)  = mp `quotRem` 60000000000000
          pico   = fromIntegral . diffTimeToPicoseconds

toTimeOfDay64 :: TimeOfDay -> TimeOfDay64
toTimeOfDay64 (TimeOfDay h m s) = TOD h m (fromIntegral (fromPico s))
