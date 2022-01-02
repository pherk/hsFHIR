{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.FHIR.DateSpec where

import Data.FHIR.Interface
import Data.FHIR.Date
import Data.FHIR.Search
import Data.Yaml
import Network.URI
import Ntwo.Data.Interface
import Ntwo.DAO.Types
import Ntwo.Query
import Ntwo.Query.QueryString
import           RIO
import qualified RIO.HashMap    as HM
import           Test.Hspec


spec :: Spec
spec = do
--  r <- runIO $ decodeFileEither "./test/config/search.yml" :: (Either ParseException SearchConfig)
--  case r of
--    Right rc -> do
      describe "year" $ do
        it "2022" $ do
          validateDatePrec "2022" `shouldBe` Just ("2022",4)
-- Error reading config file
--    Left e -> error $ show e



{-
-- |
-- Module:      Data.Aeson.Internal.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Attoparsec.Time.Internal
    (
      TimeOfDay64(..)
    , fromPico
    , toPico
    , diffTimeOfDay64
    , toTimeOfDay64
    ) where

import Prelude.Compat

import Data.Fixed (Fixed(MkFixed), Pico)
import Data.Int (Int64)
import Data.Time (TimeOfDay(..))
import Data.Time.Clock.Compat

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

-}
