{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.FHIR.Model where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics                  ( Generic )
import           RIO
import qualified RIO.Map                       as M
import           RIO.Map                       ( Map )

data ApiUsage = ApiUsage
  { timestamp :: String
  , usage :: Int
  } deriving (Generic, Show)

-- Represents expiration of cached keys in seconds
newtype Expiration = Expiration { getExpiration :: Integer } deriving Show

instance FromJSON ApiUsage
