{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.N1QL.SearchConfigSpec where

import Data.FHIR.Interface
import Data.FHIR.Search
import Data.Yaml
import Language.N1QL.SearchConfig
import Ntwo.Data.Interface 
import Ntwo.DAO.Types
import Ntwo.Query
import Ntwo.Query.QueryString
import           RIO
import qualified RIO.HashMap    as HM
import           Test.Hspec

spec :: Spec
spec = do
  r <- runIO $ decodeFileEither "./test/config/search.yml" 
  case r of
    Right rc -> do
      describe "Config" $ do
        it "read" $ do
                rc `shouldBe` testSC
    Left e -> error $ show e
