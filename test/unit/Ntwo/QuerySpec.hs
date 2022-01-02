{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Ntwo.QuerySpec where

import Data.FHIR.Interface
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
  r <- runIO $ decodeFileEither "./test/config/search.yml"
  case r of
    Right rc -> do
      describe "parseKey" $ do
        it "token" $ do
            parseKey "name" `shouldBe` Key (Just "name")
        it "token-token" $ do
            parseKey "name-use" `shouldBe` Key (Just "name-use")
        it "token:mod" $ do
            parseKey "name:missing" `shouldBe` KeyMod "name" (Just "missing")
      describe "parse illegal key" $ do
        it "token:" $ do
            parseKey "name:" `shouldBe` KeyMod "name" Nothing
        it ": only" $ do
            parseKey ":" `shouldBe` KeyMod ""  Nothing
        it ":token" $ do
            parseKey ":mod" `shouldBe` KeyMod "" (Just "mod")
      describe "parseValue" $ do
        it "=token" $ do
            parseValue "=token" `shouldBe` (QValue (Just "token"))
        it "=token1,token2" $ do
            parseValue "=token1,token2" `shouldBe` (QValue (Just "token1,token2"))
        it "=_token" $ do
            parseValue "=_token" `shouldBe` (QValue (Just "_token"))
        it "=boolean" $ do
            parseValue "=true" `shouldBe` (QValue (Just "true"))
        it "=number" $ do
            parseValue "=20200201" `shouldBe` (QValue (Just "20200201"))
        it "=date" $ do
            parseValue "=2020-02-01" `shouldBe` (QValue (Just "2020-02-01"))
        it "=datetime" $ do
            parseValue "=2020-02-01T12:30:00" `shouldBe` (QValue (Just "2020-02-01T12:30:00"))
        it "=quantity" $ do
            parseValue "=20||mg" `shouldBe` (QValue (Just "20||mg"))
        it "=reference" $ do
            parseValue "=nabu/patients/123" `shouldBe` (QValue (Just "nabu/patients/123"))
      describe "parse kv" $ do
        it "token=token" $ do
            breakPair "name=Vausi" `shouldBe` QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))
        it "token=t1,t2" $ do
            breakPair "name=Vausi,Clausi" `shouldBe` QPair QtParam (Key (Just "name")) (QValue (Just "Vausi,Clausi"))
      describe "parse illegal kv" $ do
        it "=" $ do
            breakPair "=" `shouldBe` (QValue Nothing)
        it "name=" $ do
            breakPair "name=" `shouldBe` QPair QtParam (Key (Just "name")) (QValue Nothing)
        it "=Vausi" $ do
            breakPair "=Vausi" `shouldBe` (QValue (Just "Vausi"))
      describe "parse query" $ do
        it "kv" $ do
            parseQueryString (Just FHIR_Any) "name=Vausi" `shouldBe` 
                             QList [QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))]
        it "kv" $ do
            parseQueryString (Just FHIR_Any) "name-use=offical" `shouldBe` 
                             QList [QPair QtParam (Key (Just "name-use")) (QValue (Just "offical"))]
        it "kv any" $ do
            parseQueryString (Just FHIR_Any) "_lastUpdated=2020-01-01" `shouldBe` 
                             QList [QPair QtParam (Key (Just "_lastUpdated")) (QValue (Just "2020-01-01"))]
        it "kv with mod" $ do
            parseQueryString (Just FHIR_Any) "name:exact=Vausi" `shouldBe` 
                             QList [QPair QtParam (KeyMod "name" (Just "exact")) (QValue (Just "Vausi"))]
        it "kv with prefix" $ do
            parseQueryString (Just FHIR_Any) "birthdate=gt2020-01-01" `shouldBe` 
                             QList [QPair QtParam (Key (Just "birthdate")) (QValue (Just "gt2020-01-01"))]
        it "kv&kv" $ do
            parseQueryString (Just FHIR_Any) "name=Vausi&name=Clausi" `shouldBe` 
                             QList [
                                     QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))
                                   , QPair QtParam (Key (Just "name")) (QValue (Just "Clausi"))
                                   ]
        it "&" $ do
            parseQueryString (Just FHIR_Any) "&" `shouldBe` 
                             QList []
        it "kv&" $ do
            parseQueryString (Just FHIR_Any) "name=Vausi&" `shouldBe` 
                             QList [QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))]
        it "&kv" $ do
            parseQueryString (Just FHIR_Any) "&name=Vausi" `shouldBe` 
                             QList [QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))]
      describe "validate boolean" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtBoolean) `shouldBe` QError "invalid Boolean: " "" 
        it "invalid" $ do
           validate (QValue (Just "nobool")) (Just SdtBoolean) `shouldBe` QError "invalid Boolean: " "nobool" 
        it "true" $ do
           validate (QValue (Just "true")) (Just SdtBoolean) `shouldBe` QBoolean True
        it "false" $ do
           validate (QValue (Just "false")) (Just SdtBoolean) `shouldBe` QBoolean False
      describe "validate date" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtDate) `shouldBe` QError "invalid Date: " "" 
        it "invalid YYY" $ do
           validate (QValue (Just "123")) (Just SdtDate) `shouldBe` QError "invalid Date: " "123" 
        it "invalid YYYY-" $ do
           validate (QValue (Just "1234-")) (Just SdtDate) `shouldBe` QError "invalid Date: " "1234-" 
        it "invalid YYYY-1" $ do
           validate (QValue (Just "1234-1")) (Just SdtDate) `shouldBe` QError "invalid Date: " "1234-1" 
        it "invalid YYYY-MM-D" $ do
           validate (QValue (Just "1234-12-1")) (Just SdtDate) `shouldBe` QError "invalid Date: " "1234-12-1" 
        it "valid YYYY" $ do
           validate (QValue (Just "2021")) (Just SdtDate) `shouldBe` QDate Nothing "2021" 4
        it "valid YYYY-MM" $ do
           validate (QValue (Just "2021-01")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01" 6
        it "valid YYYY-MM-DD" $ do
           validate (QValue (Just "2021-01-01")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01-01" 8
        it "valid YYYY-MM-DDTHH:MM:SS" $ do
           validate (QValue (Just "2021-01-01T12:00:00")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01-01T12:00:00" 14
        it "valid YYYY-MM-DDTHH:MM:SSZ" $ do
           validate (QValue (Just "2021-01-01T12:00:00Z")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01-01T12:00:00+0000" 14
        it "valid YYYY-MM-DDTHH:MM:SS-02:00" $ do
           validate (QValue (Just "2021-01-01T12:00:00-0200")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01-01T12:00:00-0200" 14
        it "valid YYYY-MM-DDTHH:MM:SS.sss" $ do
           validate (QValue (Just "2021-01-01T12:00:00.123")) (Just SdtDate) `shouldBe` QDate Nothing "2021-01-01T12:00:00.123" 17
        it "prefix YYYY-MM-DD" $ do
           validate (QValue (Just "gt2021-01-01")) (Just SdtDate) `shouldBe` QDate (Just "gt") "2021-01-01" 8
      describe "validate number" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtNumber) `shouldBe` QError "invalid Number: " "" 
        it "invalid" $ do
           validate (QValue (Just "abc")) (Just SdtNumber) `shouldBe` QError "invalid Number: " "abc" 
        it "valid int" $ do
           validate (QValue (Just "123")) (Just SdtNumber) `shouldBe` QNumber Nothing 123.0 3
        it "valid float" $ do
           validate (QValue (Just "123.0")) (Just SdtNumber) `shouldBe` QNumber Nothing 123.0 4
        it "valid signed float" $ do
           validate (QValue (Just "-123.0")) (Just SdtNumber) `shouldBe` QNumber Nothing (-123.0) 4
        it "valid scientific" $ do
           validate (QValue (Just "123e2")) (Just SdtNumber) `shouldBe` QNumber Nothing 12300.0 3
        it "valid scientific" $ do
           validate (QValue (Just "123.9e2")) (Just SdtNumber) `shouldBe` QNumber Nothing 12390.0 4
        it "valid float with prefix" $ do
           validate (QValue (Just "eq123.0")) (Just SdtNumber) `shouldBe` QNumber (Just "eq") 123.0 4
        it "valid int with invalid prefix" $ do
           validate (QValue (Just "q123")) (Just SdtNumber) `shouldBe` QError "invalid Number: " "q123"
      describe "validate quantity" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtQuantity) `shouldBe` QError "invalid Quantity: " "" 
        it "invalid" $ do
           validate (QValue (Just "abc")) (Just SdtQuantity) `shouldBe` QError "invalid Quantity: " "abc" 
        it "valid float" $ do
           validate (QValue (Just "5.4")) (Just SdtQuantity) `shouldBe`
                                              QQuantity Nothing (5.4) 2 Nothing Nothing
        it "valid float with code" $ do
           validate (QValue (Just "5.4||mg")) (Just SdtQuantity) `shouldBe`
                                              QQuantity Nothing (5.4) 2 Nothing (Just "mg")
        it "valid float|system|code" $ do
           validate (QValue (Just "5.4|http://unitsofmeasure.org|mg")) (Just SdtQuantity) `shouldBe`
                                              QQuantity Nothing (5.4) 2 (Just "http://unitsofmeasure.org") (Just "mg")
        it "valid full" $ do
           validate (QValue (Just "eq5.4|http://unitsofmeasure.org|mg")) (Just SdtQuantity) `shouldBe`
                                              QQuantity (Just "eq") (5.4) 2 (Just "http://unitsofmeasure.org") (Just "mg")
      describe "validate reference" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtReference) `shouldBe` QError "invalid Reference: " ""
        it "invalid" $ do
           validate (QValue (Just "_")) (Just SdtReference) `shouldBe` QError "invalid Reference: " "_"
        it "valid id" $ do
           validate (QValue (Just "1234")) (Just SdtReference) `shouldBe`
                                     QReference Nothing "1234" Nothing
        it "valid type/id" $ do
           validate (QValue (Just "Patient/1234")) (Just SdtReference) `shouldBe`
                                     QReference Nothing "Patient/1234" Nothing
        it "valid URI" $ do
           validate (QValue (Just "http://eNahar.org/nabu/Patient/1234")) (Just SdtReference) `shouldBe`
                                     QReference (Just URI {
                                                       uriScheme = "http:"
                                                     , uriAuthority = Just $ URIAuth {
                                                                 uriUserInfo = ""
                                                               , uriRegName  = "eNahar.org"
                                                               , uriPort     = ""
                                                               }
                                                     , uriPath     = "/nabu/Patient/1234"
                                                     , uriQuery    = ""
                                                     , uriFragment = ""
                                                     }) "http://eNahar.org/nabu/Patient/1234" Nothing
        it "valid URI with version" $ do
           validate (QValue (Just "http://eNahar.org/nabu/Patient/1234|1")) (Just SdtReference) `shouldBe`
                                     QReference (Just URI {
                                                       uriScheme = "http:"
                                                     , uriAuthority = Just $ URIAuth {
                                                                 uriUserInfo = ""
                                                               , uriRegName  = "eNahar.org"
                                                               , uriPort     = ""
                                                               }
                                                     , uriPath     = "/nabu/Patient/1234"
                                                     , uriQuery    = ""
                                                     , uriFragment = ""
                                                     }) "http://eNahar.org/nabu/Patient/1234" (Just "1")
      describe "validate token" $ do
        it "empty" $ do
           validate (QValue (Just "")) (Just SdtToken) `shouldBe` QError "invalid Token: " ""
        it "invalid" $ do
           validate (QValue (Just "_")) (Just SdtToken) `shouldBe` QError "invalid Token: " "_"
        it "valid code" $ do
           validate (QValue (Just "1234")) (Just SdtToken) `shouldBe` QToken Nothing (Just "1234") Nothing
        it "valid |code" $ do
           validate (QValue (Just "|1234")) (Just SdtToken) `shouldBe` QToken Nothing (Just "1234") Nothing
        it "valid system|" $ do
           validate (QValue (Just "xxx|")) (Just SdtToken) `shouldBe` QToken (Just "xxx") Nothing Nothing
        it "valid system|code" $ do
           validate (QValue (Just "xxx|123")) (Just SdtToken) `shouldBe` QToken (Just "xxx") (Just "123") Nothing
        it "valid system|code|value" $ do
           validate (QValue (Just "xxx|yyy|123")) (Just SdtToken) `shouldBe` QToken (Just "xxx") (Just "yyy") (Just "123")
      describe "checkTypeParam" $ do
        it "no _type" $ do
            checkTypeParam [QPair QtParam (Key (Just "_tag")) (QValue (Just "Vausi"))] `shouldBe` Right []
        it "_type=Patient" $ do
            checkTypeParam [QPair QtParam (Key (Just "_type")) (QValue (Just "Patient"))] `shouldBe` Right [FHIR_Patient]
        it "_type=Patient,Practitioner" $ do
            checkTypeParam [QPair QtParam (Key (Just "_type")) (QValue (Just "Patient,Practitioner"))] `shouldBe` Right [FHIR_Patient,FHIR_Practitioner]
        it "_type=Patient,Xyz" $ do
            checkTypeParam [QPair QtParam (Key (Just "_type")) (QValue (Just "Patient,Xyz"))] `shouldBe` Left "_type: invalid resource" 
      describe "analyze querystring" $ do
        it "split []" $ do
            split (QList [])
                 (Just FHIR_Any) rc `shouldBe` ([FHIR_Any], ([],[],[],[],[]))
        it "split [reserved param]" $ do
            split (QList [QPair QtParam (Key (Just "_tag")) (QValue (Just "Vausi"))])
                 (Just FHIR_Any) rc `shouldBe`
                                         ([FHIR_Any],([QPair QtProp (Key (Just "_tag")) (QToken Nothing (Just "Vausi") Nothing)],[],[],[],[]))
        it "split [reserved param]" $ do
            split (QList [QPair QtParam (Key (Just "_format")) (QValue (Just "xml"))])
                 (Just FHIR_Any) rc `shouldBe` 
                                         ([FHIR_Any], ([],[],[],[QPair QtControl (Key (Just "_format")) (QToken Nothing (Just "xml") Nothing)],[]))
        it "split [reserved result]" $ do
            split (QList [QPair QtParam (Key (Just "page")) (QValue (Just "23"))])
                 (Just FHIR_Any) rc `shouldBe`
                                         ([FHIR_Any], ([],[],[],[QPair QtResult (Key (Just "page")) (QNumber Nothing 23.0 2)],[]))
        it "split [reserved special]" $ do
            split (QList [QPair QtParam (Key (Just "_text")) (QValue (Just "Vausi"))])
                 (Just FHIR_Any) rc `shouldBe`
                                         ([FHIR_Any], ([],[],[],[],[]))
        it "split [any:nyi]" $ do
            split (QList [QPair QtParam (Key (Just "_included")) (QValue (Just "2020-01-01"))])
                 (Just FHIR_Any) rc `shouldBe`
                                         ([FHIR_Any], ([],[],[],[],[]))
        it "split [ignore]" $ do
            split (QList [QPair QtParam (Key (Just "name")) (QValue (Just "Vausi"))])
                 (Just FHIR_Any) rc `shouldBe`
                                         ([FHIR_Any], ([],[],[],[],[]))
        it "split [prop]" $ do
            split (QList [QPair QtParam (Key (Just "family")) (QValue (Just "Vausi"))])
                 (Just FHIR_Patient) rc `shouldBe`
                                         ([FHIR_Patient], ([QPair QtProp (Key (Just "family")) (QString "Vausi")],[],[],[],[]))
        it "split [prop]" $ do
            split (QList [QPair QtParam (Key (Just "name-use")) (QValue (Just "offical"))])
                 (Just FHIR_Patient) rc `shouldBe`
                                         ([FHIR_Patient], ([QPair QtProp (Key (Just "name-use")) (QToken Nothing (Just "offical") Nothing)],[],[],[],[]))
      describe "analyze type/instance level query" $ do
        let ty = Just FHIR_Patient
        let analyze s = split (parseQueryString ty s) ty rc
        it "string" $ do
            analyze "family=Vausi&family=Polausi" `shouldBe` 
                        ([FHIR_Patient], ([ QPair QtProp (Key (Just "family")) (QString "Vausi")
                                          , QPair QtProp (Key (Just "family")) (QString "Polausi")
                                          ],[],[],[],[]))
        it "string" $ do
            analyze "family=Vausi" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "family")) (QString "Vausi")],[],[],[],[]))
        it "token" $ do
            analyze "name-use=offical" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "name-use")) (QToken Nothing (Just "offical") Nothing)],[],[],[],[]))
        it "token" $ do
            analyze "identifier=123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "identifier")) (QToken Nothing (Just "123") Nothing)],[],[],[],[]))
        it "code" $ do
            analyze "identifier=123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "identifier")) (QToken Nothing (Just "123") Nothing)],[],[],[],[]))
        it "|code" $ do
            analyze "identifier=|123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "identifier")) (QToken Nothing (Just "123") Nothing)],[],[],[],[]))
        it "system|" $ do
            analyze "identifier=ORBIS_PID|123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "identifier")) (QToken (Just "ORBIS_PID") (Just "123") Nothing)],[],[],[],[]))
        it "system|code" $ do
            analyze "identifier=ORBIS_PID|123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "identifier")) (QToken (Just "ORBIS_PID") (Just "123") Nothing)],[],[],[],[]))
        it "system|code|value" $ do
            analyze "identifier:of-type=system|MR|123" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (KeyMod "identifier" (Just "of-type")) (QToken (Just "system") (Just "MR") (Just "123"))],[],[],[],[]))
        it "birthdate YMD" $ do
            analyze "birthdate=2020-01-01" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[],[],[]))
        it "birthdate YM" $ do
            analyze "birthdate=2020-01" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01" 6)],[],[],[],[]))
        it "birthdate Y" $ do
            analyze "birthdate=2020" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020" 4)],[],[],[],[]))
        it "birthdate illegal" $ do
            analyze "birthdate=202" `shouldBe` 
                        ([FHIR_Patient], ([],[],[],[],[QPair QtProp (Key (Just "birthdate")) (QError "invalid Date: " "202")]))
        it "birthdate illegal" $ do
            analyze "birthdate=2020-1" `shouldBe` 
                        ([FHIR_Patient], ([],[],[],[],[QPair QtProp (Key (Just "birthdate")) (QError "invalid Date: " "2020-1")]))
        it "birthdate illegal" $ do
            analyze "birthdate=2020-" `shouldBe` 
                        ([FHIR_Patient], ([],[],[],[],[QPair QtProp (Key (Just "birthdate")) (QError "invalid Date: " "2020-")]))
        it "modifier missing (birthdate)" $ do
            analyze "birthdate:missing=true" `shouldBe`
                        ([FHIR_Patient], ([QPair QtProp (KeyMod "birthdate" (Just "missing")) (QString "true")],[],[],[],[]))
        it "prefix 'gt' (birthdate)" $ do
            analyze "birthdate=gt2020-01-01" `shouldBe`
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate (Just "gt") "2020-01-01" 8)],[],[],[],[]))
        it "_type=Practitioner&birthdate=2020-01-01 (illegal)" $ do
            analyze "_type=Practitioner&birthdate=2020-01-01" `shouldBe` 
                        ([FHIR_Patient], ( [],[],[],[]
                                         , [QPair QtParam (Key (Just "_type"))
                                                  (QError "_type: illegal on type/instance level" "[QPair QtParam (Key (Just \"_type\")) (QValue (Just \"Practitioner\")),QPair QtParam (Key (Just \"birthdate\")) (QValue (Just \"2020-01-01\"))]")
                                           ]))
      describe "analyze server level query" $ do
        let analyzeWoT s = split (parseQueryString Nothing s) Nothing rc
        it "_type=Patient&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[],[],[]))
        it "_type=Patient,Practitioner" $ do
            analyzeWoT "_type=Patient,Practitioner&birthdate=2020-01-01" `shouldBe` 
                        ([FHIR_Patient,FHIR_Practitioner], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[],[],[]))
        it "_type=Patient,Practitioner, key non-intersecting" $ do
            analyzeWoT "_type=Patient,Practitioner&birthdate=2020-01-01&language=de" `shouldBe` 
--
--TODO ignore key or error?
--
                        ([FHIR_Patient,FHIR_Practitioner],([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8),QPair QtProp (Key (Just "language")) (QToken Nothing (Just "de") Nothing)],[],[],[],[QPair QtProp (Key (Just "language")) (QToken Nothing (Just "de") Nothing)]))
        it "_type=Patient,Patient&family=Vausi" $ do
            analyzeWoT "_type=Patient,Patient&birthdate=2020-01-01" `shouldBe` 
                        ([FHIR_Patient], ([QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[],[],[]))
        it "*?_type=Patient,Xyz&family=Vausi" $ do
            analyzeWoT "_type=Patient,Xyz&birthdate=2020-01-01" `shouldBe` 
                        ([],([],[],[],[]
                         , [ QPair QtParam (Key (Just "_type")) 
                                   (QError "_type: invalid resource" "[QPair QtParam (Key (Just \"_type\")) (QValue (Just \"Patient,Xyz\")),QPair QtParam (Key (Just \"birthdate\")) (QValue (Just \"2020-01-01\"))]")
                           ]
                         ))
        it "_type=Task&recipient-date=2020-01-01" $ do
            analyzeWoT "_type=Task&recipient-date=2020-01-01" `shouldBe`
                        ([FHIR_Task],([QPair QtProp (Key (Just "recipient-date")) (QDate Nothing "2020-01-01" 8)],[],[],[],[]))
      describe "sort" $ do
        let analyzeWoT s = split (parseQueryString Nothing s) Nothing rc
        it "_type=Patient&birthdate=2020-01-01&_sort=birthdate" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=birthdate" `shouldBe`
                        ([FHIR_Patient],
                            ( 
                              [QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[]
                            , [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "birthdate") Nothing)],[]
                            ))
        it "_type=Patient&birthdate=2020-01-01&_sort=-birthdate" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=-birthdate" `shouldBe`
                        ([FHIR_Patient],
                            ( 
                              [QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[]
                            , [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "-birthdate") Nothing)],[]
                            ))
        it "_type=Patient&_sort=family,given,birthdate" $ do
            analyzeWoT "_type=Patient&_sort=family,given,birthdate" `shouldBe`
                        ([FHIR_Patient],
                            ( 
                              [],[],[]
                            , [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "family,given,birthdate") Nothing)],[]
                            ))
        it "_type=Patient&birthdate=2020-01-01&_sort=family,given,birthdate" $ do
            analyzeWoT "_type=Patient&birthdate=2020-01-01&_sort=family,given,birthdate" `shouldBe`
                        ([FHIR_Patient],
                            ( 
                              [QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[]
                            , [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "family,given,birthdate") Nothing)],[]
                            ))
        it "_type=Patient&_sort=family,given,birthdate&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient&_sort=family,given,birthdate&birthdate=2020-01-01" `shouldBe`
                        ([FHIR_Patient],
                            ( 
                              [QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)],[],[]
                            , [QPair QtControl (Key (Just "_sort")) (QToken Nothing (Just "family,given,birthdate") Nothing)],[]
                            ))
      describe "elements" $ do
        let analyzeWoT s = split (parseQueryString Nothing s) Nothing rc
        it "_type=Patient&_elements=id,text" $ do
            analyzeWoT "_type=Patient&_elements=id,text" `shouldBe`
                        ([FHIR_Patient],([],[QPair QtSpecial (Key (Just "_elements")) (QToken Nothing (Just "id,text") Nothing)],[],[],[]))
        it "_type=Patient&_elements=id,text&birthdate=2020-01-01" $ do
            analyzeWoT "_type=Patient&_elements=id,text&birthdate=2020-01-01" `shouldBe`
                        ([FHIR_Patient],
                            (
                              [QPair QtProp (Key (Just "birthdate")) (QDate Nothing "2020-01-01" 8)]
                            , [QPair QtSpecial (Key (Just "_elements")) (QToken Nothing (Just "id,text") Nothing)],[],[],[]
                            ))
      describe "_count, page" $ do
        let analyzeWoT s = split (parseQueryString Nothing s) Nothing rc
        it "_type=Patient&_count=0" $ do
            analyzeWoT "_type=Patient&_count=0" `shouldBe`
                        ([FHIR_Patient],([],[],[],[QPair QtResult (Key (Just "_count")) (QNumber Nothing 0.0 1)],[]))
        it "_type=Patient&_count=-1" $ do
            analyzeWoT "_type=Patient&_count=-1" `shouldBe`
                        ([FHIR_Patient],([],[],[],[QPair QtResult (Key (Just "_count")) (QNumber Nothing (-1.0) 1)],[]))
        it "_type=Patient&_count=30" $ do
            analyzeWoT "_type=Patient&_count=30" `shouldBe`
                        ([FHIR_Patient],([],[],[],[QPair QtResult (Key (Just "_count")) (QNumber Nothing 30.0 2)],[]))
        it "_type=Patient&page=0" $ do
            analyzeWoT "_type=Patient&page=0" `shouldBe`
                        ([FHIR_Patient],([],[],[],[QPair QtResult (Key (Just "page")) (QNumber Nothing 0.0 1)],[]))
        it "_type=Patient&page=3" $ do
            analyzeWoT "_type=Patient&page=3" `shouldBe`
                        ([FHIR_Patient],([],[],[],[QPair QtResult (Key (Just "page")) (QNumber Nothing 3.0 1)],[]))
        it "_type=Patient&_count=30&page=3" $ do
            analyzeWoT "_type=Patient&_count=30&page=3" `shouldBe`
                        ( [FHIR_Patient]
                        , ( [],[],[]
                          , [ QPair QtResult (Key (Just "_count")) (QNumber Nothing 30.0 2)
                            , QPair QtResult (Key (Just "page")) (QNumber Nothing 3.0 1)]
                          , []
                          )
                        )


--      describe "analyze compartment query" $ do
-- Error reading config file
    Left e -> error $ show e


