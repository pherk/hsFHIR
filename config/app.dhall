let Env = < Test : {} | Prod : {} >

let DAOEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "free", Prod = λ(x : {}) →  "api" } env

let redisEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "127.0.0.1", Prod = λ(x : {}) →  "127.0.0.1" } env

-- let makeIndexes = λ(env : Env) →
--   [ { riResourceType= "Patient"
--     , riStatus = Normative
--     , riSupported = True
--     , riIndexes = [
--         { riSearchName = "active"
--         , riIndexType  = "token"
--         , riIndexName  = "patient_active"
--         , riIndexDef   = [ "(`active`)", "(`resourceType` = \"Patient\")" ]
--         }
--        ] 
--     }
--   ]
-- 
-- let makeSearch = λ(env : Env) →
--   [ 
--     { siName           = "family"
--     , siFhirDataType   = FdtString
--     , siSearchDataType = "SdtString"
--     , siPath           = [""]
--     , siTarget         = ["Patient","Practitioner"]
--     , siMultipleOr     = True
--     , siMultipleAnd    = True
--     , siComparator     = ["ScEq"]
--     , siModifier       = ["SmExact", "SmMissing", "SmContains"]
--     , siIndex          = "ItArray"
--     }
--   ]

let makeDAOConfig = λ(env : Env) →
--  { apiHost = "https://${forexEnv env}.currconv.com"
--  , apiKey = "${env:FOREX_API_KEY as Text}"
  { apiHost = "http://127.0.0.1"
  , apiKey = "nabu"
  , apiPath = "/nabu/v4"
  , apiUsage = "/others/usage"
  , apiKeyExpiration = 60 -- 3600 -- 1 hour in seconds
  , apiReqPerHour = 100
  , apiIndexes = "config/indexes.xml"
  , apiSearch = "config/search_parameters.xml"
  }

{-
-- example for search_parameter
{                                                                                                                                         
  "fullUrl" : "http://hl7.org/fhir/SearchParameter/individual-family",
  "resource" : {
    "resourceType" : "SearchParameter",
    "id" : "individual-family",
    "extension" : [{
      "url" : "http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status",
      "valueCode" : "trial-use"
    }],
    "url" : "http://hl7.org/fhir/SearchParameter/individual-family",
    "version" : "4.0.0",
    "name" : "family",
    "status" : "draft",
    "experimental" : false,
    "date" : "2018-12-27T22:37:54+11:00",
    "publisher" : "Health Level Seven International (Patient Administration)",
    "contact" : [{
      "telecom" : [{
        "system" : "url",
        "value" : "http://hl7.org/fhir"
      }]
    },
    {
      "telecom" : [{
        "system" : "url",
        "value" : "http://www.hl7.org/Special/committees/pafm/index.cfm"
      }]
    }],
    "description" : "Multiple Resources: \r\n\r\n* [Patient](patient.html): A portion of the family name of the patient\r\n* [Practitioner
    "code" : "family",
    "base" : ["Patient", "Practitioner"],
    "type" : "string",
    "expression" : "Patient.name.family | Practitioner.name.family",
    "xpath" : "f:Patient/f:name/f:family | f:Practitioner/f:name/f:family",
    "xpathUsage" : "normal",
    "multipleOr" : true,
    "multipleAnd" : true,
    "modifier" : ["missing", "exact", "contains"]
  }
},   
     { C.connectHost           = CC.HostName "couchbase://192.168.178.24"
     , C.connectPort           = CC.PortNumber 8091
     , C.connectUser           = Just "erlang"
     , C.connectAuth           = Just "5RZz(8e^y.N(+y_H"
     , C.connectBucket         = Just "nabu"
     , C.connectDatabase       = 0
     , C.connectMaxConnections = 50
     , C.connectMaxIdleTime    = 3
     , C.connectTimeout        = Nothing
-}

let makeRedisConfig = λ(env : Env) →
  { redisHost = "127.0.0.1"
  , redisPort = 6379
  , redisExpiration = 60 -- seconds
  }

let makeConfig = λ(env : Env) →
  { dao   = makeDAOConfig env
  , redis = makeRedisConfig env
  }

in makeConfig ( Env.Test {=} )
