{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.FHIR.RestApi where

import qualified Data.Text                   as T
import           RIO
import qualified RIO.Map                 as HM
import           Data.FHIR.Search

reservedKeys :: HM.Map T.Text (SearchKeyType, SearchDataType) 
reservedKeys = HM.fromList([
      ("_content"       , (SktParam, SdtToken))
    , ("_id"            , (SktParam, SdtToken))
    , ("_lastUpdated"   , (SktParam, SdtDate))
    , ("_profile"       , (SktParam, SdtToken))
    , ("_query"         , (SktParam, SdtToken)) -- ??
    , ("_security"      , (SktParam, SdtToken))
    , ("_tag"           , (SktParam, SdtToken))
    , ("_elements"      , (SktParam, SdtToken))
    , ("_count"         , (SktResult, SdtNumber))
    , ("_format"        , (SktResult, SdtToken))
    , ("_include"       , (SktResult, SdtToken))
    , ("_pretty"        , (SktResult, SdtBoolean))
    , ("_revinclude"    , (SktResult, SdtToken))
    , ("_sort"          , (SktResult, SdtToken))
    , ("_source"        , (SktResult, SdtToken)) -- ???
    , ("_summary"       , (SktResult, SdtToken))
    , ("_total"         , (SktResult, SdtToken)) -- none, estimate, accurate 
    , ("_contained"     , (SktResult, SdtToken))
    , ("_containedType" , (SktResult, SdtToken))
    , ("page"           , (SktResult, SdtNumber))
    , ("_text"          , (SktSpecial, SdtString))
    , ("_type"          , (SktSpecial, SdtToken))
    , ("_filter"        , (SktSpecial, SdtToken))
    ])
  
modififierRestriction :: HM.Map T.Text [SearchDataType]
modififierRestriction = HM.fromList(
    [
      ("missing"    , [SdtSpecial])
    , ("exact"      , [SdtString])
    , ("contains"   , [SdtString])
    , ("starts-with", [SdtString])      -- non-standard
    , ("text"       , [SdtToken])
    , ("text"       , [SdtToken])
    , ("in"         , [SdtToken])
    , ("below"      , [SdtToken, SdtUri])
    , ("above"      , [SdtToken, SdtUri])
    , ("not-in"     , [SdtToken])
    , ("text"       , [SdtToken])
    , ("type"       , [SdtReference])
    ])

{-
-define(EXPAND_KEYS, #{
      ("filter"    , {}	
    , ("date"    , {}	
    , ("offset"    , {}	
    , ("count"    , {}	
    , ("includeDesignations"    , {}	
    , ("designation"    , {}	
    , ("includeDefinition"    , {}	
    , ("activeOnly"    , {}	
    , ("excludeNested"    , {}	
    , ("excludeNotForUI"    , {}	
    , ("excludePostCoordinated"    , {}	
    , ("displayLanguage"    , {}	
    , ("exclude-system"    , {}	
    , ("system-version"    , {}	
    , ("check-system-version"    , {}	
    , ("force-system-version"    , {}	
    }).
-}

summaryTokens :: [T.Text]
summaryTokens =
    ["true"         -- Return a limited subset of elements from the resource. 
                    -- This subset SHOULD consist solely of all supported elements that are marked as "summary"
    ,"text"         -- Return only the "text" element, the 'id' element, 
                    -- the 'meta' element, and only top-level mandatory elements
    ,"data"         -- Remove the text element
    ,"count"        -- Search only: just return a count of the matching resources, without returning the actual matches
    ,"false"        -- Return all parts of the resource(s)
    ]

