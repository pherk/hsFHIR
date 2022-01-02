{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-
 - FHIR Resources v4.0.1
- deviations
-}

module Data.FHIR.Datatypes.XmlUtils where

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import           Xmlbf
 

data Props   
  = OptVal Text (Maybe Text)
  | Val Text Text
  | ValList Text [Text]
  | OptProp Text (Maybe [Node])
  | OptPropT Text (Maybe (HM.HashMap Text Text, [Node]))
  | Prop Text (HM.HashMap Text Text, [Node])
  | PropList Text [[Node]]

toAttr (OptVal k  Nothing) = Nothing
toAttr (OptVal k (Just v)) = Just (k,v)
toAttr (Val k v) = Just (k,v)

toElement (OptVal k  Nothing) = []
toElement (OptVal k (Just v)) = element k (HM.fromList [("value",v)]) []
toElement (Val k v) = element k (HM.fromList [("value",v)]) []
toElement (ValList k vs) = concatMap (\v -> (element k (HM.fromList [("value",v)]) [])) vs
toElement (OptProp k  Nothing) = []
toElement (OptProp k (Just n)) = element k HM.empty n
toElement (OptPropT k Nothing) = []
toElement (OptPropT k (Just (attrs, r))) = element k attrs r
toElement (Prop k (_,[])) = []
toElement (Prop k (a,n))  = element k a n
toElement (PropList k ps) = concatMap (\n -> (element k HM.empty n)) ps

