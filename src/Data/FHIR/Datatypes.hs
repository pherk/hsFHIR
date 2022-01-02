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
 - FHIR Datatypes v4.0.1
 - deviations
 -   primitive datatype cannot have extensions
 -   date and dateTime
 -   super element id folded in, but not all types take extensions yet
 -     only HumanName, Identifier
 -   super backBoneElement id folded in, but not all types take extensions yet
 - Todos
 -   real toJSON and parseJSON instances
 -   toXMl, parseXML instances 
 -   Extension props when TH elminated
-}
module Data.FHIR.Datatypes (
    module Data.FHIR.Datatypes.Internal 
  , module Data.FHIR.Datatypes.ElementDefinition -- cannot be an extension
  ) where

import Data.FHIR.Datatypes.Internal
import Data.FHIR.Datatypes.ElementDefinition
