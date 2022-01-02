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

module Data.FHIR.Datatypes.XhtmlDiv ( 
     XhtmlDiv(..)
   , mkXhtmlDiv
   , toXhtmlDivXml
   , Div(..)
   , Flow(..)
   , Inline(..)
   , Anchor(..)
   ) where

import Data.Aeson  
import Data.Aeson.Types hiding (parseJSON)

import Data.Int(Int64)
import Data.Scientific (Scientific)
--import Data.Time.ISO8601.Duration
--import Errors

import RIO
import qualified RIO.Text  as T
import qualified RIO.Text.Lazy  as TL
import qualified RIO.HashMap as HM
import qualified RIO.Vector as V
import           Data.FHIR.Datatypes.XmlUtils
import qualified Xmlbf as Xmlbf
{-
 - <div xmlns=\"http://www.w3.org/1999/xhtml\">
 -   Generated by <a href=\"https://github.com/synthetichealth/synthea\">Synthea</a>.
 -   Version identifier: v2.4.0-272-gbd747730\n .
 -   Person seed: 7096289649312534589  Population seed: 1563971124564
 - </div>
-}

data XhtmlDiv = XhtmlDiv Div deriving (Eq, Show)

data Attr = Attr Text Text deriving (Eq, Show)
type Attrs = [Attr] 

data Inline
    = IA Anchor
--    | IObject ObjectT
--    | IFont TypoFont
--    | ITypo TypoInline
--    | IInteraction InteractionInline
--    | IScript ScriptInline
    | IText Text
    deriving (Eq, Show)

data ObjectT
    = OtObject Object
    | OtImg Img
    | OtOther OtherObject
    deriving (Eq, Show)

data OtherObject
    = OoBr
    | OoSpan Span
    | OoBdo
    | OoMap
    deriving (Eq, Show)

data TypoFont
    = TfTT
    | TfI
    | TfB
    | TfBig
    | TfSmall
    deriving (Eq, Show)

data TypoInline
    = TiEm
    | TiStrong
    | TiDFN
    | TiCode
    | TiQ
    | TiSamp
    | TiKBD
    | TiVar
    | TiCite
    | TiAbbr
    | TiAcronym
    deriving (Eq, Show)

data InteractionInline
    = IiInput Input
    | IiSelect Select
    | IiTextarea Textarea
    | IiLabel Label
    | IiButton Button
    deriving (Eq, Show)

data ScriptInline
    = SiIns
    | SiDel
    | SiScript
    deriving (Eq, Show)

data BlockLevel
    = BlE BlockLevelElements
--    | BlF Form
    deriving (Eq, Show)

data BlockLevelElements
    = BleP P
--    | BlHeader Header
    | BleDiv Div
--    | BleList
--    | BleOther
--    | BleFieldset
--    | BleTable 
    deriving (Eq, Show)

data BlockLevelOther
    = BloPre Pre
    | BloHr Hr
    | BloBlockquote Blockquote
    | BloAddress Address
    | BloScript
    deriving (Eq, Show)

data ScriptBlockLevel
    = SblNoScript
    | SblOther ScriptInline
    deriving (Eq, Show)

data Flow
    = FBl BlockLevel
    | FI  Inline
    | FText  Text
    deriving (Eq, Show)

data AnchorContent 
    = AcObject ObjectT
    | AcFont TypoFont
    | AcTypo TypoInline
    | AcInteraction InteractionInline
    | AcScript ScriptInline
    deriving (Eq, Show)

data PreContent
    = PcA Anchor
    | PcFont TypoFont
    | PcInline TypoInline
    | PcScript ScriptInline
    | PcInteraction InteractionInline
    deriving (Eq, Show)
{-
data FormContent
    = FcP P
    | FcH Header
    | FcList List
    | FcFieldset Fieldset
    | FcTable Table
--    | FcScript ScriptBlocklevel
    deriving (Eq, Show)
-}
data ButtonContent
    = BcP P
    | BcHeader Header
    | BcList List
    | BcOther BlockLevelOther
    | BcTable Table
    | BcObject ObjectT
    | BcFont TypoFont
    | BcTypo TypoInline
    | BcScript ScriptBlockLevel
    deriving (Eq, Show)

data Div = Div Attrs [Flow] deriving (Eq, Show)
data P   = P Attrs [Inline] deriving (Eq, Show)
data Header
    = HH1 H1 
    | HH2 H2 
    | HH3 H3 
    | HH4 H4 
    | HH5 H5 
    | HH6 H6 
    deriving (Eq, Show)

data H1  = H1 Attrs Inline deriving (Eq, Show)
data H2  = H2 Attrs Inline deriving (Eq, Show)
data H3  = H3 Attrs Inline deriving (Eq, Show)
data H4  = H4 Attrs Inline deriving (Eq, Show)
data H5  = H5 Attrs Inline deriving (Eq, Show)
data H6  = H6 Attrs Inline deriving (Eq, Show)

data List 
    = LUL Attrs [Li]
    | LOL Attrs [Li]
    | LDL Attrs [DtDds]
    deriving (Eq, Show)

data Li = Li Attrs Flow deriving (Eq, Show)
data DtDds = DtDds Dt [Dd] deriving (Eq, Show)
data Dt = Dt Attrs Inline deriving (Eq, Show)
data Dd = Dd Attrs Flow deriving (Eq, Show)
data Address = Address Attrs Flow deriving (Eq, Show)
data Hr = Hr Attrs deriving (Eq, Show)
data Pre = Pre Attrs PreContent   deriving (Eq, Show)-- preserve spaces
data Blockquote = BlockQuote Attrs BlockLevel deriving (Eq, Show)
--data Anchor = Anchor Attrs AnchorContent deriving (Eq, Show)
data Anchor = Anchor Attrs Text deriving (Eq, Show)

-- Inlines
data Span   = Span    Attrs Inline deriving (Eq, Show)
data Bdo    = Bdo     Attrs Inline deriving (Eq, Show)
data Br     = Br      Attrs deriving (Eq, Show)
data Em     = Em      Attrs Inline deriving (Eq, Show)
data Strong = Strong  Attrs Inline deriving (Eq, Show)
data Dfn    = Dfn     Attrs Inline deriving (Eq, Show)
data Code   = Code    Attrs Inline deriving (Eq, Show)
data Samp   = Samp    Attrs Inline deriving (Eq, Show)
data Kbd    = Kbd     Attrs Inline deriving (Eq, Show)
data Var    = Var     Attrs Inline deriving (Eq, Show)
data Cite   = Cite    Attrs Inline deriving (Eq, Show)
data Abbr   = Abbr    Attrs Inline deriving (Eq, Show)
data Acronym= Acronym Attrs Inline deriving (Eq, Show)
data Q      = Quote   Attrs Inline deriving (Eq, Show)
data Sub    = Subscript Attrs Inline deriving (Eq, Show)
data Sup    = Superscript Attrs Inline deriving (Eq, Show)
data Tt     = TT      Attrs Inline deriving (Eq, Show)
data I      = Italics Attrs Inline deriving (Eq, Show)
data B      = Bold    Attrs Inline deriving (Eq, Show)
data Small  = Small   Attrs Inline deriving (Eq, Show)
data Big    = Big     Attrs Inline deriving (Eq, Show)
-- Object nyi
-- Image
data Img = Image Attrs deriving (Eq, Show)
-- Map nyi
-- data Form = Form Attrs FormContent

data Label = Label Attrs Inline deriving (Eq, Show)
data Input = Input Attrs deriving (Eq, Show)
data Select = Select Attrs [SelectOpt] deriving (Eq, Show)
data SelectOpt = SoG OptGroup | So Option deriving (Eq, Show)
data OptGroup  = Og Attrs [Option] deriving (Eq, Show)
data Option    = Op Attrs deriving (Eq, Show)

data Textarea  = TextArea Attrs deriving (Eq, Show)
-- Fieldset nyi
-- data Fieldset  = Fieldset Attrs FieldsetContent 
data Button = Button Attrs ButtonContent deriving (Eq, Show)

--
-- Table
--
data Table 
    = Table{
          attrs   :: Attrs
        , caption :: Maybe Caption
        , col     :: [ColGroup]
        , thead   :: Maybe THead
        , tbody   :: [TBody]  -- TBody, or naked Tr
        , tfoot   :: Maybe TFoot
        }
    deriving (Eq, Show)
data Caption  = Caption Attrs Inline deriving (Eq, Show)
data ColGroup = ColGroup Attrs [Col] deriving (Eq, Show)
data Col      = Col      Attrs  deriving (Eq, Show)
data THead   = THead Attrs [Tr] deriving (Eq, Show)
data TBody   = TBody Attrs [Tr] deriving (Eq, Show)
data TFoot   = TFoot Attrs [Tr] deriving (Eq, Show)
data Tr      = TrH Attrs Th | TrD Attrs Td deriving (Eq, Show)
data Th      = Th    Attrs Flow deriving (Eq, Show)
data Td      = Td    Attrs Flow deriving (Eq, Show)

-- let tm = fromRawXml "<Patient><active value=\"true\"/>bla<em>blub</em>bla</Patient>"
-- Right [Element "Patient" [] [
--              Element "active" [("value","true")] []
--            , Text "bla"
--            , Element "em" [] [Text "blub"]
--            , Text "bla"
--            ]]
-- Xmlbf.runParser Xmlbf.pChildren $ fromRight [] tm
-- Right [Element "Patient" [] [Element "active" [("value","true")] [],Text "bla",Element "em" [] [Text "blub"],Text "bla"]]

{- 
 - JSON picklers
 - dummy
 -}

instance ToJSON XhtmlDiv where
    toJSON (XhtmlDiv d) = object $
      [
        ("div", String (T.pack (show d)))
      ]
instance FromJSON XhtmlDiv where
    parseJSON (String s) = return $ XhtmlDiv (Div [] [FText s]) 
    parseJSON _ = return $ XhtmlDiv (Div [] [FText "not a string"]) 

{-
 - XML picklers
 -}

toXhtmlDivXml (XhtmlDiv d) = (as, Xmlbf.toXml d)
        where as = HM.fromList $ catMaybes $ fmap toAttr [ Val "xmlns" "http://www.w3.org/1999/xhtml" ]
--              cs = concatMap toElement $
--                   [ 
--                     Prop "div" (Xmlbf.toXml (d))
--                   ]

instance Xmlbf.FromXml XhtmlDiv where
    fromXml = do
       d  <- Xmlbf.fromXml
       return $ XhtmlDiv d


instance Xmlbf.ToXml Div where
    toXml (Div a [(FText t)]) = Xmlbf.text (TL.fromStrict t)
    toXml (Div a fs) = concatMap toElement $
                   [ 
                     Prop "div" (HM.empty, concat (fmap Xmlbf.toXml fs))
                   ]
instance Xmlbf.FromXml Div where
    fromXml = do
       f  <- many $ Xmlbf.fromXml
       return $ Div [] f

instance Xmlbf.ToXml Flow where
    toXml (FText t) = Xmlbf.text (TL.fromStrict t)
    toXml (FBl b) = Xmlbf.toXml b
instance Xmlbf.FromXml Flow where
    fromXml = do
       f  <- fromFText <|> fromFI <|> fromFBl
       return f
       where fromFText = FText <$> fmap TL.toStrict Xmlbf.pText
             fromFI    = FI    <$> Xmlbf.fromXml
             fromFBl   = FBl   <$> Xmlbf.fromXml

instance Xmlbf.ToXml BlockLevel where
    toXml (BlE e) = Xmlbf.toXml e
--    toXml (BlF f) = Xmlbf.toXml f
instance Xmlbf.FromXml BlockLevel where
    fromXml = do
       f  <- Xmlbf.fromXml
       return $ BlE f

instance Xmlbf.ToXml BlockLevelElements where
    toXml (BleP p) = Xmlbf.toXml p
    toXml (BleDiv d) = Xmlbf.toXml d
instance Xmlbf.FromXml BlockLevelElements where
    fromXml = do
       f  <- fromBleP <|> fromBleDiv
       return f
       where fromBleP   = BleP   <$> Xmlbf.pElement "p"   Xmlbf.fromXml
             fromBleDiv = BleDiv <$> Xmlbf.pElement "div" Xmlbf.fromXml

instance Xmlbf.ToXml Inline where
    toXml (IText t) = Xmlbf.text (TL.fromStrict t)
--    toXml (BleDiv d) = Xmlbf.toXml d
instance Xmlbf.FromXml Inline where
    fromXml = do
       f  <- fromIAnchor <|> fromIText
       return f
       where fromIAnchor = IA <$> Xmlbf.fromXml
             fromIText   = IText <$> fmap TL.toStrict Xmlbf.pText

instance Xmlbf.ToXml P where
    toXml (P a is) = concatMap toElement $
                   [ 
                     Prop "p" (HM.empty, concat (fmap Xmlbf.toXml is))
                   ]
instance Xmlbf.FromXml P where
    fromXml = do
       t  <- many $ Xmlbf.pText
       return $ P [] $ fmap (IText . TL.toStrict) t

instance Xmlbf.ToXml Anchor where
    toXml (Anchor a is) = concatMap toElement $
                   [
                   ]
instance Xmlbf.FromXml Anchor where
    fromXml = do
       a <- Xmlbf.pElement "a" (fmap TL.toStrict Xmlbf.pText)
       return $ Anchor [] a

mkXhtmlDiv t = XhtmlDiv $ Div [] [FText t]
