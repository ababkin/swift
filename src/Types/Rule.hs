{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances        #-}

module Types.Rule where

import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=), encode, decode)
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
import Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T

import Types (Gender(..), CustomerAttr(..))

data Rule = InDateRange {
    rId   :: Text
  , rAttr :: CustomerAttr
  , rMin  :: Day
  , rMax  :: Day
  } | EqualsGender {
    rId     :: Text
  , rAttr   :: CustomerAttr
  , rValue  :: Gender
  } deriving (Eq, Ord, Show)


{- instance FromDynItem Rule where -}
  {- parseItem i = Rule -}
    {- <$> getAttr "ruleId" i -}
    {- <*> getAttr "json" i -}

instance ToDynItem Rule where
  toItem rule = 
    item [
      attr "ruleId" $ rId rule
    , attr "json" $ T.pack . BL.unpack . encode $ rule
    ]


instance FromJSON Rule where
  parseJSON o@(Object v) = do
    (ruleType :: String) <- v .: "type"
    (ruleAttr :: CustomerAttr) <- v .: "attr"
    case (ruleType, ruleAttr) of
      ("inRange", DOBAttr) -> InDateRange
        <$> v .: "id"
        <*> pure DOBAttr 
        <*> (fromJust . parseTime defaultTimeLocale "%F" <$> (v .: "min"))
        <*> (fromJust . parseTime defaultTimeLocale "%F" <$> (v .: "max"))
      ("equals", GenderAttr) -> EqualsGender
        <$> v .: "id"
        <*> pure GenderAttr
        <*> v .: "value"
      _ -> typeMismatch "suitable combination of rule type and attr to construct Rule" o
  parseJSON o = typeMismatch "Rule" o

instance ToJSON Rule where
  toJSON InDateRange{rId, rAttr, rMin, rMax} =
    object  [
              "id"    .= rId 
            , "type"  .= ("inRange" :: Text)
            , "attr"  .= rAttr 
            , "min"   .= show rMin 
            , "max"   .= show rMax
            ]
  toJSON EqualsGender{rId, rAttr, rValue} =
    object  [
              "id"    .= rId 
            , "type"  .= ("equals" :: Text)
            , "attr"  .= rAttr 
            , "value" .= rValue
            ]

instance DynVal Rule where
  type DynRep Rule = DynString
  toRep = DynString . T.pack . BL.unpack . encode 
  fromRep = decode . BL.pack . T.unpack . unDynString


