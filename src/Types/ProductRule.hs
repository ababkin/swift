{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Types.ProductRule where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=))
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import           Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr)
import qualified Data.Map as M
import Data.Maybe (fromJust)


data ProductRule = ProductRule {
    prProductId :: Text
  , prRuleId    :: Text
  } deriving Show


instance FromDynItem ProductRule where
  parseItem i = ProductRule
    <$> getAttr "productId" i
    <*> getAttr "ruleId" i

instance ToDynItem ProductRule where
  toItem ProductRule{prProductId, prRuleId} = 
    item [
      attr "productId"  prProductId
    , attr "ruleId"     prRuleId
    ]


instance FromJSON ProductRule where
  parseJSON (Object v) = ProductRule 
    <$> v .: "productId"
    <*> v .: "ruleId"
  parseJSON o = typeMismatch "Product" o

instance ToJSON ProductRule where
  toJSON ProductRule{prProductId, prRuleId} =
    object  [
              "productId" .= prProductId 
            , "ruleId"    .= prRuleId 
            ]
