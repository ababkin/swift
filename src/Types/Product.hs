{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Types.Product where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=))
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import           Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr)
import qualified Data.Map as M
import Data.Maybe (fromJust)


data Product = Product {
    pId       :: Text
  , pPrice    :: Integer
  , pImageUrl :: Text
  , pClickUrl :: Text
  } deriving (Eq, Ord, Show)


instance FromDynItem Product where
  parseItem i = Product
    <$> getAttr "productId" i
    <*> getAttr "price" i
    <*> getAttr "imageUrl" i
    <*> getAttr "clickUrl" i

instance ToDynItem Product where
  toItem Product{pId, pPrice, pImageUrl, pClickUrl} = 
    item [
      attr "productId"  pId
    , attr "price"      pPrice
    , attr "imageUrl"   pImageUrl 
    , attr "clickUrl"   pClickUrl
    ]

instance FromJSON Product where
  parseJSON (Object v) = Product 
    <$> v .: "id"
    <*> v .: "price"
    <*> v .: "imageUrl"
    <*> v .: "clickUrl"
  parseJSON o = typeMismatch "Product" o

instance ToJSON Product where
  toJSON Product{pId, pPrice, pImageUrl, pClickUrl} =
    object  [
              "id"        .= pId 
            , "price"     .= pPrice 
            , "imageUrl"  .= pImageUrl 
            , "clickUrl"  .= pClickUrl
            ]


