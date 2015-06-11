{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Types where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=), encode, decode)
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import           Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem)
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text                  as T

data Customer = Customer {
    cId   :: Text
  , cAge  :: Integer
  , cLon  :: Double
  , cLat  :: Double
  } deriving Show

instance FromDynItem Customer where
  parseItem i = return $ fromJust $ Customer
    <$> (fromValue =<< M.lookup "customerId" i)
    <*> (fromValue =<< M.lookup "age" i)
    <*> (fromValue =<< M.lookup "lon" i)
    <*> (fromValue =<< M.lookup "lat" i)

{- instance ToDynItem Customer where -}
  {- toItem Customer{cId, cAge, cLon, cLat} =  -}
    {- toItem $ M.fromList [("id", toValue cId), ("age", toValue cAge), ("lon", toValue cLon), ("lat", toValue cLat)] -}

    
data Product = Product {
    pId       :: Text
  , pPrice    :: Integer
  , pImageUrl :: Text
  , pClickUrl :: Text
  } deriving (Eq, Ord, Show)

instance FromDynItem Product where
  parseItem i = return $ fromJust $ Product
    <$> (fromValue =<< M.lookup "productId" i)
    <*> (fromValue =<< M.lookup "price" i)
    <*> (fromValue =<< M.lookup "imageUrl" i)
    <*> (fromValue =<< M.lookup "clickUrl" i)


{- data ProductRule = ProductRule { -}
    {- prProduct :: Product -}
  {- , prRule    :: Rule -}
  {- } deriving Show -}

data ProductRule = ProductRule {
    prProductId :: Text
  , prRuleId    :: Text
  } deriving Show

instance FromDynItem ProductRule where
  parseItem i = return $ fromJust $ ProductRule
    <$> (fromValue =<< M.lookup "productId" i)
    <*> (fromValue =<< M.lookup "ruleId" i)


data Rule = InRange {
    rId   :: Text
  , rAttr :: Text
  , rMin  :: Integer
  , rMax  :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON Rule where
  parseJSON o@(Object v) = do
    (ruleType :: String) <- v .: "type"
    case ruleType of
      "inRange" -> InRange
                    <$> v .: "id"
                    <*> v .: "attr"
                    <*> v .: "min"
                    <*> v .: "max"
      _ -> typeMismatch "Rule" o
  parseJSON o = typeMismatch "Rule" o

instance ToJSON Rule where
  toJSON InRange{rId, rAttr, rMin, rMax} =
    object  [
              "type"  .= ("inRange" :: Text)
            , "id"    .= rId 
            , "attr"  .= rAttr 
            , "min"   .= rMin 
            , "max"   .= rMax
            ]

instance DynVal Rule where
  type DynRep Rule = DynString
  toRep = DynString . T.pack . BL.unpack . encode 
  fromRep = decode . BL.pack . T.unpack . unDynString

