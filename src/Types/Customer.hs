{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Types.Customer where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=))
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import           Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr, attrAs, text)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Char (toLower)

import Types


data Customer = Customer {
    cId     :: Text
  , cDOB    :: Day
  , cGender :: Gender
  , cLon    :: Double
  , cLat    :: Double
  } deriving Show

{- instance DynVal Day where -}
  {- type DynRep Day = DynString -}
  {- toRep = DynString . T.pack . formatTime defaultTimeLocale "%D" -}
  {- fromRep = parseTime defaultTimeLocale "%D" . T.unpack . unDynString -}


instance FromDynItem Customer where
  parseItem i = Customer
    <$> getAttr "customerId" i
    <*> getAttr "dob" i
    <*> getAttr "gender" i
    <*> getAttr "lon" i
    <*> getAttr "lat" i

instance ToDynItem Customer where
  toItem Customer{cId, cDOB, cGender, cLon, cLat} = 
    item [
      attr "customerId"   cId
    , attr "dob"    cDOB 
    , attr "gender" cGender 
    , attr "lon"    cLon 
    , attr "lat"    cLat
    ]


instance FromJSON Customer where
  parseJSON (Object v) = Customer 
    <$> v .: "id"
    <*> (fromJust . parseTime defaultTimeLocale "%F" <$> (v .: "dob"))
    <*> (readGender <$> v .: "gender")
    <*> v .: "lon"
    <*> v .: "lat"
  parseJSON o = typeMismatch "Customer" o

instance ToJSON Customer where
  toJSON Customer{cId, cDOB, cGender, cLon, cLat} =
    object  [
              "id"      .= cId 
            , "dob"     .= formatTime defaultTimeLocale "%F" cDOB 
            , "gender"  .= (map toLower . show $ cGender)
            , "lon"     .= cLon 
            , "lat"     .= cLat
            ]

readGender gender = if map toLower gender == "male" 
                      then Male 
                      else Female


