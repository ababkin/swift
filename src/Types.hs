{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Types where

import Control.Applicative (pure)
import Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Char (toLower)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), (.=), encode, decode)
import Data.Aeson.Types (typeMismatch)  
import qualified Data.Text as T

data Gender = Male | Female 
  deriving (Eq, Ord, Read, Show, Data, Typeable, Enum)

instance DynVal Gender where
  type DynRep Gender = DynString
  toRep = DynString . T.pack . show 
  fromRep v = case T.unpack $ unDynString v of
    "Male" -> Just Male
    "Female" -> Just Female
    other -> error $ "unexpected gender: " ++ other

instance FromJSON Gender where
  parseJSON (String "male")   = pure Male 
  parseJSON (String "female") = pure Female 
  parseJSON o = typeMismatch "Gender" o

instance ToJSON Gender where
  toJSON Male   = String "male"
  toJSON Female = String "female"


data CustomerAttr = 
    GenderAttr 
  | DOBAttr 
  | LonLatAttr
  deriving (Eq, Ord, Show)

instance FromJSON CustomerAttr where
  parseJSON (String "gender") = pure GenderAttr
  parseJSON (String "dob")    = pure DOBAttr
  parseJSON (String "lonlat") = pure LonLatAttr
  parseJSON o = typeMismatch "CustomerAttr" o

instance ToJSON CustomerAttr where
  toJSON GenderAttr = String "gender"
  toJSON DOBAttr    = String "dob"
  toJSON LonLatAttr = String "lonlat"

