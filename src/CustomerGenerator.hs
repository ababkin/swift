{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module CustomerGenerator where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Text as T

import Types.Customer (Customer(..))
import DDB.Customer (putCustomer)
import DDB (runWithCreds)
import Random (uniformDateInRange, uniformGender, uniformLocation)

generate :: Int -> IO ()
generate num = do
  forM_ [1..num] $ \i -> do
    dob         <- uniformDateInRange "1950-01-01" "2010-01-01"
    gender      <- uniformGender
    (lon, lat)  <- uniformLocation
    putCustomer runWithCreds $ Customer {
      cId     = T.pack $ "customer" ++ show i
    , cDOB    = dob
    , cGender = gender
    , cLon    = lon
    , cLat    = lat
    }
    

