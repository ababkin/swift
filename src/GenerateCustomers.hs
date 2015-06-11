{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Monad              (forM, forM_)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
{- import           System.Environment         (getArgs) -}
import           Aws
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core
import Data.Maybe (fromJust)

import Types (Customer(..))
import DDB (runWithCreds)

main = do
  forM_ [
      Customer "cust1" 22 112.3323 (-22.332)
    , Customer "cust2" 45 112.4554 23.4345
    ] 
    (putCustomer runWithCreds)


putCustomer 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Customer 
  -> IO ()
putCustomer runWithCreds Customer{cId, cAge, cLon, cLat} = do
  let x = item [ attrAs text    "customerId"  cId
               , attrAs int     "age"         cAge 
               , attrAs double  "lon"         cLon
               , attrAs double  "lat"         cLat
               ]

  let req1 = (putItem "swift-CustomersTable-DXE4T6L786O7" x ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  resp1 <- runWithCreds req1
  {- print resp1 -}
  return ()



