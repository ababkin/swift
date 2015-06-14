{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DDB.Customer where

import Control.Applicative        ((<$>))
import Aws.DynamoDb.Commands 
import Data.Either (rights)
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import Aws.DynamoDb.Core (fromItem, toItem, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))

import Types.Customer (Customer(..))
import DDB (runWithCreds)

customerTable = "swift-CustomersTable-DXE4T6L786O7"

getCustomers :: IO([Customer])
getCustomers = do
  (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan customerTable)


putCustomers :: [Customer] -> IO ()
putCustomers customers =
  forM_ customers $ putCustomer runWithCreds

putCustomer 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Customer 
  -> IO ()
putCustomer runWithCreds customer = do
  let req = (putItem customerTable $ toItem customer ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()

