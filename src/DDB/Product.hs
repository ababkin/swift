{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DDB.Product where

import Control.Applicative        ((<$>))
import Aws.DynamoDb.Commands 
import Data.Either (rights)
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import Aws.DynamoDb.Core (fromItem, UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  toItem, ReturnItemCollectionMetrics(RICMSize), toValue, fromItem, 
  Attribute(Attribute))

import Data.Text (Text)

import Types.Product (Product(..))
import DDB (runWithCreds)

productsTable = "swift-ProductsTable-12LEX2YDJZDWR"
eligibleProductsTable = "swift-EligibleProductsTable-125F2ICF4TAII"

getProducts :: IO([Product])
getProducts = do
  (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan productsTable)

getEligibleProducts :: Text -> IO ([Product]) 
getEligibleProducts customerId = do
  fmap (rights . map fromItem . V.toList . qrItems) $ fmap fromRight $ do
    runWithCreds $ query eligibleProductsTable 
                        (Slice (Attribute "customerId" (toValue customerId)) Nothing) 


putProducts :: [Product] -> IO ()
putProducts products =
  forM_ products $ putProduct runWithCreds
  

putProduct 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Product 
  -> IO ()
putProduct runWithCreds product = do
  let req = (putItem productsTable $ toItem product ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()
