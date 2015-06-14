{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DDB.ProductRule where

import Control.Applicative        ((<$>))
import Aws.DynamoDb.Commands 
import Data.Either (rights)
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import Aws.DynamoDb.Core (fromItem, UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  toItem, ReturnItemCollectionMetrics(RICMSize))

import Types.ProductRule (ProductRule(..))
import DDB (runWithCreds)

productRulesTable = "swift-ProductRulesTable-69YP2IKAF308"

getProductRules :: IO([ProductRule])
getProductRules = do
  (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan productRulesTable)


putProductRules :: [ProductRule] -> IO ()
putProductRules productRules =
  forM_ productRules $ putProductRule runWithCreds

putProductRule 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> ProductRule 
  -> IO ()
putProductRule runWithCreds productRule = do
  let req = (putItem productRulesTable $ toItem productRule ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()


