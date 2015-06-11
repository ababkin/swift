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
import           Aws (baseConfiguration, Configuration(credentials),
  loadCredentialsFromEnv, debugServiceConfig, simpleAws)
import           Aws.DynamoDb.Commands (PutItem, PutItemResponse, 
  piReturn, piRetCons, piRetMet, putItem)
import           Aws.DynamoDb.Core (item, attrAs, text, int, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))
import Data.Aeson (encode)

import Types (Product(..))
import DDB (runWithCreds)

products = [
    Product "product1" 5 "http://image1" "http://click1"
  , Product "product2" 10 "http://image2" "http://click2"
  ]

main = forM_ products $ putProduct runWithCreds

putProduct 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Product 
  -> IO ()
putProduct runWithCreds Product{pId, pPrice, pImageUrl, pClickUrl} = do
  let x = item [ attrAs text  "productId"  pId
               , attrAs int   "price"      pPrice 
               , attrAs text  "imageUrl"   pImageUrl
               , attrAs text  "clickUrl"   pClickUrl
               ]

  let req1 = (putItem "swift-ProductsTable-12LEX2YDJZDWR" x ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req1
  return ()



