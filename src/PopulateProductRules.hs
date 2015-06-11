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
import           Aws.DynamoDb.Core (item, attrAs, text, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))
import Data.Aeson (encode)

import Types (ProductRule(..))
import DDB (runWithCreds)

rules = [
    ProductRule "product1" "rule1"
  , ProductRule "product1" "rule2"
  ]

main = forM_ rules $ putProductRule runWithCreds

putProductRule 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> ProductRule 
  -> IO ()
putProductRule runWithCreds productRule = do
  let x = item [ attrAs text "productId" $ prProductId productRule 
               , attrAs text "ruleId" $ prRuleId productRule
               ]

  let req = (putItem "swift-ProductRulesTable-69YP2IKAF308" x ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()



