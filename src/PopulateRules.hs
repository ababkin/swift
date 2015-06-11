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

import Types (Rule(..))
import DDB (runWithCreds)

rules = [
    InRange "rule1" "age" 10 20 
  , InRange "rule2" "age" 20 30 
  ]

main = forM_ rules $ putRule runWithCreds

putRule 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Rule 
  -> IO ()
putRule runWithCreds rule = do
  let x = item [ attrAs text "ruleId" $ rId rule 
               , attrAs text "json"   $ T.pack . BL.unpack . encode $ rule
               ]

  let req = (putItem "swift-RulesTable-1JYAWXXAXKDSA" x ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()


