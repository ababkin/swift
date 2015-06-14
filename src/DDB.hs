{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DDB where

import           Control.Applicative        ((<$>))
import           Aws (baseConfiguration, Configuration(credentials),
  loadCredentialsFromEnv, debugServiceConfig, simpleAws)
import           Aws.DynamoDb.Commands 
import Aws.Core (Transaction, AsMemoryResponse, MemoryResponse, Credentials)
import Data.Text (Text)
import Data.Either (rights)
import qualified Data.Vector as V
import Aws.DynamoDb.Core (Item, fromValue, toValue, fromItem, Attribute(Attribute))
import Aws.DynamoDb.Commands (query, Slice(Slice), qrItems)
import Data.Either.Unwrap (fromRight)

import           Control.Monad              (forM, forM_)
import           Aws (baseConfiguration, Configuration(credentials),
  loadCredentialsFromEnv, debugServiceConfig, simpleAws)
import           Aws.DynamoDb.Commands (PutItem, PutItemResponse, 
  piReturn, piRetCons, piRetMet, putItem)
import           Aws.DynamoDb.Core (item, attrAs, text, int, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))

{- runWithCreds -}
  {- :: (Transaction r a, AsMemoryResponse a)  -}
  {- => Credentials  -}
  {- -> r  -}
  {- -> IO (MemoryResponse a) -}

{- runWithCreds creds r = do -}
  {- cfg <- Aws.baseConfiguration -}
  {- simpleAws cfg{credentials = creds} debugServiceConfig r -}

runWithCreds r = do
  cfg <- Aws.baseConfiguration
  maybeCreds <- loadCredentialsFromEnv
  maybe
    (return $ Left "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET")
    (\creds -> Right <$> simpleAws cfg{credentials = creds} debugServiceConfig r) 
    maybeCreds



