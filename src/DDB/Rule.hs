{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DDB.Rule where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Aws.DynamoDb.Commands 
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import Aws.DynamoDb.Core (UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  toItem, ReturnItemCollectionMetrics(RICMSize), fromValue)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import Data.Text (Text)
import Data.Aeson (encode, decode, fromJSON, Value(Object, Number, String), Result(Success, Error))

import Types.Rule (Rule(..))
import DDB (runWithCreds)

rulesTable = "swift-RulesTable-1JYAWXXAXKDSA"

getRules :: IO (Text)
getRules = do
  (jsons :: [Text]) <- (map (fromJust . (fromValue <=< M.lookup "json")) . V.toList . srItems) <$> fromRight <$> runWithCreds (scan rulesTable) 
  return . T.pack . BL.unpack . encode $ jsons 

putRules :: BL.ByteString -> IO ()
putRules rules = do
  let (jsonRules :: [Value]) = fromJust $ decode rules
  forM_ jsonRules $ putRule runWithCreds   

putRule 
  :: (PutItem -> IO (Either String PutItemResponse)) 
  -> Value 
  -> IO ()
putRule runWithCreds jsonRule = do
  case fromJSON jsonRule of
    Success (rDay :: Rule) -> do
      let req = (putItem rulesTable . toItem $ rDay) { 
          piReturn  = URAllOld
        , piRetCons = RCTotal
        , piRetMet  = RICMSize
        }

      resp <- runWithCreds req
      return ()

    Error err -> 
      putStrLn $ "error: " ++ err

