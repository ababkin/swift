{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Aws (baseConfiguration, Configuration(credentials),
  loadCredentialsFromEnv, debugServiceConfig, simpleAws)
import           Aws.DynamoDb.Commands
import           Aws.DynamoDb.Core (Item, fromValue, toValue, fromItem)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad ((<=<))
import Data.Either (rights)
import Data.Either.Unwrap (fromRight)
import Data.Maybe (fromJust, catMaybes)
import           Aws.DynamoDb.Core (item, attrAs, text, int, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))

import Types (Customer(..), Product(..), Rule(..), ProductRule(..))
import DDB (runWithCreds)

data ProductWithRules = ProductWithRules {
    prProduct :: Product
  , prRules   :: [Rule]
  } deriving Show

main = do
  let doIt creds = do
        customers <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan "swift-CustomersTable-DXE4T6L786O7")
        products  <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan "swift-ProductsTable-12LEX2YDJZDWR")
        rules     <- (map (fromJust . (fromValue <=< M.lookup "json")) . V.toList . srItems) <$> fromRight <$> runWithCreds (scan "swift-RulesTable-1JYAWXXAXKDSA")
        productRules  <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan "swift-ProductRulesTable-69YP2IKAF308")
        mapM_ (\customer -> doCustomer customer (consolidate products rules productRules) ) customers

  maybeCreds <- loadCredentialsFromEnv
  maybe
    (putStrLn "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET")
    doIt
    maybeCreds


  where
    consolidate
      :: [Product] 
      -> [Rule] 
      -> [ProductRule] 
      -> [ProductWithRules]
    consolidate products rules productRules =
      let 
        productMap = M.fromList $ zip (map pId products) products
        ruleMap = M.fromList $ zip (map rId rules) rules
      in
        map (uncurry ProductWithRules) . M.toList . M.fromListWith (++) $ 
          map (\ProductRule{prProductId, prRuleId} -> 
            (fromJust $ M.lookup prProductId productMap, [fromJust $ M.lookup prRuleId ruleMap]) ) productRules

    satisfy 
      :: Customer
      -> Rule
      -> Bool
    satisfy Customer{cAge, cLon, cLat} InRange{rAttr, rMin, rMax} =
      case rAttr of
        "age" -> cAge >= rMin && cAge <= rMax

    checkEligibility
      :: Customer
      -> ProductWithRules
      -> Maybe Product
    checkEligibility customer ProductWithRules{prProduct, prRules} =
      if all (satisfy customer) prRules
        then Just prProduct
        else Nothing
      
    putEligibleProducts 
      :: Customer
      -> [Product]
      -> IO ()
    putEligibleProducts Customer{cId} products = do
      forM_ products $ \Product{pId, pPrice, pImageUrl, pClickUrl} -> do
        let x = item [ 
                  attrAs text  "customerId" cId
                , attrAs text  "productId"  pId
                , attrAs int   "price"      pPrice 
                , attrAs text  "imageUrl"   pImageUrl
                , attrAs text  "clickUrl"   pClickUrl
                ]
        let req = (putItem "swift-EligibleProductsTable-125F2ICF4TAII" x ) { 
            piReturn  = URAllOld
          , piRetCons = RCTotal
          , piRetMet  = RICMSize
          }

        eitherResp <- runWithCreds req
        return ()

    doCustomer 
      :: Customer
      -> [ProductWithRules]
      -> IO ()
    doCustomer customer = putEligibleProducts customer . catMaybes . map (checkEligibility customer)
      {- getItem "devel-1" (hk "name" "josh") -}
        






