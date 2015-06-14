{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DDB.ApplyRules where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Monad ((<=<))
import Data.Either (rights)
import Data.Either.Unwrap (fromRight)
import Data.Maybe (fromJust, catMaybes)
import Aws.DynamoDb.Commands (srItems, scan, piReturn, piRetCons, piRetMet,
  putItem)
import Aws.DynamoDb.Core (UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize), 
  item, attrAs, int, text, fromValue, fromItem)

import Types.Customer (Customer(..))
import Types.Product (Product(..))
import Types.Rule (Rule(..))
import Types.ProductRule (ProductRule(..))
import DDB (runWithCreds)

customersTable = "swift-CustomersTable-DXE4T6L786O7"
productsTable = "swift-ProductsTable-12LEX2YDJZDWR"
rulesTable = "swift-RulesTable-1JYAWXXAXKDSA"
productRulesTable = "swift-ProductRulesTable-69YP2IKAF308"
eligibleProductsTable = "swift-EligibleProductsTable-125F2ICF4TAII"

data ProductWithRules = ProductWithRules {
    prProduct :: Product
  , prRules   :: [Rule]
  } deriving Show

applyRules = do
  -- add pagination / parallel scanning to customers
  customers <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan customersTable)
  
  products  <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan productsTable)
  rules     <- (map (fromJust . (fromValue <=< M.lookup "json")) . V.toList . srItems) <$> fromRight <$> runWithCreds (scan rulesTable)
  productRules  <- (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan productRulesTable)
  mapM_ (\customer -> doCustomer customer (consolidate products rules productRules) ) customers

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
    satisfy Customer{cDOB} InDateRange{rMin, rMax} =
      cDOB >= rMin && cDOB <= rMax
    satisfy Customer{cGender} EqualsGender{rValue} =
      cGender == rValue
    satisfy customer rule =
      error $ "Fatal: rule " ++ show rule ++ " for customer " ++ show customer ++ "is not implemented!"

    checkEligibility
      :: Customer
      -> ProductWithRules
      -> Maybe Product
    checkEligibility customer ProductWithRules{prProduct, prRules} =
      if all (satisfy customer) prRules
        then Just prProduct
        else Nothing
      

    putEligibleProducts Customer{cId} products = do
      forM_ products $ \Product{pId, pPrice, pImageUrl, pClickUrl} -> do
        let x = item [ 
                  attrAs text  "customerId" cId
                , attrAs text  "productId"  pId
                , attrAs int   "price"      pPrice 
                , attrAs text  "imageUrl"   pImageUrl
                , attrAs text  "clickUrl"   pClickUrl
                ]
        let req = (putItem eligibleProductsTable x ) { 
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
        







