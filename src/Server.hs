{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status201)
import Data.Text (Text)

import DDB.Customer (putCustomers, getCustomers)
import DDB.Product (getProducts, getEligibleProducts, 
  putProducts)
import DDB.Rule (putRules, getRules)
import DDB.ProductRule (putProductRules, getProductRules)
import DDB.ApplyRules (applyRules)
import Random (evalShuffle)
import Data.Text.Lazy (fromStrict)
import CustomerGenerator (generate)

numProductsInSubset = 3

main :: IO ()
main = scotty 3000 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    get "/customers" $ do
      json =<< (liftIO getCustomers)

    post "/customers" $ do
      (liftIO . putCustomers) =<< jsonData
      status status201

    post "/customers/generate/:num" $ do
      (liftIO . generate) =<< param "num"
      status status201


    get "/products" $ do
      json =<< (liftIO getProducts)

    post "/products" $ do
      (liftIO . putProducts) =<< jsonData
      status status201


    get "/rules" $ do
      text . fromStrict =<< (liftIO getRules)

    post "/rules" $ do
      (liftIO . putRules) =<< body
      status status201


    get "/productRules" $ do
      json =<< (liftIO getProductRules)

    post "/productRules" $ do
      (liftIO . putProductRules) =<< jsonData
      status status201


    post "/applyRules" $ do
      liftIO applyRules
      status status201


    get "/eligibleProducts/:customerId" $ do
      (customerId :: Text) <- param "customerId"
      json =<< liftIO ( take numProductsInSubset `fmap` (evalShuffle =<< getEligibleProducts customerId) )
  

