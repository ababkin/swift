{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forever)
import Control.Lens ((^.))
import Network.Wreq
import System.Random (randomRIO)

host = "http://localhost:3000"
endpoint = "/eligibleProducts"

main :: IO ()
main = forever $ do
  (i :: Int) <- randomRIO (1, 100)
  r <- get $ host ++ endpoint ++ "/customer" ++ show i
  case r ^. responseStatus . statusCode of
    200 -> return ()
    otherCode -> error $ "returned unexpected code: " ++ show otherCode
