module DDB where

import           Control.Applicative        ((<$>))
import           Aws (baseConfiguration, Configuration(credentials),
  loadCredentialsFromEnv, debugServiceConfig, simpleAws)
import           Aws.DynamoDb.Commands 
import Aws.Core (Transaction, AsMemoryResponse, MemoryResponse, Credentials)
  

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


