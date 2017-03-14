module Api.Environment
( Environment(..)
, getEnvironment
, getPort
) where

import System.Environment ( lookupEnv )

-- | The type of server to run.
data Environment
  = Development -- ^ Enables logging
  | Production  -- ^ Disables logging
  deriving (Show, Read, Eq)

-- | Returns the enviroment type by searching for an API_ENV environment
-- variable.
getEnvironment :: IO Environment
getEnvironment = do
  env <- lookupEnv "API_ENV"
  let e = case env of Nothing -> Development
                      Just s  -> read s
  return e          

-- | Looks up the port for the web application to run on, returning a maybe
-- value.
getPort :: IO (Maybe Int)
getPort = do
  mp <- lookupEnv "PORT"
  return $ mp >>= read
