module Api.Database
( getConnection
) where

import System.Environment                 ( lookupEnv )
import Database.PostgreSQL.Simple         ( connectPostgreSQL
                                          , postgreSQLConnectionString
                                          , ConnectInfo(..)
                                          , Connection )
import Api.Environment                    ( Environment(..) )
import Data.Maybe                         ( fromMaybe
                                          , maybe )
import Data.Word                          ( Word16 )
  
import qualified Data.ByteString.Char8 as BS

import Api.Environment (Environment(..))

-- | Return the appropriate connection string to the database based off of
-- the current envrionment and the variables either "DATABASE_URL" or
-- "DATABASE_NAME", "DATABASE_PASS", "DATABASE_USER", "DATABASE_NAME",
-- "DATABASE_PORT", "DATABASE_HOST"
getConnectionString :: Environment -> IO BS.ByteString
getConnectionString e = do
  let defaultDB = case e of Development -> "snow_public" -- could change name
                            Production  -> "snow_public" -- could change name
  -- Get all necessary environemnt variables.
  mb_url  <- lookupEnv "DATABASE_URL"
  mb_host <- lookupEnv "DATABASE_HOST"
  mb_name <- lookupEnv "DATABASE_NAME"
  mb_pass <- lookupEnv "DATABASE_PASS"
  mb_user <- lookupEnv "DATABASE_USER"
  mb_port <- lookupEnv "DATABASE_PORT"
  -- Return either the url or the connection info stringified.
  return $ maybe (postgreSQLConnectionString ConnectInfo
    { connectHost     = fromMaybe "localhost" mb_host
    , connectPort     = toEnum $ (read (fromMaybe "5432" mb_port) :: Int) :: Word16
    , connectUser     = fromMaybe "snow_public_user" mb_user
    , connectPassword = fromMaybe "beammeup" mb_pass
    , connectDatabase = fromMaybe defaultDB mb_name
    }) BS.pack mb_url

-- | Connect to the appropriate database based off the current environment.
getConnection :: Environment -> IO Connection
getConnection e = do
  connStr <- getConnectionString e
  connectPostgreSQL connStr
