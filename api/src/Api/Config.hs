{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api.Config
( ConfigM(..)
, Config(..)
, getOptions
, getConfig
) where

import Api.Environment                  ( getEnvironment
                                        , getPort
                                        , getEnvironment
                                        , Environment(..) )
import Api.Database                     ( getConnection )
import Network.Wai.Handler.Warp         ( Settings(..)
                                        , defaultSettings
                                        , setFdCacheDuration
                                        , setPort )
import Web.Scotty.Trans                 ( Options(..) )
import Database.PostgreSQL.Simple       ( Connection )
import Control.Monad.Reader             ( ReaderT )
import Control.Monad.Reader.Class       ( MonadReader )
import Control.Monad.IO.Class           ( MonadIO )


-- | Reades the origional environment from env variables. and constructs
-- the config.
getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  c <- getConnection e
  return $ Config
    { environment = e
    , conn = c
    }

-- | Construct the settings for the web application. Disables caching
-- in development mode.
getSettings :: Environment -> IO Settings
getSettings e = do
  let s  = defaultSettings
  let s' = case e of
        Development -> setFdCacheDuration 0 s
        Production  -> s
  mp <- getPort
  return $ case mp of
    Nothing -> s'
    Just p  -> setPort p s'

-- | Construct the necessary arguments for scotty options.
getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return Options
    { settings = s
    , verbose  = case e of
        Development -> 1
        Production  -> 0
    }
  

-- | Config holds information about the enviroment as well as the database
-- connection.
data Config = Config
  { environment :: Environment -- ^ Either Development or Production
  , conn :: Connection -- ^ The database connection.
  }

-- | The ConfigM monad contains the database information and is the primary
-- type used throught the aplication. This uses GeneralizedNewTypeDeriving.
newtype ConfigM a = ConfigM
   { runConfigM :: ReaderT Config IO a
   } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

