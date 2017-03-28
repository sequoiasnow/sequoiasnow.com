{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Api.Server
( runApplication
) where

import Control.Monad.Reader         ( ReaderT
                                    , ask
                                    , asks
                                    , runReaderT )
import Control.Monad.Trans.Class    ( lift )
import Api.Environment              ( Environment )
import Api.Config                   ( ConfigM( runConfigM )
                                    , Config( environment )
                                    , getOptions )
import Api.Types.Web                ( Action
                                    , Application
                                    , loggingM
                                    , defaultH
                                    , Error )
import Api.Types.Post               ( postRoutes )
import Web.Scotty.Trans             ( defaultHandler
                                    , ScottyT
                                    , scottyOptsT
                                    , middleware )
import Network.Wai.Middleware.Cors  ( simpleCors )

-- | The entrypoint to the web application, the one source of all truth for
-- the program.
application :: Application
application = do
  middleware simpleCors
  -- c <- lift (asks environment) :: ScottyT Error ConfigM (Environment)
  -- middleware (loggingM env)
  -- defaultHandler (defaultH env)
  -- routes
  postRoutes
  

-- | Runs the web application, creating a web server on the provided port.
-- Can also be seen as performing the runConfig action of the ConfigM
-- monad.
runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions $ environment c
  let r = \m -> runReaderT (runConfigM m) c
  scottyOptsT o r application

