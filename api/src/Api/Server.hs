{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Api.Server
( runApplication
) where

import Control.Monad.Reader       ( ReaderT
                                  , ask
                                  , asks
                                  , runReaderT )
import Control.Monad.Trans.Class  ( lift )
import Api.Environment            ( Environment )
import Api.Config                 ( ConfigM( runConfigM )
                                  , Config( environment )
                                  , getOptions )
import Api.Types.Web              ( Action
                                  , loggingM
                                  , defaultH
                                  , Error )
import Api.Types.Post             ( getPost
                                  , insertPost
                                  , getAllPosts )
import Web.Scotty.Trans           ( defaultHandler
                                  , ScottyT
                                  , scottyOptsT
                                  , middleware
                                  , get
                                  , post )

-- | The entrypoint to the web application, the one source of all truth for
-- the program.
application :: ScottyT Error ConfigM ()
application = do
  -- c <- lift (asks environment) :: ScottyT Error ConfigM (Environment)
  -- middleware (loggingM env)
  -- defaultHandler (defaultH env)
  -- routes
  get "/posts" $ getAllPosts
  get "/posts/:id" $ getPost
  post "/posts" $ insertPost 

-- | Runs the web application, creating a web server on the provided port.
-- Can also be seen as performing the runConfig action of the ConfigM
-- monad.
runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions $ environment c
  let r = \m -> runReaderT (runConfigM m) c
  scottyOptsT o r application

