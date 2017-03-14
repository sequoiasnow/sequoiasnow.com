module Api.Types.Web
( Action
, Error
, loggingM
, defaultH
, queryDB
, queryDB_
, executeDB
) where

import Web.Scotty.Trans                     ( ActionT
                                            , ScottyT
                                            , status
                                            , text
                                            , showError )
import Network.Wai                          ( Middleware )
import Network.HTTP.Types.Status            ( internalServerError500 )
import Network.Wai.Middleware.RequestLogger ( logStdout
                                            , logStdoutDev )
import Api.Config                           ( ConfigM
                                            , Config( conn ) )
import Data.Text.Lazy                       ( Text )
import Api.Environment                      ( Environment(..) )
import Control.Monad.IO.Class               ( MonadIO
                                            , liftIO )
import Control.Monad.Trans.Class            ( MonadTrans
                                            , lift )
import Control.Monad.Reader                 ( asks )
import Database.PostgreSQL.Simple           ( FromRow
                                            , ToRow
                                            , Query
                                            , query
                                            , query_
                                            , execute )

-- | The primary action that allows database interaction and
-- error catching.
type Action a = ActionT Error ConfigM a

-- | The error handeling type, simply resolves to text at the
-- moment but it could conceivably be more specialized in the future.
type Error = Text

-- | Handel any type of failure or error smoothly, by returning it in
-- development and hiding it in production.
defaultH :: Environment -> Error -> Action () 
defaultH e x = do
  status internalServerError500
  let r = case e of
        Development -> showError x
        Production  -> ""
  text r

-- | Logging middleware based off of development or production setting.
loggingM :: Environment -> Middleware
loggingM e = case e of
  Development -> logStdoutDev
  Production  -> logStdout


-- | Lift's the database connection into scope and executes a query,
-- with a parameter
queryDB :: (MonadTrans t, MonadIO (t ConfigM), ToRow q, FromRow r) =>
         Query -> q -> t ConfigM [r]
queryDB q args = do
  c <- lift $ asks conn
  liftIO $ query c q args

-- | Lift's the database connection into scope and executes a query
-- without a parameter.
queryDB_ :: (MonadTrans t, MonadIO (t ConfigM), FromRow r) =>
          Query -> t ConfigM [r]
queryDB_ q = do
  c <- lift $ asks conn
  liftIO $ query_ c q

-- | Lift's the database connection into scope and executes a query
-- without a result.
executeDB :: (MonadTrans t, MonadIO (t ConfigM), ToRow q) =>
             Query -> q -> t ConfigM ()
executeDB q args = do
  c <- lift $ asks conn
  liftIO $ execute c q args
  return ()
