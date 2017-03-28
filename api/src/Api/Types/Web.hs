module Api.Types.Web
( Action
, Application
, Error
, loggingM
, defaultH
, queryDB
, queryDB_
, executeDB
, notFoundA
) where

import GHC.Int                              ( Int64 )
import Web.Scotty.Trans                     ( ActionT
                                            , ScottyT
                                            , status
                                            , text
                                            , showError )
import Network.Wai                          ( Middleware )
import Network.HTTP.Types.Status            ( internalServerError500 )
import Network.Wai.Middleware.RequestLogger ( logStdout
                                            , logStdoutDev )
import Network.HTTP.Types.Status            ( notFound404 )
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

-- | The primary type of the actual scotty application run by scottyOptsT
type Application = ScottyT Error ConfigM ()

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

-- | Makes a not found status and error.
notFoundA :: Action ()
notFoundA = do
  status notFound404

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
             Query -> q -> t ConfigM Int64
executeDB q args = do
  c <- lift $ asks conn
  liftIO $ execute c q args
