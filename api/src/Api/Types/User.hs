{-# LANGUAGE QuasiQuotes #-}
module Api.Types.User
( authenticate ) where

import Control.Applicative                  ( (<$>)
                                            , (<*>) )
import Database.PostgreSQL.Simple           ( FromRow
                                            , Query
                                            , Only(..) )
import Database.PostgreSQL.Simple.SqlQQ     ( sql )
import Database.PostgreSQL.Simple.FromRow   ( fromRow
                                            , field )
import Api.Types.Web                        ( Action
                                            , queryDB )
import Web.Scotty.Trans                     ( header
                                            , status )
import Network.HTTP.Types.Status            ( status400
                                            , status401
                                            , status500 )
import qualified Data.Text.Lazy as         T

type UserToken = String
type UserName = String

-- | The user for our simple haskell applications. These can only
-- be created in the database.
data User = User
  { user_token :: UserToken
  , user_name :: UserName
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field

tokenExists :: Query
tokenExists = [sql|SELECT token,username
                   FROM users
                   WHERE token = ? |]

-- | Authenticates a user based off of the user token in the hash.
-- Only if the secure token is provieded is the user allowed to proceed.
authenticate :: Action () -> Action ()
authenticate a = do
  token <- header "Authorization"
  case token of
    Nothing     -> status status400
    Just token' -> do
      let token'' = T.unpack token'
      mbUser <- queryDB tokenExists $ Only (token'' :: UserToken)
      case mbUser of
        [user] -> if (user_token user) == token'' then a else status status500
        _      -> status status401
