{-# LANGUAGE QuasiQuotes #-}
module Api.Types.Post
( getPost
) where

import Control.Applicative                ( (<$>)
                                          , (<*>) )
import Data.Aeson                         ( ToJSON(..)
                                          , FromJSON(..)
                                          , Value(..)
                                          , object
                                          , (.:)
                                          , (.=) )
import Control.Monad                      ( liftM4
                                          , mzero )
import Database.PostgreSQL.Simple         ( FromRow
                                          , ToRow
                                          , Query
                                          , Only(..) )
import Database.PostgreSQL.Simple.ToRow   ( toRow )
import Database.PostgreSQL.Simple.FromRow ( fromRow
                                          , field )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.SqlQQ   ( sql )
import Api.Types.Web                      ( Action
                                          , Error
                                          , queryDB
                                          , queryDB_
                                          , executeDB )
import Network.HTTP.Types.Status          ( status201
                                          , status200 )
import Web.Scotty.Trans                   ( json
                                          , status
                                          , param
                                          , ActionT )
import Data.Time                          ( UTCTime )
import qualified Data.Text as             T

-- Column Types

type PostID    = Int
type PostTitle = T.Text
type PostBody  = T.Text
type PostTag   = T.Text
type PostUrl   = T.Text 
type PostDate  = UTCTime

-- | The post is seperated in two parts. One is given to the database and returned
-- by the server. This is the post datatype. The other is the following postfields
-- datatype which holds all information required to instantiate or update a post,
-- but not enough to edit it.
data PostFields = PostFields
  { post_title :: PostTitle
  , post_body  :: PostBody
  , post_tags  :: [PostTag]
  } deriving (Show)

instance FromJSON PostFields where
  parseJSON (Object v) = PostFields <$> v .: "title"
                                    <*> v .: "body"
                                    <*> v .: "tags"
  parseJSON _ = mzero

instance ToRow PostFields where
  -- | Excludes post_tags as they are added to a different table.
  toRow (PostFields title body _) = [toField title, toField body]

-- | The Post type represents the result of database insert, or update. It can only
-- be converted to JSON and should never be parsed by json.
data Post = Post
  { post_id  :: PostID
  , post_url :: PostUrl
  , post_date :: PostDate
  , post_fields :: PostFields
  } deriving (Show)

instance ToJSON Post where
  toJSON (Post id url date (PostFields title body tags)) =
    object [ "id"    .= id
           , "url"   .= url
           , "date"  .= date
           , "title" .= title
           , "body"  .= body 
           , "tags"  .= tags ]

instance FromRow Post where
  fromRow = do
    id <- field
    url <- field
    date <- field
    title <- field
    body <- field
    return $ Post id url date (PostFields title body [])

-- SQL Queries

-- The SQL to retrieve all posts of a given tag
getPostsWithTagQ :: Query
getPostsWithTagQ =
  [sql| WITH tags AS ( SELECT tag, post_id FROM post_tags WHERE tag = ? )
        SELECT post_id,
               post_url,
               post_date
               post_title,
               post_body,
               (SELECT tag FROM tags) AS post_tags
        FROM posts
        WHERE post_id IN (SELECT post_id FROM tags)|]


-- The SQL to insert a new post from the post fields
insertPostQ :: Query
insertPostQ =
  [sql| INSERT INTO posts
        ( post_id, post_title, post_body )
        VALUES (?, ?, ?)
        RETURNING post_id |]

insertPostTagQ :: Query
insertPostTagQ =
  [sql| INSERT INTO post_tags
        (tag, post_id) VALUES (?,?) |]

getPostQ :: Query
getPostQ =
  [sql| SELECT post_id,
               post_url,
               post_date,
               post_title,
               post_body
        FROM posts
        WHERE post_id = ? |]

getPostTagsQ :: Query
getPostTagsQ =
  [sql| SELECT tag
        FROM post_tags
        WHERE post_id = ? |]

getPostsByTagQ :: Query
getPostsByTagQ =
  [sql| WITH tags AS ( SELECT post_id FROM post_Tags WHERE tag = ? )
        SELECT post_id,
               post_url,
               post_date,
               post_title,
               post_body
        FROM posts
        WHERE post_id IN tags |]

getAllPosts :: Query
getAllPosts = [sql| SELECT post_id,
                           post_url,
                           post_date,
                           post_title,
                           substr(post_body, 0, 300)
                     FROM posts |]
    
-- server actions
getPost :: Action ()
getPost = do
  id <- param "id"
  let idParam = Only (id :: Integer)
  [p] <- queryDB getPostQ idParam
  tags <- queryDB getPostTagsQ idParam
  let tags' = map (\(Only tag) -> tag) tags
  status status200
  json $ p { post_fields = (post_fields p) { post_tags = tags' } }
    
insertPost :: PostFields -> Action ()
insertPost pf = do
  [Only id] <- queryDB insertPostQ pf
  let id' = read . T.unpack $ id :: Int
  mapM_ (executeDB getPostQ) (map (\t -> [id, t]) (post_tags pf))
  [p] <- queryDB getPostQ (Only id') :: Action [Post]
  -- Run the status response.
  status status201
  json (p :: Post)
