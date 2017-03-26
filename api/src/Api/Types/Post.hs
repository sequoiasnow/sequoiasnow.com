{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Types.Post
( getPost
, insertPost
, getAllPosts
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
import Network.HTTP.Types.Status          ( status202
                                          , status201
                                          , status200 )
import Web.Scotty.Trans                   ( json
                                          , jsonData
                                          , status
                                          , param
                                          , ActionT )
import Data.Time                          ( UTCTime )
import Api.Types.Web                      ( notFoundA )
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

-- | Conveniance Function for updating the nested record of a
-- post with a post_tags.
setPostTags :: Post -> [PostTag] -> Post
setPostTags p tags = p { post_fields = (post_fields p) { post_tags = tags } }

-- SQL Queries

-- | The SQL to retrieve all posts of a given tag
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


-- | The SQL to insert a new post from the post fields
insertPostQ :: Query
insertPostQ =
  [sql| INSERT INTO posts
        ( post_title, post_body )
        VALUES (?, ?)
        RETURNING post_id |]

-- | The SQL to insert a post tag, just one into the database.
insertPostTagQ :: Query
insertPostTagQ =
  [sql| INSERT INTO post_tags
        (tag, post_id)
        VALUES (?,?) |]

-- | The SQL to remove all tags assocaited with a post from the
-- post tags table. This must be run before deletePostQ.
deletePostTagsQ :: Query
deletePostTagsQ =
  [sql| DELETE FROM post_tags
        WHERE post_id = ? |]
    
-- | The SQL to delete the post at an index
deletePostQ :: Query
deletePostQ =
  [sql| DELETE FROM posts
        WHERE post_id = ? |]
    
-- | The SQL to retrieve a post with an id 
getPostQ :: Query
getPostQ =
  [sql| SELECT post_id,
               post_url,
               post_date,
               post_title,
               post_body
        FROM posts
        WHERE post_id = ? |]

-- | The SQL to retrieve all tags associated with post id
getPostTagsQ :: Query
getPostTagsQ =
  [sql| SELECT tag
        FROM post_tags
        WHERE post_id = ? |]

-- | The SQL to retreive all posts associated with a give tag.
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

-- | The SQL to return an array of posts which contain such modifications
-- as excerpts for their bodies in place of full content.
getAllPostsQ :: Query
getAllPostsQ = [sql| SELECT post_id,
                           post_url,
                           post_date,
                           post_title,
                           substr(post_body, 0, 300)
                     FROM posts |]
    
-- server actions
  
-- | Conveniance function to return all post tags given an id
getPostTags :: PostID -> Action [PostTag]
getPostTags id = do
  tags <- queryDB getPostTagsQ (Only id)
  return $ map (\(Only tag) -> tag) tags

-- | GET /posts/:id
getPost :: Action ()
getPost = do
  id <- param "id"
  let idParam = Only (id :: PostID)
  mbPost <- queryDB getPostQ idParam
  tags <- getPostTags id
  case mbPost of
    [p] -> do 
      status status200
      json $  setPostTags p tags
    _   -> notFoundA

-- | GET /posts, or optionally GET /posts?tag=3
getAllPosts :: Action ()
getAllPosts = do
  posts  <- queryDB_ getAllPostsQ :: Action [Post]
  posts' <- mapM (\p -> do tags <- getPostTags (post_id p)
                           return $ setPostTags p tags ) posts
  status status201
  json posts'

  
-- | POST /posts                                                   
insertPost :: Action ()
insertPost = do
  pf <- jsonData :: Action PostFields
  [Only (id :: PostID)] <- queryDB insertPostQ pf
  mapM_ (executeDB insertPostTagQ) (map (\t -> (t, id)) (post_tags pf))
  mbPost <- queryDB getPostQ (Only id) :: Action [Post]
  tags <- getPostTags id
  -- Run the status response.
  case mbPost of
    [p] -> do
      status status201
      json $ setPostTags p tags
    _   -> notFoundA

-- | DELETE /posts/:id
deletePost :: Action ()
deletePost = do
  id <- param "id"
  let id' = read . T.unpack $ id :: PostID
  executeDB deletePostTagsQ (Only id')
  a <- executeDB deletePostQ (Only id')
  if a == 1
    then status status202
    else notFoundA
    
