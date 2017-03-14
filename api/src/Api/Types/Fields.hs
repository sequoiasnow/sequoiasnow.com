module Fields where

import Data.Aeson ( FromJSON
                  , ToJSON
                  )
import Data.Time.Clock

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Fields Types

newtype PostID    = PostID Int       deriving (Show, FromJSON, ToJSON)
newtype PostTitle = PostTitle T.Text deriving (Show, FromJSON, ToJSON)
newtype PostBody  = PostBody T.Text  deriving (Show, FromJSON, ToJSON)
newtype PostTag   = PostTag T.Text   deriving (Show, FromJSON, ToJSON)
newtype PostDate  = PostDate UTCTime deriving (Show, FromJSON, ToJSON)

