module Lib
    ( someFunc
    ) where

-- Implement a haskell test.
import Database.PostgreSQL.Simple

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo
  { connectUser     = "sequoia_personal"
  , connectPassword = "notthedroidsyourlookingfor"
  , connectDatabase = "sequoia_public"
  }

sampleQuery :: Query
sampleQuery = "select 2 + 2"

someFunc :: IO ()
someFunc = do
  conn <- connect connectionInfo
  res <- query_ conn "select 2 + 2" :: IO [Int]
  print res
  
  
