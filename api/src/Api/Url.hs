module Api.Url where

import qualified Data.Text as T

-- Exports a simple url from the title, takes text and parses.
titleToUrl :: T.Text -> T.Text
titleToUrl title = 
