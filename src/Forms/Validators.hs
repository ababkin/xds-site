module Forms.Validators where

import Network.HTTP.Client (parseUrl)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust)


isFormattedLikeURL :: Text -> Bool
isFormattedLikeURL = isJust . parseUrl . T.unpack

isNotEmpty :: Text -> Bool
isNotEmpty = not . T.null

