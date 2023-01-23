module SafeSync.Utils where

import Data.Text (Text)

class IsText a where
    toText :: a -> Text
    fromText :: Text -> a