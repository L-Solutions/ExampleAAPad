module Data.Pad where

import           Data.Text (Text)

newtype PAD = PAD (GUID, Text)
        deriving (Eq, Read, Show)

type GUID = Text
type Log = Text

