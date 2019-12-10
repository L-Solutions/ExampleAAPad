module Data.Pass
    ( PASS
    , build
    , GUID
    , Category
    , Activity
    , GUIDable (..)
    , activities
    , category
    ) where

import           Control.Monad.Identity
import           Control.Monad.Writer

import qualified Data.Pad               (GUID)
import           Data.Text              (Text)
import           Data.Text.Utils

type GUID = Data.Pad.GUID

-- | GUIDable class
--   pour manipuler facilement les `GUID`
class GUIDable a where
    guid :: a -> GUID

-- | PASS datatype
data PASS = PASS { passID        :: GUID
                 , passStructure :: [ Activity ]
                 }
        deriving (Eq, Read, Show)

build :: GUID -> [ Activity ] -> PASS
build g as = PASS g as

instance GUIDable PASS where
    guid = passID

activities :: PASS -> [ Activity ]
activities = passStructure

-- | Activity datatype
data Activity = Activity { activityCategory :: Category, activityID :: GUID }
        deriving (Eq, Read, Show)

instance GUIDable Activity where
    guid = activityID

data Category = Count | Selection | Extraction
        deriving (Eq, Read, Show)

category :: Activity -> Category
category = activityCategory

