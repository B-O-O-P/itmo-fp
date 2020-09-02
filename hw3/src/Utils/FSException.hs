module Utils.FSException
    ( FSException(..)
    ) where

import Control.Exception (Exception)

-- | Data type, which represents exception in 'FS'
data FSException =
    PathDoesNotExists
  deriving (Show)

instance Exception FSException
