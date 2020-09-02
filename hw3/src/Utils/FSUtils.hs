module Utils.FSUtils
    ( isFile
    , isDir
    ) where

import Task5 (FS(..))

-- | Check if given 'FS' element is file.
isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

-- | Check if given 'FS' element is Directory.
isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _        = False
