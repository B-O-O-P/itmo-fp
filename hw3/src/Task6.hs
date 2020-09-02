{-# LANGUAGE Rank2Types #-}

module Task6
  ( cd
  , ls
  , file
  ) where

import Lens.Micro (Traversal', (^.), traversed, filtered)

import Task5 (FS, contents, name)
import Utils.FSUtils (isFile, isDir)

-- | Change directory to one of its subdirectores by given name.
cd :: FilePath -> Traversal' FS FS
cd path = contents.traversed.filtered(\fs -> isDir fs && fs^.name == path)

-- | Return list of all elements in current 'FS' directory.
ls :: Traversal' FS FilePath
ls = contents.traversed.name

-- | Get file's name if it exists in 'FS' directory.
file :: FilePath -> Traversal' FS String
file s = contents.traversed.filtered(\fs -> isFile fs && fs^.name == s).name
