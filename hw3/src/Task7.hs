{-# LANGUAGE Rank2Types #-}

module Task7
    ( replaceFilesExt
    , lsRecursive
    , removeIfEmpty
    ) where

import Lens.Micro ((&), (%~), (.~), (^..), (^.), traversed, filtered)
import System.FilePath (replaceExtension)

import Task5 (FS(..), name, contents)
import Utils.FSUtils (isFile)

-- | Replace extension of all files in current directory into give one. Non-recursive.
replaceFilesExt :: FS -> String -> FS
replaceFilesExt fs newExt = fs &
  contents.traversed.(filtered isFile).name %~ (flip replaceExtension newExt)

-- | Get the name of all files, directories and its subdirectories. Recursive.
lsRecursive :: FS -> [FilePath]
lsRecursive fs = fs ^.. rec
  where
    rec = contents.traversed.name <> contents.traversed.rec

-- | Remove chosen subdirectory if it isn't empty.
removeIfEmpty :: FS-> String -> FS
removeIfEmpty fs dir = fs &
  contents.~(fs^..contents.traversed.filtered(\fss -> isFile fss
                                                   || (fss^.name /= dir)
                                                   || not (null $ fss^.contents)))
