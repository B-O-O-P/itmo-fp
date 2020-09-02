module Task5Practice where

import Lens.Micro ((%~), (&), (.~), (^..), (^?), traversed)
import Data.Maybe (fromMaybe)

import Task5 (FS, name, contents, _File, _Dir)

-- Module is for practising purposes only.

subtreesList :: FS -> [FS]
subtreesList dir = dir^.._Dir.contents.traversed._Dir

maybeDirectory :: FS -> Maybe FilePath
maybeDirectory dir = dir^?_Dir.name

getFileName :: FS -> FilePath
getFileName file = fromMaybe "" (file^?_File.name)

setDefaultRoot :: FS -> FS
setDefaultRoot dir = dir & _Dir.name.~("/")

addSuffix :: FS -> String -> FS
addSuffix dir suf = dir & _Dir.name%~(++ suf)

getFirstDirectory :: FS -> Maybe FilePath
getFirstDirectory dir = dir ^? _Dir.contents.traversed._Dir.name

listFile :: FS -> [FilePath]
listFile directory = directory ^.._Dir.contents.traversed._File.name
