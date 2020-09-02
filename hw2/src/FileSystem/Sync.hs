module FileSystem.Sync
    ( saveFileSystem
    ) where

import qualified Data.ByteString as B

import System.FilePath
import System.Directory hiding (findFile)
import System.Directory.Internal.Prelude()
import FileSystem.FileSystem hiding (removeDirectory)

-- | Saves current virtual file system state in real file system.
saveFileSystem :: Directory -> IO ()
saveFileSystem root = do
  let rootPath = joinPath $ directoryPath root
  let rootContent = directoryContent root

  realDirectories <- listDirectory rootPath
  removeFromRealDirectory rootPath realDirectories rootContent
  createAndUpdateInRealDirectory rootContent

  _ <- sequence $ map saveFileSystemElement rootContent
  return ()

saveFileSystemElement :: FileSystemElement -> IO ()
saveFileSystemElement (Left dir) = saveFileSystem dir
saveFileSystemElement (Right _)  = return ()

-- | Removes all items which are no longer in virtual file system, but still in real file system.
removeFromRealDirectory :: String -> [String] -> [FileSystemElement] -> IO ()
removeFromRealDirectory _    []     _  = return ()
removeFromRealDirectory path (r:rs) fs = do
  case findDirectory r fs of
    Just _  -> removeFromRealDirectory path rs fs
    Nothing -> case findFile r fs of
      Just _  -> removeFromRealDirectory path rs fs
      Nothing -> do
        let fullPath = path ++ [pathSeparator] ++ r
        directoryExist <- doesDirectoryExist fullPath
        if directoryExist
          then do
            removeDirectory fullPath
          else do
            fileExist <- doesFileExist fullPath
            if fileExist
              then do
                removeFile fullPath
              else return ()

-- | Create or updates items which were changed in virtual file system.
createAndUpdateInRealDirectory :: [FileSystemElement] -> IO ()
createAndUpdateInRealDirectory [] = return ()
createAndUpdateInRealDirectory (f:fs) = do
  case f of
    Left dir -> do
      let dirPath = joinPath $ directoryPath dir
      directoryExist <- doesDirectoryExist dirPath
      if directoryExist
        then return()
        else do
          createDirectory dirPath
    Right file -> do
      let fPath = filePath file
      fileExist <- doesFileExist fPath
      if fileExist
        then updateFile fPath file
        else createFile fPath file
  createAndUpdateInRealDirectory fs

-- | Updates file content in real system.
updateFile :: String -> File -> IO ()
updateFile path file = do
  realContent <- B.readFile path
  let fsContent = fileContent $ fileData file
  if realContent == fsContent
    then return ()
    else B.writeFile path fsContent

-- | Creates file content in real system.
createFile :: String -> File -> IO ()
createFile path file = do
  let fsContent = fileContent $ fileData file
  B.writeFile path fsContent
