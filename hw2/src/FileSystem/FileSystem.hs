module FileSystem.FileSystem
    ( File(..)
    , FileData(..)
    , Directory(..)
    , FSState
    , FileSystemElement
    , createFileSystem
    , getFSEName
    , fullFileName
    , createEmptyDirectory
    , replaceContentInDirectory
    , createEmptyFile
    , getDirectorySize
    , getDirectoryFilesCount
    , replaceContentInFile
    , getDirectoryParent
    , defaultPermissions
    , findFile
    , findDirectory
    , removeDirectory
    , removeFSE
    ) where


import Data.Time.Clock
import System.FilePath
import System.Directory hiding (findFile, removeDirectory)
import System.Directory.Internal.Prelude()
import Control.Exception
import Control.Applicative
import qualified Data.ByteString as B

import FileSystem.Exception
import Utils.Utils (sequenceIO, createZeroDay)

type FileContent = B.ByteString

type FileName = String

type DirectoryName = String

-- | Representation of file data. Contains it's content and time in UTC.
data FileData = FileData
                 { fileContent    :: FileContent
                 , fileUpdateTime :: UTCTime
                 }
                 deriving (Show, Eq)

-- | Representation of file in virtual file system.
-- Contains:
-- * 'FileData' - representation of file data in virtual file system
-- * Name of file in real file system
-- * Absolute path to file in real file system
-- * Size of file in real file system
-- * File extension in real file system
-- * File permissions in real file system
data File = File
              { fileData        :: FileData
              , fileName        :: FileName
              , filePath        :: FilePath
              , fileSize        :: Integer
              , fileFormat      :: String
              , filePermissions :: Permissions
              }
              deriving (Show, Eq)

-- | Representation of file in virtual file system.
-- Contains:
-- * Representations of file and directories in virtual file system
-- * Name of directory in real file system
-- * Absolute path to directory in real file system
-- * Directory permissions in real file system
data Directory = Directory
                    { directoryContent     :: [FileSystemElement]
                    , directoryName        :: DirectoryName
                    , directoryPath        :: [DirectoryName]
                    , directoryPermissions :: Permissions
                    }
                    deriving (Show, Eq)

-- | Every item in virtual file system is file or directory.
type FileSystemElement = Either Directory File

-- | Root in virtual file system is directory.
type FileSystem = Directory

-- | State of virtual file system contains root directory and current working directory for updates.
type FSState = (FileSystem, Directory)

-- | Initializes root.
createFileSystem :: String -> IO FileSystem
createFileSystem root = do
  let rootPath = splitPath root
  let rootName = last rootPath
  rootPermissions <- getPermissions root
  innerDirectories <- listDirectory root
  contents <- sequenceIO
    $ map (\p -> createFileSystemElement p)
    $ map (\name -> root ++ [pathSeparator] ++ name)  innerDirectories
  let rootDirectory = Directory contents rootName rootPath rootPermissions
  return rootDirectory

-- | Create file system element by given path.
createFileSystemElement :: FilePath -> IO FileSystemElement
createFileSystemElement path = do
  fileExist <- doesFileExist path
  if fileExist
    then do
      let name   = takeBaseName path
      let format = takeExtensions path
      permissions <- getPermissions path
      size <- getFileSize path
      content <- B.readFile path
      updateTime <- getModificationTime path

      let fileD = FileData content updateTime
      let file  = File fileD name path size format permissions
      return $ Right file
    else do
      directoryExist <- doesDirectoryExist path
      if directoryExist
        then do
          let dirPath = splitPath path
          let dirName = last dirPath
          permissions <- getPermissions path
          directories <- listDirectory path
          contents <- sequenceIO $ map (\p -> createFileSystemElement p) $ map (\name -> path ++ [pathSeparator] ++ name) $ directories

          let directory = Directory contents dirName dirPath permissions
          return $ Left directory
        else
          throw $ InvalidPathException path

fullFileName :: File -> String
fullFileName file = (fileName file) ++ (fileFormat file)

-- | Returns name of file system element. For directory it's name, for file is it's name and extension.
getFSEName :: FileSystemElement -> String
getFSEName (Left fse) = directoryName fse
getFSEName (Right fse) = fullFileName fse

-- | Default value of permissions.
defaultPermissions :: Permissions
defaultPermissions = do
  let emptyP  = emptyPermissions
  let readP   = setOwnerReadable True emptyP
  let writeP  = setOwnerWritable True readP
  let searchP = setOwnerSearchable  True writeP
  searchP

-- | Create representation of empty directory.
createEmptyDirectory :: DirectoryName -> [DirectoryName] -> Directory
createEmptyDirectory name path =  Directory [] name path defaultPermissions

-- | Create representation of empty file. Time of creation is set 0.
createEmptyFile :: FileName -> String -> FilePath -> File
createEmptyFile name format path =
  File (FileData B.empty createZeroDay) name path 0 format defaultPermissions

-- | Changes directory content.
replaceContentInDirectory :: Directory -> [FileSystemElement] -> Directory
replaceContentInDirectory dir@(Directory {directoryContent = _ }) ys =
  dir { directoryContent = ys }

-- | Changes file content. In representation time of update doesn't change.
replaceContentInFile :: File -> FileContent -> File
replaceContentInFile file@(File {fileData = FileData _ time }) newContent =
  file { fileData = FileData newContent time}

-- | Returns size of all files in current and sub directories.
getDirectorySize :: Directory -> Integer
getDirectorySize directory = do
  let content = directoryContent directory
  let result  = foldl (+) 0 $ map (\fse -> getSizeOrContinue fse) content
  result
    where
      getSizeOrContinue :: FileSystemElement -> Integer
      getSizeOrContinue (Left dir)   = getDirectorySize dir
      getSizeOrContinue (Right file) = fileSize file

-- | Returns number of all files in current and sub directories.
getDirectoryFilesCount :: Directory -> Integer
getDirectoryFilesCount directory = do
  let content = directoryContent directory
  let result  = foldl (+) 0 $ map (\fse -> getSizeOrContinue fse) content
  result
    where
      getSizeOrContinue :: FileSystemElement -> Integer
      getSizeOrContinue (Left dir) = getDirectoryFilesCount dir
      getSizeOrContinue (Right _)  = 1

-- | Returns parent for given directory. For root is Nothing.
getDirectoryParent :: Directory -> FileSystemElement -> Maybe Directory
getDirectoryParent root search = helper (directoryContent root)
  where
    helper :: [FileSystemElement] -> Maybe Directory
    helper [] = Nothing
    helper (f:fs) =
      if f == search
        then Just root
        else case f of
          Left dir -> (helper fs) <|> getDirectoryParent dir search
          Right _  -> helper fs

-- | Finds file in current directory.
findFile :: String -> [FileSystemElement] -> Maybe (File)
findFile _ [] = Nothing
findFile searchName (x:xs) =
  if searchName == getFSEName x
    then case x of
      Left  _     -> findFile searchName xs
      Right file  -> Just file
    else findFile searchName xs

-- | Finds directory in current directory.
findDirectory :: String -> [FileSystemElement] -> Maybe (Directory)
findDirectory _ [] = Nothing
findDirectory searchName (x:xs) =
  if searchName == getFSEName x
    then case x of
      Left  dir -> Just dir
      Right _   -> findDirectory searchName xs
    else findDirectory searchName xs

-- | Removes any item in directory content by given name.
removeFSE :: String -> [FileSystemElement] -> [FileSystemElement]
removeFSE _   [] = []
removeFSE fse (f:fs) | fse == getFSEName f = removeFSE fse fs
                     | otherwise = f : removeFSE fse fs

-- | Removes directory in directory content by given name.
removeDirectory :: Directory -> [FileSystemElement] -> [FileSystemElement]
removeDirectory _ [] = []
removeDirectory r ((Left dir):fs) | directoryPath dir == directoryPath r = removeDirectory r fs
                                  | otherwise = (Left dir) : removeDirectory r fs
removeDirectory r ((Right file):fs) = (Right file) : removeDirectory r fs
