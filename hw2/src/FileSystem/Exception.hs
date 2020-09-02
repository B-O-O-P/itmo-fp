{-# LANGUAGE InstanceSigs #-}

module FileSystem.Exception
    ( FileSystemException(..)
    ) where

import Control.Exception

import Data.Typeable (Typeable)

-- | Data type represents all types of file system errors.
data FileSystemException
  = InvalidPathException String
  | FileSystemElementNotFoundException String
  | FileAlreadyExistsException String
  | FolderAlreadyExistsException String
  | FileSystemElementAlreadyExistsException String
  | OtherException String
  deriving (Eq, Typeable)

instance Exception FileSystemException

instance Show FileSystemException where
  show :: FileSystemException -> String
  show (InvalidPathException path) = "InvalidPathException: given '" ++ path ++ "' is invalid."
  show (FileSystemElementNotFoundException fseName) = "FileSystemElementNotFoundException: " ++
    "File or folder with given name '" ++ fseName ++ "' wasn't found."
  show (FileAlreadyExistsException fileName) = "FileAlreadyExistsException: file with name '" ++
    fileName ++ "' already exists."
  show (FolderAlreadyExistsException folderName) = "FolderAlreadyExistsException: folder with name '" ++
    folderName ++ "' already exists."
  show (FileSystemElementAlreadyExistsException fseName) = "FileSystemElementAlreadyExistsException: " ++
    "file or folder with name '" ++ fseName ++ "' already exists."
  show (OtherException message) = message
