{-# LANGUAGE RecordWildCards #-}

module Command.Handler
    ( showHelpOnErrorExecParser
    , runCommand
    , handleDirCommand
    , handleCatCommand
    , handleCreateFolderCommand
    , handleCreateFileCommand
    , handleRemoveCommand
    , handleWriteFileCommand
    , handleFindFileCommand
    , handleInformationFileCommand
    , handleInformationFolderCommand
    ) where

import Control.Monad.Except
import Control.Monad.State
import System.FilePath

import Command.Command
import Command.Parser
import Utils.Utils (findJust, modifyLastPath)
import FileSystem.Exception
import FileSystem.FileSystem

import qualified Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.Text.Encoding.Error as T.Encoding.Error

-- | Runs current Command, updates current virtual file system state.
-- Commands work only on current directory.
-- * 'FileSystemElementNotFoundException'
--    Command got wrong path or element doesn't exist.
-- * 'FolderAlreadyExistsException'
--    Command got path to create folder which already exists.
-- * 'FileAlreadyExistsException'
--    Command got path to create file which already exists.
-- * 'FileAlreadyExistsException'
--    Command got path to create file which already exists.
-- * 'OtherException'
--    Can be thrown by handlers.
runCommand :: Command -> ExceptT FileSystemException (State FSState) String
runCommand cmd = do
    st <- lift get
    let currentDirectory = snd st
    case cmd of
      CommandCd{..} -> do
        let changeDirectory = getIfUp commandFolderName currentDirectory
        case changeDirectory of
          Just dir -> do
            put (fst st, dir)
            return $ "> Directory was changed to " ++ (directoryName dir)
          Nothing  -> throwError (FileSystemElementNotFoundException commandFolderName)
        where
          getIfUp :: String -> Directory ->  Maybe (Directory)
          getIfUp name current = if name == ".."
            then getDirectoryParent (fst st) (Left current)
            else findDirectory name $ directoryContent current

      CommandDir -> return $ ">\n" ++ (handleDirCommand currentDirectory)

      CommandLs{..} -> do
        let directories = directoryContent currentDirectory
        let directory = findDirectory commandFolderName directories
        case directory of
          Just dir -> return $ handleDirCommand dir
          Nothing  -> throwError (FileSystemElementNotFoundException commandFolderName)

      CommandCat{..} -> do
        case handleCatCommand commandFileName currentDirectory of
          Just res -> do
            case res of
              Left exception -> throwError $ OtherException $ show exception
              Right text -> return $ "> " ++ T.unpack text
          Nothing  -> throwError (FileSystemElementNotFoundException commandFileName)

      CommandCreateFolder{..} -> do
        let updatedCurrentDirectory = handleCreateFolderCommand commandFolderName currentDirectory
        case updatedCurrentDirectory of
          Just dir -> do
            put (syncRoot (fst st) currentDirectory dir, dir)
            return $ "> Directory '" ++ commandFolderName ++ "' was created"
          Nothing  -> throwError (FolderAlreadyExistsException commandFolderName)

      CommandCreateFile{..} -> do
        let updatedCurrentDirectory = handleCreateFileCommand commandFileName currentDirectory
        case updatedCurrentDirectory of
          Just dir -> do
            put (syncRoot (fst st) currentDirectory dir, dir)
            return $ "> File '" ++ commandFileName ++ "' was created"
          Nothing -> throwError (FileAlreadyExistsException commandFileName)

      CommandRemove{..} -> do
        let updatedCurrentDirectory = handleRemoveCommand commandFileOrFolderName currentDirectory
        case updatedCurrentDirectory of
          Just dir -> do
            put (syncRoot (fst st) currentDirectory dir, dir)
            return $ "> '" ++ commandFileOrFolderName ++ "' was deleted"
          Nothing  -> throwError (FileSystemElementNotFoundException commandFileOrFolderName)


      CommandWriteFile{..} -> do
        let byteText = T.Encoding.encodeUtf8 commandWriteText
        let updatedCurrentDirectory = handleWriteFileCommand commandFileName byteText currentDirectory
        case updatedCurrentDirectory of
          Just dir -> do
            put (syncRoot (fst st) currentDirectory dir, dir)
            return $ "> '" ++ commandFileName ++ "' was rewrited"
          Nothing  -> throwError (FileSystemElementNotFoundException commandFileName)

      CommandFindFile{..} -> do
        let handleFile = handleFindFileCommand commandFileName currentDirectory
        case handleFile of
          Just file ->
            return $ ">  File '" ++ commandFileName ++ "' is available on the path " ++ filePath file
          Nothing   -> return $ "> File '" ++ commandFileName ++ "' doesn't exist"

      CommandInformation{..} -> do
        let content   = directoryContent currentDirectory
        let foundFile = findFile commandFileOrFolderName content
        let directory = findDirectory commandFileOrFolderName content
        case foundFile of
          Just file -> case directory of
            Just _   -> throwError (FolderAlreadyExistsException commandFileOrFolderName)
            Nothing  -> return $ "> " ++ (handleInformationFileCommand file)
          Nothing   -> case directory of
            Just dir -> return $ "> " ++ (handleInformationFolderCommand dir)
            Nothing  -> throwError (FileAlreadyExistsException commandFileOrFolderName)

      CommandHelp -> do
        return $ "> Available options:\n" ++
          "   -h,--help                Write to get more information about command\n\n" ++
          "Available commands:\n" ++
          "  cd                       change directory\n" ++
          "  dir                      show contents of current directory\n" ++
          "  ls                       show contents of selected directory\n" ++
          "  cat                      show file contents\n" ++
          "  create-folder            create folder\n" ++
          "  create-file              create file\n" ++
          "  write-file               remove file or folder\n" ++
          "  remove                   remove file or folder\n" ++
          "  find-file                find file\n" ++
          "  information              show information of file or folder\n" ++
          "  help                     show help text\n" ++
          "  exit                     exit the program\n"

      CommandExit -> return $ "Shut down."

-- | Handler for 'dir' command. Returns content of current virtual file system directory.
handleDirCommand :: Directory -> String
handleDirCommand currentDirectory = do
  let directories = directoryContent currentDirectory
  let result = foldl (++) "" $ map (\fse -> (getFSEName fse) ++ "\n") directories
  result

-- | Handler for 'cat' command. Returns content of file in current virtual file system directory.
handleCatCommand :: String -> Directory -> Maybe (Either T.Encoding.Error.UnicodeException T.Text)
handleCatCommand catName currentDirectory = do
  let directories = directoryContent currentDirectory
  case findFile catName directories of
    Just file -> do
      let content = T.Encoding.decodeUtf8' $ fileContent $ fileData file
      Just content
    Nothing -> Nothing

-- | Handler for 'create-folder' command. Creates folder by given name in current virtual file
-- system directory.
handleCreateFolderCommand :: String -> Directory -> Maybe (Directory)
handleCreateFolderCommand createName currentDirectory = do
  let directories = directoryContent currentDirectory
  case findDirectory createName directories of
    Just _  -> Nothing
    Nothing -> do
      let newPath = modifyLastPath (directoryPath currentDirectory) ++ [createName]
      let newDirectory = createEmptyDirectory createName newPath
      Just $ replaceContentInDirectory currentDirectory (directories ++ [Left newDirectory])

-- | Handler for 'create-file' command. Creates file by given name in current virtual file
-- system directory.
handleCreateFileCommand :: String -> Directory -> Maybe (Directory)
handleCreateFileCommand createName currentDirectory = do
  let directories = directoryContent currentDirectory
  case findFile createName directories of
    Just _  -> Nothing
    Nothing -> do
      let name = takeBaseName createName
      let format = takeExtension createName
      let newPath = joinPath $ modifyLastPath (directoryPath currentDirectory) ++ [createName]
      let newFile = createEmptyFile name format newPath
      Just $ replaceContentInDirectory currentDirectory (directories ++ [Right newFile])

-- | Handler for 'remove' command. Removes file or folder by given name in current virtual file
-- system directory.
handleRemoveCommand :: String -> Directory -> Maybe (Directory)
handleRemoveCommand removeName currentDirectory = do
  let directories = directoryContent currentDirectory
  case findFile removeName directories of
    Just _  -> do
       Just $ replaceContentInDirectory currentDirectory (removeFSE removeName directories)
    Nothing -> case findDirectory removeName directories of
        Just _  -> do
            Just $ replaceContentInDirectory currentDirectory (removeFSE removeName directories)
        Nothing -> Nothing

-- | Handler for 'write-file' command. Rewrites in fine by given name of file in current virtual file
-- system directory.
handleWriteFileCommand :: String -> Data.ByteString.ByteString -> Directory -> Maybe (Directory)
handleWriteFileCommand writeName newContent currentDirectory = do
  let directories = directoryContent currentDirectory
  case findFile writeName directories of
    Just file -> do
      let newFile = replaceContentInFile file newContent
      case handleRemoveCommand writeName currentDirectory of
        Just dir -> Just $ replaceContentInDirectory dir (directoryContent dir ++ [Right newFile])
        Nothing  -> Nothing
    Nothing -> Nothing

-- | Handler for 'find-file' command. Finds any path to file in current and sub virtual file
-- system directories by given name.
handleFindFileCommand :: String -> Directory -> Maybe (File)
handleFindFileCommand findName currentDirectory = do
  let content = map (\fse -> checkOrContinue findName fse) (directoryContent currentDirectory)
  findJust content
    where
      checkOrContinue :: String -> FileSystemElement -> Maybe (File)
      checkOrContinue name (Left dir) = handleFindFileCommand name dir
      checkOrContinue name (Right file) | fullFileName file == name = Just file
                                        | otherwise = Nothing

-- | Handler for 'information' command. Returns detailed info of file in current virtual file
-- system directory by given name.
handleInformationFileCommand :: File -> String
handleInformationFileCommand file = "File '" ++ (fileName file) ++ "' information:\n"
  ++ "  Path: " ++ (filePath file) ++ "\n"
  ++ "  Extension: " ++ (fileFormat file) ++ "\n"
  ++ "  Last update time (UTC): " ++ (show $ fileUpdateTime $ fileData file) ++ "\n"
  ++ "  Size: " ++ (show $ fileSize file) ++ "\n"
  ++ "  Permissions: " ++ (show $ filePermissions file) ++ "\n"

-- | Handler for 'information' command. Returns detailed info of folder in current virtual file
-- system directory by given name.
handleInformationFolderCommand :: Directory -> String
handleInformationFolderCommand directory = "Directory '" ++ (directoryName directory) ++ "' information:\n"
  ++ "  Path: " ++ (joinPath $ directoryPath directory) ++ "\n"
  ++ "  Files count: " ++ (show $ getDirectoryFilesCount directory) ++ "\n"
  ++ "  Size: " ++ ( show $ getDirectorySize directory) ++ "\n"
  ++ "  Permissions: " ++ (show $ directoryPermissions directory) ++ "\n"

-- | Updates current file system state.
syncRoot :: Directory -> Directory -> Directory -> Directory
syncRoot root old new = do
  let parent = getDirectoryParent root (Left old)
  case parent of
    Just changedParent -> do
      let parentContent = directoryContent changedParent
      replaceContentInDirectory changedParent ((removeDirectory old parentContent) ++ [Left new])
    Nothing -> new
