module Command.Parser
    ( showHelpOnErrorExecParser
    , parseCommand
    , parse
    ) where


import Options.Applicative
import System.Environment (getProgName)

import qualified Data.Text as T

import Command.Command

-- | Parses command from given string
parse :: String -> ParserInfo Command -> IO (Maybe (Command))
parse s opts = do
  parsed <- handleResult $ showHelpOnErrorExecParser opts (words s)
  return parsed

-- | Returns help for command if it wasn't parsed successfully.
showHelpOnErrorExecParser :: ParserInfo a -> [String] -> ParserResult a
showHelpOnErrorExecParser s = execParserPure (prefs showHelpOnError) s

-- | Handles result after parsing and prints message if it was failed.
handleResult :: ParserResult a -> IO (Maybe a)
handleResult (Success a) = return $ Just a
handleResult (Failure failure) = do
      progn <- getProgName
      let (msg, _) = renderFailure failure progn
      putStrLn msg
      return Nothing
handleResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      return Nothing

-- | Creates parser for given command representation.
parseCommand :: Parser Command
parseCommand = subparser $
  (command "cd"
    (info
      (helper <*> parseCdCommand)
      (fullDesc <> progDesc "change directory to one of current directory directories"))
    )
  <>
  (command "dir"
    (info
      (helper <*> parseDirCommand)
      (fullDesc <> progDesc "show contents of current directory"))
    )
  <>
  (command "ls"
    (info
      (helper <*> parseLsCommand)
      (fullDesc <> progDesc "show contents of selected directory of current directory"))
  )
  <>
  (command "cat"
    (info
      (helper <*> parseCatCommand)
      (fullDesc <> progDesc "show file contents from current directory"))
  )
  <>
  (command "create-folder"
    (info
      (helper <*> parseCreateFolderCommand)
      (fullDesc <> progDesc "create folder in current directory"))
  )
  <>
  (command "create-file"
    (info
      (helper <*> parseCreateFileCommand)
      (fullDesc <> progDesc "create file in current directory"))
  )
  <>
  (command "write-file"
    (info
      (helper <*> parseWriteFileCommand)
      (fullDesc <> progDesc "write in file from current directory"))
  )
  <>
  (command "remove"
    (info
      (helper <*> parseRemoveCommand)
      (fullDesc <> progDesc "remove file or folder in current directory"))
  )
  <>
  (command "find-file"
    (info
      (helper <*> parseFindFileCommand)
      (fullDesc <> progDesc "find file in current or sub directories"))
  )
  <>
  (command "information"
    (info
      (helper <*> parseInformationCommand)
      (fullDesc <> progDesc "show information of file or folder from current directory"))
  )
  <>
  (command "help"
    (info
      (helper <*> parseHelpCommand)
      (fullDesc <> progDesc "show help text"))
  )
  <>
  (command "exit"
    (info
      (helper <*> parseExitCommand)
      (fullDesc <> progDesc "exit the program"))
  )

parseCdCommand :: Parser Command
parseCdCommand = fmap CommandCd folderNameParser

parseLsCommand :: Parser Command
parseLsCommand = fmap CommandLs folderNameParser

parseCatCommand :: Parser Command
parseCatCommand = fmap CommandCat folderNameParser

parseCreateFolderCommand :: Parser Command
parseCreateFolderCommand = fmap CommandCreateFolder folderNameParser

parseCreateFileCommand :: Parser Command
parseCreateFileCommand = fmap CommandCreateFile fileNameParser

parseFindFileCommand :: Parser Command
parseFindFileCommand = fmap CommandFindFile fileNameParser

parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap CommandRemove fileOrFolderNameParser

parseInformationCommand :: Parser Command
parseInformationCommand = fmap CommandInformation fileOrFolderNameParser

parseDirCommand :: Parser Command
parseDirCommand = pure (CommandDir)

parseExitCommand :: Parser Command
parseExitCommand = pure (CommandExit)

parseHelpCommand :: Parser Command
parseHelpCommand = pure (CommandHelp)

parseWriteFileCommand :: Parser Command
parseWriteFileCommand =
                   (liftA2
                        CommandWriteFile
                        fileNameParser
                        textParser
                   )

nameParser :: String -> Parser String
nameParser helpMessage = argument str (metavar "NAME" <> help helpMessage)

fileNameParser :: Parser String
fileNameParser = nameParser "name of file for usage"

folderNameParser :: Parser String
folderNameParser = nameParser "name of folder for usage"

fileOrFolderNameParser :: Parser String
fileOrFolderNameParser = nameParser "name of file or folder for usage"

textParser :: Parser T.Text
textParser = argument (str >>= readText) (metavar "TEXT" <> help "text to write in file")

readText :: String -> ReadM T.Text
readText s = do
  return $ T.pack s
