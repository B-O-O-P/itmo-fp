module Command.Command
    ( Command(..)
    ) where

import qualified Data.Text as T

-- | Data type represents command in file manager.
data Command
  = CommandCd {
      commandFolderName :: String
    }
  | CommandDir
  | CommandLs {
      commandFolderName :: String
    }
  | CommandCat {
      commandFileName :: String
    }
  | CommandCreateFolder {
      commandFolderName :: String
    }
  | CommandCreateFile {
      commandFileName :: String
    }
  | CommandRemove {
      commandFileOrFolderName :: String
    }
  | CommandWriteFile {
      commandFileName :: String,
      commandWriteText :: T.Text
  }
  | CommandFindFile {
      commandFileName :: String
    }
  | CommandInformation {
      commandFileOrFolderName :: String
    }
  | CommandHelp
  | CommandExit
  deriving (Show, Eq)
