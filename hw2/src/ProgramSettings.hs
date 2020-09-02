module ProgramSettings
    ( opts
    ) where

import Options.Applicative

import Command.Command (Command)
import Command.Parser (parseCommand)

-- | Options for message of parser.
opts :: ParserInfo Command
opts =  (info (helper <*> parseCommand)
                                  (fullDesc  <>
                                   progDesc programDescription <>
                                   header programHeader))

programDescription :: String
programDescription = "I hope i'll get more than zero points for that"

programHeader :: String
programHeader = "Simple file manager"
