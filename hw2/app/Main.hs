module Main where

import Control.Monad.Except
import Control.Monad.State
import Control.Exception
import System.Exit
import System.IO (hPutStrLn, stderr)

import Command.Command (Command(..))
import Command.Parser (parse)
import Command.Handler (runCommand)
import FileSystem.FileSystem
import FileSystem.Sync (saveFileSystem)
import FileSystem.Exception (FileSystemException)
import ProgramSettings (opts)

main :: IO ()
main = do
  putStrLn "Initialize root directory (input absolute path):"
  root <- getLine
  fs <- catch (createFileSystem root)
    (\e -> do
      hPutStrLn stderr $ show (e :: FileSystemException)
      exitFailure
    )
  putStrLn $ "Root directory initalized to " ++ root
  let fsState = (fs, fs)
  runFS fsState

runFS :: FSState -> IO ()
runFS st = do
  input <- getLine
  mcmd <- parse input opts
  case mcmd of
    Just CommandExit -> do
      saveFileSystem (fst st)
      putStrLn $ "Changes saved. Shutting down..."
      return ()
    Just cmd -> do
      let (res, newState) = runState (runExceptT $ runCommand cmd) st
      case res of
        Left exception -> do
          hPutStrLn stderr $ show exception
          runFS newState
        Right success -> do
          putStrLn success
          runFS newState
    Nothing -> runFS st
