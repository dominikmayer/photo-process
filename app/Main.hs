module Main where

import System.Environment
import System.Directory
import Data.List

import Lib

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = runDefault
parseArgs [path] = processDirectory path
parseArgs args = photoProcess args

runDefault :: IO ()
runDefault = do
  putStrLn "No file specified. Running on directory."
  currentDirectory <- getCurrentDirectory
  imagePaths <- getFilesFromDirectory currentDirectory
  photoProcess imagePaths

getFilesFromDirectory :: FilePath -> IO [FilePath]
getFilesFromDirectory directory = do
  filesAndDirectories <- listDirectory directory
  return (filter fileIsMedia filesAndDirectories)

processDirectory :: String -> IO ()
processDirectory path = do
  isDirectory <- doesDirectoryExist path
  if (isDirectory)
    then do
      putStrLn "You asked me to work on a directory."
      files <- getFilesFromDirectory path
      let relativePaths = map ((path ++ "/") ++) files
      absolutePaths <- mapM makeAbsolute relativePaths
      photoProcess absolutePaths
    else do
      photoProcess [path]
