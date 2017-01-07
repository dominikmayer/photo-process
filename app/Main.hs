module Main where

import System.Environment
import System.Directory
import Data.List

import Lib

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = do
  putStrLn "No file specified. Running on directory."
  currentDirectory <- getCurrentDirectory
  filesAndDirectories <- listDirectory currentDirectory
  let imagePaths = filter filterImages filesAndDirectories
  photoProcess imagePaths
parseArgs args = photoProcess args

filterImages :: FilePath -> Bool
filterImages file = isSuffixOf "jpg" file
                 || isSuffixOf "jpeg" file
                 || isSuffixOf "JPG" file
                 || isSuffixOf "JPEG" file
