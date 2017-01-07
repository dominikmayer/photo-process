module Lib where

import Graphics.HsExif
import Data.Time.Format
import Data.Time.LocalTime
import Data.Map

photoProcess :: [FilePath] -> IO ()
photoProcess arguments = do
  dateStrings <- getDateStringFromFileList arguments
  putStrLn "I found images from the following times:"
  mapM_ putStrLn dateStrings

getDateStringFromFileList :: [FilePath] -> IO [String]
getDateStringFromFileList = mapM getDateStringFromFile

getDateStringFromFile :: FilePath -> IO String
getDateStringFromFile filePath = do
  exifData <- parseFileExif filePath
  return (exifToString exifData)

exifToString :: Either String (Map ExifTag ExifValue) -> String
exifToString (Left message) = message
exifToString (Right exifMap) = timeToString exifMap

timeToString :: Map ExifTag ExifValue -> String
timeToString exifMap = case getDateTimeOriginal exifMap of
  Just localTime -> formatTime defaultTimeLocale "%Y-%m-%d - %H-%M-%S" localTime
  Nothing -> "No time available."
