module Lib where

import Graphics.HsExif
import Data.Time.Format
import Data.Time.LocalTime
import Data.Map

photoProcess :: [FilePath] -> IO ()
photoProcess arguments = do
  dateStrings <- getDateStringFromFileList arguments
  putStrLn ("photoProcess: " ++ (unwords dateStrings))

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
  Just localTime -> formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" localTime
  Nothing -> "Nothing"
