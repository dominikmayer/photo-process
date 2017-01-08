module Lib where

import Graphics.HsExif
import Data.Time.Format
import Data.Time.LocalTime
import Data.Map
import System.Directory

data Media = Photo { path :: FilePath
                   , exifTime :: LocalTime
                   , fileTime :: LocalTime
                   }
           | LivePhoto { photoPath :: FilePath
                       , videoPath :: FilePath
                       , exifTime :: LocalTime
                       , fileTime :: LocalTime
                       }
           | Image { path :: FilePath
                   , fileTime :: LocalTime
                   }
           | Video { path :: FilePath
                   --, fileTime :: LocalTime
                   } deriving (Show)

photoProcess :: [FilePath] -> IO ()
photoProcess paths = do
  putStrLn "I found the following media files:"
  media <- mapM classifyFile paths
  mapM_ print media

getExifTime :: FilePath -> IO (Maybe LocalTime)
getExifTime filePath = do
  exifData <- parseFileExif filePath
  return (exifToLocalTime exifData)

exifToLocalTime :: Either String (Map ExifTag ExifValue) -> Maybe LocalTime
exifToLocalTime (Left message) = Nothing
exifToLocalTime (Right exifMap) = getDateTimeOriginal exifMap

classifyFile :: FilePath -> IO Media
classifyFile filePath = do
  modificationTime <- getLocalizedModificationTime filePath
  exifTime <- getExifTime filePath
  case exifTime of
    Just time -> return Photo { path = filePath
                               , exifTime = time
                               , fileTime = modificationTime
                               }
    Nothing -> return Image { path = filePath
                            , fileTime = modificationTime
                            }

getLocalizedModificationTime :: FilePath -> IO LocalTime
getLocalizedModificationTime filePath = do
  modificationTime <- getModificationTime filePath
  currentTimeZone <- getTimeZone modificationTime
  return (utcToLocalTime currentTimeZone modificationTime)


--TODO: Use ZonedTime
--TODO: Use detection of summertime
--  Just localTime -> formatTime defaultTimeLocale "%Y-%m-%d - %H-%M-%S" localTime
