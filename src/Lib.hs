module Lib where

import Graphics.HsExif
import Data.Time.Format
import Data.Time.LocalTime
import Data.Char
import Data.List
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
                   , fileTime :: LocalTime
                   } deriving (Show)

imageExtensions :: [String]
imageExtensions = ["jpg", "jpeg", "png"]

videoExtensions :: [String]
videoExtensions = ["mov"]

fileIsMedia :: FilePath -> Bool
fileIsMedia path = fileIsImage path || fileIsVideo path

fileIsImage :: FilePath -> Bool
fileIsImage path = filterByExtension path imageExtensions

fileIsVideo :: FilePath -> Bool
fileIsVideo path = filterByExtension path videoExtensions

filterByExtension :: FilePath -> [String] -> Bool
filterByExtension path extensions = any (hasExtension path) extensions

hasExtension :: FilePath -> String -> Bool
hasExtension path extension = isSuffixOf extension (Prelude.map toLower path)

photoProcess :: [FilePath] -> IO ()
photoProcess paths = do
  media <- mapM classifyFile paths
  let numberOfMediaFiles = length media
  putStrLn ("I found " ++ (show numberOfMediaFiles) ++ " media files:")
  mapM_ print media

classifyFile :: FilePath -> IO Media
classifyFile filePath = do
  modificationTime <- getLocalizedModificationTime filePath
  exifTime <- getExifTime filePath
  case exifTime of
    Just time -> return Photo { path = filePath
                               , exifTime = time
                               , fileTime = modificationTime
                               }
    Nothing -> return (classifyNonPhotos filePath modificationTime)

classifyNonPhotos :: FilePath -> LocalTime -> Media
classifyNonPhotos filePath modificationTime =
  if fileIsVideo filePath
    then
      Video { path = filePath
            , fileTime = modificationTime
            }
    else
      Image { path = filePath
            , fileTime = modificationTime
            }

getExifTime :: FilePath -> IO (Maybe LocalTime)
getExifTime filePath = do
  exifData <- parseFileExif filePath
  return (exifToLocalTime exifData)

exifToLocalTime :: Either String (Map ExifTag ExifValue) -> Maybe LocalTime
exifToLocalTime (Left message) = Nothing
exifToLocalTime (Right exifMap) = getDateTimeOriginal exifMap

getLocalizedModificationTime :: FilePath -> IO LocalTime
getLocalizedModificationTime filePath = do
  modificationTime <- getModificationTime filePath
  currentTimeZone <- getTimeZone modificationTime
  return (utcToLocalTime currentTimeZone modificationTime)


--TODO: Use ZonedTime
--TODO: Use detection of summertime
--  Just localTime -> formatTime defaultTimeLocale "%Y-%m-%d - %H-%M-%S" localTime
