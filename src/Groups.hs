module Groups
    ( GroupBy (..)
    , Level (..)
    , File (..)
    , groupBy
    , folderByLevel
    ) where

import Data.Char ( toUpper
                 , isDigit
                 )
import Data.Map.Strict ( Map
                       , insertWith
                       )
import Data.Maybe ( fromMaybe )
import Data.Time ( FormatTime
                 , UTCTime
                 , formatTime
                 , defaultTimeLocale
                 )

import Files ( File (..) )

data GroupBy
    = LastModified (Maybe Level)
    | LastAccessed (Maybe Level)
    | Name
    deriving (Eq, Ord, Show)

data Level
    = Year
    | YearMonth
    | YearMonthDay
    | Month
    | MonthDay
    | Day
    | Custom String
    deriving (Eq, Ord, Show)

groupBy :: GroupBy -> (File -> Map String [File] -> Map String [File])
groupBy (LastAccessed level) = groupByLastModified $ fromMaybe Month level
groupBy (LastModified level) = groupByLastModified $ fromMaybe Month level
groupBy Name = groupByName

groupByLastAccessed :: Level -> File -> Map String [File] -> Map String [File]
groupByLastAccessed = groupByTime accessTime

groupByLastModified :: Level -> File -> Map String [File] -> Map String [File]
groupByLastModified = groupByTime modificationTime

groupByTime
    :: (File -> UTCTime)
    -> Level
    -> File
    -> Map String [File]
    -> Map String [File]
groupByTime f level file = insertFileWithKey (folderByLevel level (f file)) file

groupByName :: File -> Map String [File] -> Map String [File]
groupByName file = insertFileWithKey (formatFolderName file) file
    where 
        formatFolderName file
            | isDigit c = "0_9"
            | otherwise = [c]
        c = toUpper . head . name $ file

insertFileWithKey :: String -> File -> Map String [File] -> Map String [File]
insertFileWithKey key file = insertWith (++) key (pure file)

folderByLevel :: Level -> UTCTime -> String
folderByLevel Year time = formatTime defaultTimeLocale "%Y" time
folderByLevel YearMonth time = formatTime defaultTimeLocale "%Y/%B" time
folderByLevel YearMonthDay time = formatTime defaultTimeLocale "%Y/%B/%d" time
folderByLevel Month time = formatTime defaultTimeLocale "%B" time
folderByLevel MonthDay time = formatTime defaultTimeLocale "%B/%d" time
folderByLevel Day time = formatTime defaultTimeLocale "%d" time
folderByLevel (Custom format) time = formatTime defaultTimeLocale format time
