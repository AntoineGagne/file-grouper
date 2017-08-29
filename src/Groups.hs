module Groups
    ( GroupBy (..)
    , Level (..)
    , File (..)
    , groupBy
    , folderByLevel
    ) where

import Data.Map.Strict ( Map )
import Data.Maybe ( fromMaybe )
import Data.Time ( FormatTime
                 , UTCTime
                 , formatTime
                 , defaultTimeLocale
                 )

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

data File
    = File { name :: String
           , path :: String
           , accessTime :: UTCTime
           , modificationTime :: UTCTime
           } deriving (Eq, Ord, Show)

groupBy :: GroupBy -> (File -> Map String FilePath -> Map String FilePath)
groupBy (LastAccessed level) = groupByLastModified $ fromMaybe Month level
groupBy (LastModified level) = groupByLastModified $ fromMaybe Month level
groupBy Name = groupByName

groupByLastAccessed :: Level -> File -> Map String FilePath -> Map String FilePath
groupByLastAccessed = undefined

groupByLastModified :: Level -> File -> Map String FilePath -> Map String FilePath
groupByLastModified = undefined

groupByName :: File -> Map String FilePath -> Map String FilePath
groupByName = undefined

folderByLevel :: Level -> UTCTime -> String
folderByLevel Year time = formatTime defaultTimeLocale "%Y" time
folderByLevel YearMonth time = formatTime defaultTimeLocale "%Y/%B" time
folderByLevel YearMonthDay time = formatTime defaultTimeLocale "%Y/%B/%d" time
folderByLevel Month time = formatTime defaultTimeLocale "%B" time
folderByLevel MonthDay time = formatTime defaultTimeLocale "%B/%d" time
folderByLevel Day time = formatTime defaultTimeLocale "%d" time
folderByLevel (Custom format) time = formatTime defaultTimeLocale format time
