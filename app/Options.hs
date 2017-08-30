module Options
    ( options
    , Options (..)
    ) where

import Control.Applicative ( (<|>)
                           , (*>)
                           , optional
                           )
import Data.Monoid ( (<>) )
import Options.Applicative ( (<**>)
                           , auto
                           , eitherReader
                           , flag
                           , flag'
                           , fullDesc
                           , header
                           , helper
                           , help
                           , info
                           , long
                           , metavar
                           , option
                           , progDesc
                           , short
                           , strOption
                           )
import Options.Applicative.Types ( Parser
                                 , ParserInfo
                                 , ReadM
                                 )

import Files ( FileType (..)
             , readFileType
             )
import Groups ( GroupBy (..)
              , Level (..)
              )

data Options = Options
    { optionFileType :: FileType
    , optionGlobPattern :: String
    , optionMaxDepth :: Int
    , optionMinDepth :: Int
    , optionGroupBy :: GroupBy
    , optionPath :: String
    }

options :: ParserInfo Options
options = info (options' <**> helper) ( fullDesc
                                      <> progDesc "Group files in folders."
                                      <> header "image-sorter - Group your files"
                                      )
    where
        options' = Options
                <$> fileType
                <*> globPattern
                <*> maxDepth
                <*> minDepth
                <*> groupBy
                <*> path

path :: Parser String
path = strOption
    ( long "output"
   <> short 'o'
   <> metavar "DESTINATION"
   <> help "The folder where to put the sorted files."
    )

fileType :: Parser FileType
fileType = option parseFileType
    ( long "type"
   <> metavar "TYPE"
   <> help "The filetype to match when searching for file. The following types are available: \
           \n\n b  Block device \
           \n c  Character device \
           \n f  Regular file \
           \n d  Directory \
           \n l  Symbolic link \
           \n p  Named pipe \
           \n s  Socket"
    )

parseFileType :: ReadM FileType
parseFileType = eitherReader readFileType

globPattern :: Parser String
globPattern = strOption 
    ( long "pattern"
   <> metavar "PATTERN"
   <> help "The glob pattern to match when searching for files."
    )

maxDepth :: Parser Int
maxDepth = option auto
    ( long "maxdepth"
   <> metavar "DEPTH"
   <> help "The maximum depth to use when recursing."
    )

minDepth :: Parser Int
minDepth = option auto
           ( long "mindepth"
          <> metavar "DEPTH"
          <> help "The minimum depth to use when recursing."
           )

groupBy :: Parser GroupBy
groupBy = groupByName
       <|> groupByLastAccessed
       <|> groupByLastModified
  where
      groupByName :: Parser GroupBy
      groupByName = flag' Name
          ( long "name"
         <> help "Group the files by their names."
          )
      groupByLastAccessed :: Parser GroupBy
      groupByLastAccessed = LastAccessed <$> (flag' ()
          ( long "last-accessed"
         <> help "Group the files by the last time they were accessed."
          ) *> level)
      groupByLastModified :: Parser GroupBy
      groupByLastModified = LastModified <$> (flag' ()
          ( long "last-modified"
         <> help "Group the files by the last time they were modified."
          ) *> level)

level :: Parser (Maybe Level)
level = levelYear
     <|> levelMonth
     <|> levelDay
     <|> levelCustom
  where
    levelYear :: Parser (Maybe Level)
    levelYear = flag Nothing (Just Year)
        ( long "year"
       <> help "Create year named folders."
        )
    levelMonth :: Parser (Maybe Level)
    levelMonth = flag Nothing (Just Month)
        ( long "month"
       <> help "Create month named folders."
        )
    levelDay :: Parser (Maybe Level)
    levelDay = flag Nothing (Just Day)
        ( long "day"
       <> help "Create day named folders."
        )
    levelCustom :: Parser (Maybe Level)
    levelCustom = pure . Custom <$> strOption
        ( long "custom"
       <> metavar "PATTERN"
       <> help "Create custom named folders. To see the available options, see the following page: \
                \n\n https://hackage.haskell.org/package/time-1.8.0.3/docs/Data-Time-Format.html"
        )