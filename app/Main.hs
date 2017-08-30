module Main where

import Data.Map.Strict ( empty )
import Data.Maybe ( maybe )
import Options.Applicative ( execParser )
import System.FilePath.Find ( always )

import Files ( fetchFiles
             , fileType'
             , globPattern
             , maxDepth
             , minDepth
             , moveFiles
             )
import Groups ( groupBy )
import Options ( Options (..)
               , options
               )


main :: IO ()
main = do
    options' <- execParser options
    let fileType = maybe always fileType' $ optionFileType options'
        globPattern' = maybe always globPattern $ optionGlobPattern options'
        maxDepth' = maybe always maxDepth $ optionMaxDepth options'
        minDepth' = maybe always minDepth $ optionMinDepth options'
        f = groupBy $ optionGroupBy options'
    files <- fetchFiles [minDepth', maxDepth'] [globPattern', fileType] $ optionInputPath options'
    moveFiles (optionOutputPath options') $ foldr f empty files
