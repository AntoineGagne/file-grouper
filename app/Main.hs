module Main where

import Options.Applicative ( execParser )

import Options ( Options (..)
               , options
               )


main :: IO ()
main = do
    execParser options
    pure ()
