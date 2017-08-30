module Files
    ( File (..)
    ) where


import Data.Maybe ( fromMaybe )
import Data.Time ( UTCTime )
import System.Directory ( canonicalizePath
                        , createDirectoryIfMissing
                        , getAccessTime
                        , getCurrentDirectory
                        , getModificationTime
                        )
import System.EasyFile ( combine
                       , splitFileName
                       )
import System.FilePath.Find ( FileType (..)
                            , FilterPredicate
                            , FindClause (..)
                            , RecursionPredicate
                            , (==?)
                            , (<=?)
                            , (>=?)
                            , (~~?)
                            , (&&?)
                            , always
                            , depth
                            , fileName
                            , find
                            )

data File
    = File { name :: String
           , path :: String
           , accessTime :: UTCTime
           , modificationTime :: UTCTime
           } deriving (Eq, Ord, Show)

readFileType :: String -> FileType
readFileType "b" = BlockDevice
readFileType "c" = CharacterDevice
readFileType "p" = NamedPipe
readFileType "f" = RegularFile
readFileType "d" = Directory
readFileType "l" = SymbolicLink
readFileType "s" = Socket
readFileType _ = Unknown

maxDepth :: Int -> FindClause Bool
maxDepth n = depth <=? n

minDepth :: Int -> FindClause Bool
minDepth n = depth >=? n

globPattern :: String -> FindClause Bool
globPattern p = fileName ~~? p

fetchFiles
    :: (Traversable t, Foldable t)
    => t RecursionPredicate
    -> t FilterPredicate
    -> FilePath
    -> IO [File]
fetchFiles recursionClauses filterClauses path = do 
    filepaths <- find recursionClause filterClause path
    mapM makeFile filepaths
    where
        recursionClause = foldr (&&?) always recursionClauses
        filterClause = foldr (&&?) always filterClauses

makeFile :: FilePath -> IO File
makeFile filepath = do
    filepath' <- canonicalizePath filepath
    let (path', filename) = splitFileName filepath'
    accessTime' <- getAccessTime filepath
    modificationTime' <- getModificationTime filepath
    pure File { name = filename
              , path = path'
              , accessTime = accessTime'
              , modificationTime = modificationTime'
              }

makeDirectory :: Maybe FilePath -> FilePath -> IO ()
makeDirectory destination directoryPath = do
    defaultDestination <- getCurrentDirectory
    destination' <- canonicalizePath $ fromMaybe defaultDestination destination
    createDirectoryIfMissing True $ combine destination' directoryPath
