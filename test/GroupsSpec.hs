module GroupsSpec where

import Data.Map.Strict ( empty
                       , fromList
                       )
import Data.Time ( parseTimeOrError
                 , defaultTimeLocale
                 , UTCTime
                 )
import Test.Hspec

import Files
import Groups

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "folderByLevel" $ parallel $ do
        it "returns <year> when the `Year` format is given" $
            folderByLevel Year timestamp `shouldBe` "2010"
        it "returns <month-name> when the `Month` format is given" $
            folderByLevel Month timestamp `shouldBe` "October"
        it "returns <day> when the `Day` format is given" $
            folderByLevel Day timestamp `shouldBe` "10"
        it "returns the correct given format string (i.e. %D)" $
            folderByLevel (Custom "%D") timestamp `shouldBe` "10/10/10"

    describe "groupByLastAccessed" $ parallel $ do
        it "groups by year" $
            group (groupBy (LastAccessed (Just Year))) files `shouldBe` fromList [("2010", files)]
        it "groups by month" $
            group (groupBy (LastAccessed (Just Month))) files `shouldBe` fromList [("October", files)]
        it "groups by day" $
            group (groupBy (LastAccessed (Just Day))) files `shouldBe` fromList [("10", files)]
        it "groups by custom format (i.e. %D)" $
            group (groupBy (LastAccessed (Just (Custom "%D")))) files `shouldBe` fromList [("10/10/10", files)]
        it "groups by month if no format was given" $
            group (groupBy (LastAccessed Nothing)) files `shouldBe` fromList [("October", files)]

    describe "groupByLastModified" $ parallel $ do
        it "groups by year" $
            group (groupBy (LastModified (Just Year))) files `shouldBe` fromList [("2010", files)]
        it "groups by month" $
            group (groupBy (LastModified (Just Month))) files `shouldBe` fromList [("October", files)]
        it "groups by day" $
            group (groupBy (LastModified (Just Day))) files `shouldBe` fromList [("10", files)]
        it "groups by custom format (i.e. %D)" $
            group (groupBy (LastModified (Just (Custom "%D")))) files `shouldBe` fromList [("10/10/10", files)]
        it "groups by month if no format was given" $
            group (groupBy (LastModified Nothing)) files `shouldBe` fromList [("October", files)]

    describe "groupByName" $ parallel $ do
        it "groups by the filename's first letter" $
            group (groupBy Name) files `shouldBe` fromList [("T", files)]
        it "groups by a folder named '0_9' if the first character is a digit" $
            group (groupBy Name) digitFile `shouldBe` fromList [("0_9", digitFile)]
    where
        timestamp :: UTCTime
        timestamp = parseTimeOrError True defaultTimeLocale "%s" "1286668800"
        files :: [File]
        files = [ File { name = "test_1"
                       , path = "test/test-path/test_1"
                       , accessTime = timestamp
                       , modificationTime = timestamp
                       }
                , File { name = "test_2"
                       , path = "test/test-path/test_1"
                       , accessTime = timestamp
                       , modificationTime = timestamp
                       }
                ]
        digitFile :: [File]
        digitFile = [ File { name = "0_test"
                           , path = "test/test-path/0_test"
                           , accessTime = timestamp
                           , modificationTime = timestamp
                           }
                    ]
        group f = foldr f empty
