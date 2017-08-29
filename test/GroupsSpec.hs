module GroupsSpec where

import Data.Time ( parseTimeOrError
                 , defaultTimeLocale
                 , UTCTime
                 )
import Test.Hspec
import Test.QuickCheck

import Groups

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "folderByLevel" $ parallel $ do
        it "returns <year> when the `Year` format is given" $
            folderByLevel Year timestamp `shouldBe` "2010"
        it "returns <year>/<month-name> when the `YearMonth` format is given" $
            folderByLevel YearMonth timestamp `shouldBe` "2010/October"
        it "returns <year>/<month-name>/<day> when the `YearMonthDay` format is given" $
            folderByLevel YearMonthDay timestamp `shouldBe` "2010/October/10"
        it "returns <month-name> when the `Month` format is given" $
            folderByLevel Month timestamp `shouldBe` "October"
        it "returns <month-name>/<day> when the `MonthDay` format is given" $
            folderByLevel MonthDay timestamp `shouldBe` "October/10"
        it "returns <day> when the `Day` format is given" $
            folderByLevel Day timestamp `shouldBe` "10"
        it "returns the correct given format string (i.e. %D)" $
            folderByLevel (Custom "%D") timestamp `shouldBe` "10/10/10"
    -- describe "groupByLastAccessed" $
    --     it "groups by year" $ undefined
    --     it "groups by year/month" $ undefined
    --     it "groups by year/month/day" $ undefined
    --     it "groups by month" $ undefined
    --     it "groups by month/day" $ undefined
    --     it "groups by day" $ undefined
    -- describe "groupByLastModified" $
    --     it "groups by year" $ undefined
    --     it "groups by year/month" $ undefined
    --     it "groups by year/month/day" $ undefined
    --     it "groups by month" $ undefined
    --     it "groups by month/day" $ undefined
    --     it "groups by day" $ undefined
    -- describe "groupByName" $
    --     it "groups by the filename's first letter" $ undefined
    where
        timestamp :: UTCTime
        timestamp = parseTimeOrError True defaultTimeLocale "%s" "1286668800"
