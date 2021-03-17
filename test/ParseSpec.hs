{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
module ParseSpec where

import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import SpecHelper

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

import Money

spec_parse :: Spec
spec_parse = describe "parses samples" $ do
  forM_ [(0 :: Int)..3] $ \i -> it ("parses sample # " ++ show i) $ do
    r <- eitherDecodeStatementFile ("test/samples/" ++ show i ++ ".pretty")
    r `shouldSatisfy` isRight
    let Right as = r
    validateSome as `shouldBe` True

spec_parse_known :: Spec
spec_parse_known = describe "parses samples vs known info" $ do
  it "gets correct input" $ do
    Right r <- eitherDecodeStatementFile "test/samples/0.pretty"
    dateStart r `shouldBe` td'
    openingBalance r `shouldBe` (fromJust $ mkSomeDense "CZK" 195)
    closingBalance r `shouldBe` (fromJust $ mkSomeDense "CZK" 195.01)

    (length $ transactions r) `shouldBe` 3
    (date $ transactions r !! 0) `shouldBe` td'

    validateSome r `shouldBe` True

    let r' = fromSomeDense @"CZK" <$> r
    (openingBalance r') `shouldBe` dense @"CZK" 195
    (closingBalance r') `shouldBe` dense @"CZK" 195.01

    ((+) <$> openingBalance r' <*> transactionsBalance r')  `shouldBe` (closingBalance r')

    validate r' `shouldBe` True

td :: Maybe Day
td = utctDay <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" "2021-01-01"

td' :: Day
td' = fromJust td

spec_date :: Spec
spec_date = do
  describe "parses dates" $ do
    it "handles simple one" $ parseFioDate "2021-01-01" == td
    it "handles simple timezoned" $ parseFioDate "2021-01-01+0200" == td
    it "handles simple timezoned #2" $ parseFioDate "2021-01-01+02:00" == td
