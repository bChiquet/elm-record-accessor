{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Lib
import Data.Text
import Data.Attoparsec.Text

main = hspec $ do
  describe "Parsing an 'elm-package.json' file" $ do
    it "parse a single root" $ do
      maybeResult (parse root "\"theRoot\"") `shouldBe` Just "theRoot"
    it "parse a block of roots" $ do
      let example = "[        \
          \        \"./src\",      \
          \        \"./docs/src\"  \
          \    ]"
      maybeResult (parse rootBlock example) `shouldBe` Just ["./src", "./docs/src"]
    it "locate the source-directories block" $ do
       let example = " \
  \     \"repository\": \"https://github.com/bChiquet/line-charts.git\", \
  \    \"license\": \"BSD3\",    \
  \    \"source-directories\": [ \
  \        \"./src\",            \
  \        \"./docs/src\"        \
  \        ],                    \
  \    \"exposed-modules\": [    \
  \        \"LineChart\",        \
  \        \"LineChart.Area\",   "
        
       maybeResult (parse package example) `shouldBe` Just ["./src", "./docs/src"]
  describe "elm module parsing" $ do
    describe "capturing field names" $ do
      it "record access" $ do
        let access = " titi.toto  . nope .tutu"
        let (Partial a) = (parse recAccess access)
        (maybeResult $ a "") `shouldBe` Just ["toto", "tutu"]
        

