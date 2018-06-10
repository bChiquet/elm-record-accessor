module Main where

import Lib

main :: IO ()
main = do 
    elmPkg <- getElmPackage 
    case elmPkg of 
      Nothing -> print "Unable to find elm-package.json" 
      Just pkg -> 
        parseElmPackage pkg 
        >>= crawlElmFiles 
        >>= parseRecordFields
        >>= print
