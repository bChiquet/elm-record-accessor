{-# LANGUAGE OverloadedStrings #-}

module Lib
--    ( readPackageManifest
--    , parseElmPackage
--    , roots
--    , example
--    ) 
   where

import System.FilePath.Find 
    (find
    , depth
    , (<?)
    , fileName
    , (==?)
    , always
    , extension
    )
import Data.Attoparsec.Text 
    (Parser
    , parse
    , maybeResult
    , char
    , anyChar
    , many'
    , notChar
    , string
    , skipWhile
    , endOfInput
    , takeWhile
    , manyTill
    )
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Prelude hiding (takeWhile)
import Control.Monad (join)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Dir = String

getElmPackage :: IO (Maybe FilePath)
getElmPackage = 
    find noRecursion (fileName ==? "elm-package.json") "."
    >>= \match -> case match of 
      [pkg] -> return $ Just pkg
      _   -> return Nothing

parseElmPackage :: FilePath -> IO [FilePath]
parseElmPackage pkg = 
      fmap Text.unpack
  <$> fromMaybe []
  <$> maybeResult
  <$> parse package
  <$> Text.readFile pkg 

crawlElmFiles :: [FilePath] -> IO [FilePath]
crawlElmFiles = fmap join . sequence . fmap crawlElmRoot

crawlElmRoot :: FilePath -> IO [FilePath]
crawlElmRoot = find always (extension ==? ".elm")


package :: Parser [Text.Text]
package =
       manyTill anyChar (string "source-directories\"")
    *> filler
    *> rootBlock

root :: Parser Text.Text
root = char '"' *> takeWhile (/= '"') <* char '"'

rootBlock :: Parser [Text.Text]
rootBlock = char '[' 
         *> many' ( filler *> root <*filler)
        <*  char ']'

filler = skipWhile (\c -> c/= '"' && c /= ']' && c /= '[')

noRecursion = depth <? 1

type Field = String
parseRecordFields : FilePath -> IO [Field]
parseRecordFields module =
      fmap Text.unpack
  <$> fromMaybe []
  <$> MaybeResult
  <$> parse fields
  <$> Text.readFile module 

fields :: Parser [Text.Text]
fields module = undefined
  
  
  
