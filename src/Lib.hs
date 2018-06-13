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
    , parseOnly
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
    , letter
    , skipSpace
    , sepBy
    )
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Char (isLower)
import Prelude hiding (takeWhile)
import Control.Monad (join)
import Control.Applicative ((<|>))
import Data.Set (fromList, toList)

type Dir = String

--checks if there is a elm-package.json
getElmPackage :: IO (Maybe FilePath)
getElmPackage = 
    find noRecursion (fileName ==? "elm-package.json") "."
    >>= \match -> case match of 
      [pkg] -> return $ Just pkg
      _   -> return Nothing

-- read the elm-package.json for a list of elm source roots.
parseElmPackage :: FilePath -> IO [FilePath]
parseElmPackage pkg = 
      fmap Text.unpack
  <$> fromMaybe []
  <$> maybeResult
  <$> parse package
  <$> Text.readFile pkg 

-- Finds recursively all elm files in a list of roots
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

type Field = Text.Text

-- Parses record fields accessed in a list of source files
parseRecordFields :: [FilePath] -> IO [Field]
parseRecordFields = fmap unique . fmap join . sequence . fmap parseModuleRecordFields

parseModuleRecordFields :: FilePath -> IO [Field]
parseModuleRecordFields elmModule =
      fromRight []
  <$> parseOnly recAccess
  <$> Text.readFile elmModule

-- matcches all fieldAccess, remove capitalized and empty ones
recAccess :: Parser [Field]
recAccess = 
        filter startsWithLower
    <$> filter (/= "") 
    <$> many' ((skipWhile (/= '.')) *> fieldAccess ) 

-- match `.tutu` `.Titi` `.` etc
fieldAccess :: Parser Field
fieldAccess = 
      Text.pack <$> (char '.' *> letters)

letters = many' letter

startsWithLower t = isLower $ Text.head t

unique = toList . fromList
