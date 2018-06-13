{-# LANGUAGE OverloadedStrings #-}

module Generation 
  (makeLenses)
  where

import Prelude hiding (unlines)
import Data.List (intercalate)
import Control.Monad (join)
import Data.Monoid ((<>))

import Data.Text (Text, unlines)
import qualified Data.Text.IO as Text

type Field = Text
type Lens = [Text]


makeLenses :: [Field] -> IO ()
makeLenses fields = do
  let header = [ "module Lens exposing (r)"    
               , ""
               , "import LensTypes exposing (Accessor(..))"
               , ""
               , "r = {"
               ]
  let lenses = intercalate ["    ,"] $ map makeLens fields
  let footer = [ "    }"]
  Text.writeFile "Lens.elm" 
    $ unlines 
    $ join [header, lenses, footer]



makeLens :: Field -> Lens
makeLens fld = [ "    " <> fld <> " = \\(Accessor sub) ->"
               , "      Accessor { get  = \\super -> sub.get super." <> fld
               , "               , over = \\f -> \\super -> { super | " <> fld <> " = sub.over f super." <> fld <> " }"
               , "               }"
               ]
