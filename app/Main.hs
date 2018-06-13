--{-# LANGUAGE RankNTypes #-}
module Main where

import Lib
import Generation (makeLenses)

main :: IO ()
main = do 
    elmPkg <- getElmPackage 
    case elmPkg of 
      Nothing -> print "Unable to find elm-package.json" 
      Just pkg -> 
        parseElmPackage pkg 
        >>= crawlElmFiles 
        >>= parseRecordFields
        >>= makeLenses
--        >>= print


data Toto a b = Toto (a -> b) ((b -> b) -> (a -> a))

--id_ = Toto (\a -> a) (\_ -> id)

--get :: ( Toto a i -> Toto b i) -> b -> i
get lens s =
  let (Toto f1 f2) = lens truc in
    f1 s


--truc :: Toto a a
truc = Toto (\a -> a) (\q -> q)

getter = get fooLens 3

fooLens :: Toto a z -> Toto b z
fooLens = undefined
