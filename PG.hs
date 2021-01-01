module PG where

import Prelude hiding ( Char, String )
import Parsing ( oneOrMore, parse, readsP )
import Data.Maybe ( maybeToList )
import Test.QuickCheck ( elements, Arbitrary(arbitrary) )

-- pag 50 --

data Char = P | G | Dash
 deriving Eq

type String = [Char]

instance Show Char where
   show P = "p"
   show G = "g"
   show Dash = "-"
   showList = showString . concatMap show

instance Read Char where
   readsPrec _ pg
    = case pg of
      ('p':pg) -> [(P,pg)]
      ('g':pg) -> [(G,pg)]
      ('-':pg) -> [(Dash,pg)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary Char where
   arbitrary = elements [P,G,Dash]