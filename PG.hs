module PG where

import Test.QuickCheck
import Parsing
import Data.Maybe

-- pag 50 --

data PG = P | G | Dash
 deriving Eq

type PGString = [PG]

instance Show PG where
   show P = "p"
   show G = "g"
   show Dash = "-"
   showList = showString . concatMap show

instance Read PG where
   readsPrec _ pg
    = case pg of
      ('p':pg) -> [(P,pg)]
      ('g':pg) -> [(G,pg)]
      ('-':pg) -> [(Dash,pg)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary PG where
   arbitrary = elements [P,G,Dash]