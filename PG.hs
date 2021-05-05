module PG where

import Parsing ( oneOrMore, parse, readsP )
import Data.Maybe ( maybeToList )
import Test.QuickCheck ( elements, Arbitrary(arbitrary), sized, Gen ())

-- pag 50 --

data Char = P | G | Dash
 deriving Eq

type String = [PG.Char]

instance Show PG.Char where
   show P = "p"
   show G = "g"
   show Dash = "-"
   showList = showString . concatMap show

instance Read PG.Char where
   readsPrec _ pg
    = case pg of
      ('p':pg) -> [(P,pg)]
      ('g':pg) -> [(G,pg)]
      ('-':pg) -> [(Dash,pg)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary PG.Char where
   arbitrary = elements [P,G,Dash]

type Theorem = PG.String

rTheorem :: Gen Theorem
rTheorem = sized $ \n -> do
   let sum = n + 2
   m <- elements [1..n+1]
   return  $ replicate m Dash 
      ++ P : replicate (sum - m) Dash 
      ++ G : replicate sum Dash

axioms = 
 [ let x = replicate n Dash in 
    x ++ read "p-g" ++ x ++ read "-" 
 | n <- [1..] 
 ]
