module MIU where

import Prelude hiding ( Char, String )
import Parsing ( oneOrMore, parse, readsP )
import Data.List ( isInfixOf )
import Data.Maybe ( maybeToList )
import Test.QuickCheck ( elements, Arbitrary(arbitrary) )

-- pag 36 --

data Char = M | I | U
 deriving Eq

type String = [MIU.Char]

instance Show MIU.Char where
   show M = "M"
   show I = "I"
   show U = "U"
   showList = showString . concatMap show

instance Read MIU.Char where
   readsPrec _ miu
    = case miu of
      ('M':iu) -> [(M,iu)]
      ('I':iu) -> [(I,iu)]
      ('U':iu) -> [(U,iu)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary MIU.Char where
   arbitrary = elements [M,I,U]

-- pag 37 --

theorems :: [MIU.String]
theorems = [read "MI" :: MIU.String]

type Rule = Int

rule :: Rule -> MIU.String -> MIU.String
rule 1 miu | last miu == I = miu ++ [U]
rule 2 (m:iu) = m:iu ++ iu
rule 3 miu | iii `isInfixOf` miu = rule3 miu
 where
   iii = replicate 3 I
   rule3 :: MIU.String -> MIU.String
   rule3 (I:I:I:iu) = U:iu
   rule3 (m:iu) = m : rule3 iu
   rule3 [] = []
rule 4 miu | uu `isInfixOf` miu = rule4 miu
 where
   uu = replicate 2 U
   rule4 :: MIU.String -> MIU.String
   rule4 (U:U:iu) = iu
   rule4 (m:iu) = m : rule4 iu
   rule4 [] = []
rule _ miu = miu

-- pag 39 --

derivation :: [MIU.String]
derivation
 = do let m1 = read "MI"
      let m2 = rule 2 m1
      let m3 = rule 2 m2
      let m4 = rule 1 m3
      let m5 = rule 3 m4
      let m6 = rule 2 m5
      let m7 = rule 4 m6
      [m1,m2,m3,m4,m5,m6,m7]
