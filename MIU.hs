module MIU where

import Parsing
import Data.List ( isInfixOf )
import Data.Maybe ( maybeToList )
import Test.QuickCheck

-- pag 36 --
data MIU = M | I | U
 deriving Eq

type MIUString = [MIU]

instance Show MIU where
   show M = "M"
   show I = "I"
   show U = "U"
   showList = showString . concatMap show

instance Read MIU where
   readsPrec _ miu
    = case miu of
      ('M':iu) -> [(M,iu)]
      ('I':iu) -> [(I,iu)]
      ('U':iu) -> [(U,iu)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary MIU where
   arbitrary = elements [M,I,U]

-- pag 37 --

type Rule = Int

rule :: Rule -> MIUString -> MIUString
rule 1 miu | last miu == I = miu ++ [U]
rule 2 (m:iu) = m:iu ++ iu
rule 3 miu | iii `isInfixOf` miu = rule3 miu
 where
   iii = replicate 3 I
   rule3 :: MIUString -> MIUString
   rule3 (I:I:I:iu) = U:iu
   rule3 (m:iu) = m : rule3 iu
   rule3 [] = []
rule 4 miu | uu `isInfixOf` miu = rule4 miu
 where
   uu = replicate 2 U
   rule4 :: MIUString -> MIUString
   rule4 (U:U:iu) = iu
   rule4 (m:iu) = m : rule4 iu
   rule4 [] = []
rule _ miu = miu

-- pag 39 --

derivation :: [MIUString]
derivation
 = do let m1 = read "MI"
      let m2 = rule 2 m1
      let m3 = rule 2 m2
      let m4 = rule 1 m3
      let m5 = rule 3 m4
      let m6 = rule 2 m5
      let m7 = rule 4 m6
      [m1,m2,m3,m4,m5,m6,m7]