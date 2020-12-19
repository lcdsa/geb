module MIU where

import Parsing
import Data.List ( isInfixOf )
import Data.Maybe ( maybeToList )
import Test.QuickCheck

-- pag 36 --
data MIU_ = M | I | U
 deriving Eq

type MIU = [MIU_]

instance Show MIU_ where
   show M = "M"
   show I = "I"
   show U = "U"
   showList = showString . concatMap show

instance Read MIU_ where
   readsPrec _ miu
    = case miu of
      ('M':iu) -> [(M,iu)]
      ('I':iu) -> [(I,iu)]
      ('U':iu) -> [(U,iu)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary MIU_ where
   arbitrary = elements [M,I,U]

-- pag 37 --

type Rule = Int

rule :: Rule -> MIU -> MIU
rule 1 miu | last miu == I = miu ++ [U]
rule 2 (m:iu) = m:iu ++ iu
rule 3 miu | iii `isInfixOf` miu = rule3 miu
 where
   iii = replicate 3 I
   rule3 :: MIU -> MIU
   rule3 (I:I:I:iu) = U:iu
   rule3 (m:iu) = m : rule3 iu
   rule3 [] = []
rule 4 miu | uu `isInfixOf` miu = rule4 miu
 where
   uu = replicate 2 U
   rule4 :: MIU -> MIU
   rule4 (U:U:iu) = iu
   rule4 (m:iu) = m : rule4 iu
   rule4 [] = []
rule _ miu = miu

-- pag 39 --

derivation :: [MIU]
derivation
 = do let m1 = read "MI"
      let m2 = rule 2 m1
      let m3 = rule 2 m2
      let m4 = rule 1 m3
      let m5 = rule 3 m4
      let m6 = rule 2 m5
      let m7 = rule 4 m6
      [m1,m2,m3,m4,m5,m6,m7]

-- pag 50 --

data PG_ = P | G | Sign
 deriving Eq

type PG = [PG_]

instance Show PG_ where
   show P = "p"
   show G = "g"
   show Sign = "-"
   showList = showString . concatMap show

instance Read PG_ where
   readsPrec _ pg
    = case pg of
      ('p':pg) -> [(P,pg)]
      ('g':pg) -> [(G,pg)]
      ('-':pg) -> [(Sign,pg)]
      ________ -> []
   readList = maybeToList . parse (oneOrMore readsP)

instance Arbitrary PG_ where
   arbitrary = elements [P,G,Sign]