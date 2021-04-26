module MIU where

import Parsing ( oneOrMore, parse, readsP )
import Data.List ( isInfixOf )
import Data.Maybe ( maybeToList, fromJust )
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

type Rule = Int

rule :: Rule -> MIU.String -> Maybe MIU.String
rule 1 miu | last miu == I       = Just $ miu ++ [U]
rule 2 (m:iu)                    = Just $ m:iu ++ iu
rule 3 miu | iii `isInfixOf` miu = Just $ rule3 miu
 where
   iii = replicate 3 I
   rule3 :: MIU.String -> MIU.String
   rule3 (I:I:I:iu) = U:iu
   rule3 (m:iu) = m : rule3 iu
   rule3 [] = []
rule 4 miu | uu `isInfixOf` miu  = Just $ rule4 miu
 where
   uu = replicate 2 U
   rule4 :: MIU.String -> MIU.String
   rule4 (U:U:iu) = iu
   rule4 (m:iu) = m : rule4 iu
   rule4 [] = []
rule _ _ = Nothing

-- pag 38 --

type Index = Int
type Theorem = MIU.String
newtype Derivation = Derivation [(Index, Rule)]

axioms :: [Theorem]
axioms = [read "MI"] -- MI is the only axiom

theorems :: Derivation -> Maybe [Theorem]
theorems = theorems' axioms

theorems' :: [Theorem] -> Derivation -> Maybe [Theorem]
theorems' ts (Derivation []) = Just ts
theorems' ts (Derivation ((i,r):d)) = do -- Maybe
   t <- rule r (ts !! (i - 1))
   theorems' (ts ++ [t]) (Derivation d)

sequential :: [Rule] -> Derivation
sequential = Derivation . zip [1..]

instance Show Derivation where
   show d@(Derivation xs) = unlines $ zipWith3
      (\ i t s -> "(" ++ show i ++ ")" ++ "  " ++ show t ++ "\t\t" ++ s)
      [1..]
      (fromJust (theorems d))
      ( replicate (length axioms) ('\t':"assioma")
         ++ map 
            ( \(i,r) -> "da (" ++ show i ++ ") con la regola " ++ roman r )
            xs
      )
    where 
      roman :: Rule -> Prelude.String
      roman 1 = "I"
      roman 2 = "II"
      roman 3 = "III"
      roman 4 = "IV"