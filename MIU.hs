{-# LANGUAGE OverloadedStrings #-}
module MIU where

import GHC.Exts( IsString(..) )
import Data.List ( isInfixOf )
import Data.Maybe 
import Test.QuickCheck 

-- pag 36 --

data Char = M | I | U
 deriving (Eq, Read, Show)

newtype String = String [MIU.Char]
   deriving Eq

instance IsString MIU.String where
   fromString = String . map translate
    where
      translate :: Prelude.Char -> MIU.Char
      translate 'M' = M
      translate 'I' = I
      translate 'U' = U
      translate ___ = undefined

instance Show MIU.String where
   show (String cs) = concatMap show cs 

instance Arbitrary MIU.Char where
   arbitrary = elements [M,I,U]

instance Arbitrary MIU.String where
   arbitrary = sized $ \ n -> String <$> 
      do k <- choose (2,n+2)
         vectorOf k arbitrary

randomString :: Gen MIU.String
randomString = arbitrary

-- pag 37 --

ownedStrings :: [MIU.String]
ownedStrings = ["MI"]

type Rule = Int

rule :: Rule -> MIU.String -> Maybe MIU.String
rule 1 (String miu) | last miu == I       = Just $ String $ miu ++ [U]
rule 2 (String (m:iu))                    = Just $ String $ m:iu ++ iu
rule 3 (String miu) | iii `isInfixOf` miu = Just $ String $ rule3 miu
 where
   iii = replicate 3 I
   rule3 :: [MIU.Char] -> [MIU.Char]
   rule3 (I:I:I:iu) = U:iu
   rule3 (m:iu) = m : rule3 iu
   rule3 [] = []
rule 4 (String miu) | uu `isInfixOf` miu  = Just $ String $ rule4 miu
 where
   uu = replicate 2 U
   rule4 :: [MIU.Char] -> [MIU.Char]
   rule4 (U:U:iu) = iu
   rule4 (m:iu) = m : rule4 iu
   rule4 [] = []
rule _ _ = Nothing

-- pag 38 --

axioms :: [MIU.String]
axioms = ownedStrings

type Derivation = [(MIU.String, Rule)] -- bottom-up
data Dunno = Dunno | Known Bool
 deriving ( Eq, Show )

isKnownTrue :: Dunno -> Bool
isKnownTrue (Known True) = True
isKnownTrue _ = False

isKnownFalse :: Dunno -> Bool
isKnownFalse (Known False) = True
isKnownFalse _ = False

isTheorem :: MIU.String -> Dunno
isTheorem _ = Dunno

isTheorem' :: MIU.String -> Derivation -> Dunno
isTheorem' s d | s == (fst $ head d) && isValid d = Known True
isTheorem' _ _ = Dunno

isValid :: Derivation -> Bool
isValid [] = True
isValid [(s,_)] | s `elem` axioms = True
isValid ((s,r):((s',r'):d)) | Just s == rule r s' && isValid ((s',r'):d) = True
isValid _ = False

derivation :: [Rule] -> Derivation
derivation = derivation' [(a,0) | a <- axioms]

derivation' :: Derivation -> [Rule] -> Derivation
derivation' d [] = d
derivation' d (r:rs) = do
   s <- maybeToList $ rule r (fst $ head d)
   derivation' ((s,r):d) rs

