module NumberSequence where

import Data.List
import Data.Maybe

-- the sequence at pag. 80 
figure :: [Int]
figure = [1,3,7,12,18,26,35,45,56]

-- | Given a sequence such constructed as figure, return the numbers already 
-- skipped and the numbers to skip
skip :: [Int] -> ([Int],[Int])
skip figure = (skipped, toSkip)
    where 
        figure' = background figure 
        lastSkipped = last figure - last (init figure)
        toSkip = dropWhile (<= lastSkipped) figure'
        skipped = takeWhile (<= lastSkipped) figure'

-- | Given a sequence constructed as figure, return the numbers already 
-- skipped
skipped :: [Int] -> [Int]
skipped = fst . skip

-- | Given a sequence constructed as figure, return the numbers to skip
toSkip :: [Int] -> [Int]
toSkip = snd . skip

-- | Numbers not in a sequence ("background" of the "figure")
background :: [Int] -> [Int]
background figure = [1..(last figure)] \\ figure

-- | Given a sequence such as figure, return the next number, unless a
-- decision is required
next :: [Int] -> Maybe Int
next figure = if null $ toSkip figure 
    then Nothing 
    else Just $ last figure + head (toSkip figure)

-- | Given a sequence such as figure, continue it until possible
-- (tends to be infinite ;)
continue :: [Int] -> [Int] 
continue figure = case next figure of
    (Just n) -> continue (figure ++ [n])
    Nothing -> figure

-- | Given a sequence such as figure, continue it with its m successors,
-- if possible
continue' :: [Int] -> Int -> [Int]
continue' figure 0 = figure
continue' figure m = case next figure of
    (Just n) -> continue' (figure ++ [n]) (m - 1)
    Nothing -> figure