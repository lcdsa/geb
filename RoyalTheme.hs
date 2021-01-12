module RoyalTheme where

import Euterpea

royalTheme :: Music Pitch
royalTheme = foldl (:+:) (rest 0) [
    -- bar by bar
    c 5 hn, ef 5 hn, 
    g 5 hn, af 5 hn, 
    b 4 hn, rest 1, g 5 qn, 
    fs 5 hn, f 5 hn,
    e 5 hn, ef 5 dhn, 
    d 5 qn, df 5 qn, c 5 qn, 
    b 4 qn, a 4 en, g 4 en, c 5 qn, f 5 qn, 
    ef 5 hn, d 5 hn, 
    c 5 qn]

