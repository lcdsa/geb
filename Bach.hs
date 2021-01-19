module Bach where

import Euterpea
import Eutermea

royalTheme :: Music Pitch
royalTheme = fromList [
    -- bar by bar
    c 5 (1/2), ef 5 (1/2), 
    g 5 (1/2), af 5 (1/2), 
    b 4 (1/2), rest (1/4), g 5 (1/4), 
    fs 5 (1/2), f 5 (1/2),
    e 5 (1/2), ef 5 (3/4), 
    d 5 (1/4), df 5 (1/4), c 5 (1/4), 
    b 4 (1/4), a 4 (1/8), g 4 (1/8), c 5 (1/4), f 5 (1/4), 
    ef 5 (1/2), d 5 (1/2), 
    c 5 (1/4)]

