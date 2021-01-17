module Kim where

import Euterpea
import Eutermea

goodKingWenceslas :: Music Pitch
goodKingWenceslas
 = let 
   bar1_4
    = [ c  4 (1/4), c  4 (1/4), c  4 (1/4), d  4 (1/4)
      , c  4 (1/4), c  4 (1/4), g  3 (2/4)
      , a  3 (1/4), g  3 (1/4), a  3 (1/4), b  3 (1/4)
      , c  4 (2/4), c  4 (2/4)                        
      ]
   in 
   fromList
    $    bar1_4
      ++ bar1_4 
      ++ [ g  4 (1/4), f  4 (1/4), e  4 (1/4), d  4 (1/4)
         , e  4 (1/4), d  4 (1/4), c  4 (2/4)
         , a  3 (1/4), g  3 (1/4), a  3 (1/4), b  3 (1/4)
         , c  4 (2/4), c  4 (2/4)                        
         , g  3 (1/4), g  3 (1/4), a  3 (1/4), b  3 (1/4)
         , c  4 (1/4), c  4 (1/4), d  4 (2/4)  
         , g  4 (1/4), f  4 (1/4), e  4 (1/4), d  4 (1/4)
         , c  4 (2/4), f  4 (2/4)
         , c  4 (4/4)
         ]