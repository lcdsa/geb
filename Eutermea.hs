module Eutermea where

import Euterpea
import Control.DeepSeq

-- port number is installation specific
play :: (ToMusic1 a, NFData a) => Music a -> IO ()
play = playDev 2