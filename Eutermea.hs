module Eutermea where

import Euterpea
import Control.DeepSeq

-- | Plays music on systems where the default port is != 0
-- (port number is installation specific)
play :: (ToMusic1 a, NFData a) => Music a -> IO ()
play = playDev 2

fromList :: [Music Pitch] -> Music Pitch
fromList = foldl (:+:) (rest 0)

-- | Given the duration of a rest and tune, it returns a canon of that tune
canon :: Dur -> Music a -> Music a
canon offset tune = tune :=: (rest offset :+: tune)