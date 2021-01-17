module Eutermea where

import Euterpea
import Control.DeepSeq

-- | Plays music on systems where the default port is != 0
-- (port number is installation specific)
play :: Music Pitch -> IO ()
play = playDev 2

fromList :: [Music Pitch] -> Music Pitch
fromList = foldl (:+:) (rest 0)

-- | Given the duration of a rest and tune, it returns a canon of that tune
canon :: Dur -> Music Pitch -> Music Pitch
canon offset tune = tune :=: (rest offset :+: tune)

inverseCanon :: Dur -> Music Pitch -> Music Pitch
inverseCanon offset tune = tune :=: (rest offset :+: invert tune)