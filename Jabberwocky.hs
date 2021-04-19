module Jabberwocky where

import Data.List

jabberwocky :: String
jabberwocky = 
    let a = [
            "Brillocco, con le svisce tove",
            "girava e gimbolava nella guana:",
            "mollacce parean le borogove",
            "e argeva ogni momerana."
            ]  
        b = [
            "\"Rifuggi il Mascellonte, figlia mia!",
            "Smascella e morde, artiglia e arruffa!",
            "Rifuggi il Giuggiurello e vola via",
            "dal frumio che bacciuffa!\""
            ]
        c = [
            "Serrò la vorpa spada nella mano",
            "e a lungo le sfuggì il nemico a mosa -",
            "si stese dunque sotto il Tutumano",
            "e restò lì pensosa."
            ]
        d = [
            "E mentre rifletteva sbuffa, fosco",
            "il Mascellonte, il fuoco nello sguardo",
            "corse sguillando pel fitto del bosco",
            "e venne burgugliando!"
            ]
        e = [
            "Oh-issa! Oh-issa! Ancora un po'!",
            "La spada vorpa sgozzosgnaccerò!",
            "Lo lasciò morto, il collo torto",
            "E indietro galfoppò."
            ]
        f = [
            "\"Dì, cadde il Mascellonte per davvero?",
            "Figlia raggiante, abbracciami, si addice!",
            "O giorno favoloso e trallallero!\"",
            "cantava sì felice."
            ]
    in intercalate "\n" (map unlines [a, b, c, d, e, f, a])