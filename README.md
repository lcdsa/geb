# GEB
Libreria di Haskell fatta per "giocare" con alcuni concetti (anche musicali) proposti in "Gödel, Escher, Back: un'Eterna Ghirlanda Brillante" di Douglas R. Hofstadter. 

---

## Come funziona?

Un possibile approccio è il seguente: se state leggendo il libro e volete sapere se un certo concetto è stato implementato in questa libreria, usate `pag <num>` nell'interprete di Haskell e vedete cosa accade.
Stiamo lavorando anche ad un eseguibile (`Pagine`) che permette di scorrere i contenuti interattivamente anche fuori dall'interprete di Haskell.

---

## Indice per capitoli

- Introduzione: un'offerta musico-logica (3-30)
   - [Il Tema Regio](Bach.hs) (6)
   - [Canone inverso di Scott E. Kim sul tema Good King Wenceslas](GoodKingWenceslas.hs) (9)
-  Capitolo I: Il gioco MU (36-45)
   - [MIU.hs](MIU.hs) (36-39,43)
- Capitolo II: Significato e forma in matematica (50-66)
   - [Il sistema PG](PG.hs) (50)
- Capitolo III: Figura e sfondo (70-81)
   - [Rompicapo sequenza numerica](NumberSequence.hs) (80)

- ...

- Diaologo: Suite anglo-franco-italo-tedesca (397-399)
   - [Traduzione italiana alternativa](Jabberwocky.hs) (397)
---

### Note di installazione Euterpea (linux)

1. scarica un sound font qualunque (`.sf2`)
2. rinominalo `default.sf2` e salvalo in `/usr/share/soundfonts/`
3. avvia server MIDI (e.g. fluidsynth)
