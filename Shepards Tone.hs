-- Shepards Tone

import Euterpea
import Codec.Midi

-- Base one octave chromatic scale starting on C4
chrSc = addDur qn [c 4, d 4, e 4, f 4, g 4, a 4, b 4]

-- Normative Pitch, C4 which all volume calculations are based off of the note's distance from this pitch
np = (C,4)

-- Put the sequence together
-- Side note: Timbre has a huge effect on how well the illusion works, on the default instrument
--     (a piano) the reintroduction of the bass, the part of this trick that really breaks the
--     illusion, is very prominent.  With this PanFlute, the reintroduction of the bass is much 
--     less prominent (At least on my computer).
shepTone = play $ cut 32 $ repeatM $ instrument Flute $ multipleChrSc (-4) (4)

-- Plays multiple 
multipleChrSc :: Int -> Int -> Music(Pitch, Volume)
multipleChrSc i j
           | i < j  = setVol (difTranspose (i * 12) chrSc) :=: multipleChrSc (i + 1) j
           | i == j = setVol (difTranspose (i * 12 ) chrSc)
           | otherwise = setVol (Prim(Rest 0))

-- Replaces the normal transpose function to work better with multipleChrSc
difTranspose :: AbsPitch -> Music Pitch -> Music Pitch
difTranspose x ((Prim (Note d p)) :+: m2) = (Prim (Note d (pitch (absPitch p + x)))) :+: difTranspose x m2
difTranspose x ((Prim (Note d p)))        = (Prim (Note d (pitch (absPitch p + x)))) 
difTranspose _ _                          = Prim(Rest 0)

-- setVol scales the volume of each note relative to its distance from middle C (C,4)
--        For each note away from (C,4), subtract 3 from the maximum midi volume (127)
setVol :: Music Pitch -> Music(Pitch, Volume)
setVol ((Prim (Note d p)) :+: m2) = addVolume (127 - 3 * (abs (round (toRational (absPitch p - absPitch np))))) (Prim (Note d p)) :+: setVol(m2)
setVol (Prim (Note d p)) =  addVolume (127 - 1 * (abs (round (toRational (absPitch p - absPitch np))))) (Prim (Note d p))
setVol _ = Prim (Rest 0)

-- Adds a rhthmic value (quarter note, eigth note, etc) to a line of notes
addDur :: Dur -> [Dur -> Music a ] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)