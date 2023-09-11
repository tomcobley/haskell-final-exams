module Clues where

import Types

clues :: [Clue]
clues
  
  -- Double definitions (synonyms)...
  = [-- concern/problem [double definition]
     ("Business worry",7), 
  
  -- Anagrams...
     -- rotates [anag of TOASTER]
     ("New toaster revolves",7), 
     -- redshank [anag of HENSDARK]
     ("Hen's dark - unusual bird",8), 
     -- master [anag of STREAM]
     ("Perfect meandering stream",6), 
   
  -- Reversals...
     -- edam [reversal of MADE]
     ("Cheese made from east to west",4), 
     -- repaid [reversal of DIAPER]
     ("Recompensed for returning nappy", 6), 
 
  -- Envelope insertions (i.e. of the form X around Y)...
     -- amnesty [AMY around IN]
     ("Pardon girl ringing home",7), 
     -- remainder [REMINDER around A]
     ("Hint about a rest",9), 
     -- sustain [STAIN around US]
     ("Spot inhabited by American bear",7), 

  -- Standard insertion (i.e. of the form X within Y)...
     -- loire [I in LORE, i.e. a regular insertion]
     ("River, one featured in superstitions",5),

  -- Unindicated charades...
     -- cabin [CAB + IN]
     ("Taxi home from hut",5), 
   
  -- Indicated charades...
     -- snappy [S +[needing] NAPPY]
     ("Irritable, son needing diaper",6), 
 
  -- Compositions of the above...
     -- fremantle [FRET around MAN +[on} LE]
     ("Worry about chap on the French port",9), 
     -- nasty [(A + ST) in NY]
     ("A street in New York that's filthy",5), 
     -- rotate [reversal of OR +[by] TATE]
     ("Turn or turning by gallery",6),
     -- inapt [IN + anagram of TAP]
     ("Unsuitable at home, ornate tap",5), 
     -- kensington [SING in KENT + ON]
     ("Carol, in county, working for palace",10), 
     -- defiant [reversal of FED + I + ANT]
     ("Fed up, one worker becomes uncooperative",7),
     -- speed [reversal of DEEP + S]
     ("Race about deep South",5),
   
  -- Hidden words...
     -- "ling" [gilLINGham]
     ("Fish from Gillingham", 4),
     -- edward [fittED WARDrobe]
     ("Boy hiding in fitted wardrobe",6), 
     -- hera [motHER Anyway]
     ("Goddess? Some mother, anyway",4), 
     -- large [molecuLAR GEnetics]
     ("Big in molecular genetics",5),
     -- tofu [vaT OF Unpleasant]
     ("Found in vat of unpleasant curd", 4)]

