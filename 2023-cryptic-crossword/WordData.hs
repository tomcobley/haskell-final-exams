module WordData where

import System.IO
import Data.Maybe
import System.IO.Unsafe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
 
synonyms :: String -> [String]
synonyms s
  = lookUp s thesaurus 
  where
    lookUp x t
      = fromMaybe [] (Map.lookup x t)

thesaurus :: Map.Map String [String]
thesaurus
  = unsafePerformIO $ 
    do 
      ls <- fmap Text.lines (TextIO.readFile "thesaurus.txt")
      return (Map.fromList (fmap (readSyns . Text.unpack) ls))

readWord :: String -> String
readWord = read

readSyns :: String -> (String, [String])
readSyns = read

------------------------------------------

linkWords
  = ["","and","and","became","becomes","for","from","getting","give",
     "gives","got from","in","is","made by","makes","of","or",
     "provides","providing","show","that is","thats","to","to get",
     "to give","to make a","with"]

anagramIndicators
  = ["abandoned","abnormal","abnormally","about","absurd","abused",
     "abysmal","accident","adapt","adapted","adjusted","adrift",
     "affected","afresh","agitated","altered","alternative","amend",
     "amended","amendment","animated","anomalous","another","anyhow",
     "arrange","arrangement","arrangement of","assembled","assorted",
     "astray","at random","at sea","atrocious","awful","awkward","bad",
     "battered","batty","beaten","beaten up","berserk","bespoke",
     "bizarre","blend","boiled","brewed","broadcast","broken","changed",
     "changing","chaotic","choppy","churn","cocktail","compose",
     "composition","concoction","confused","contorted","contrived",
     "conversion","convert","converted","converts","convoluted","cooked",
     "corrupt","crazy","damaged","dancing","delirious","demented",
     "deranged","destroyed","destruction","devastated","developed",
     "deviant","devious","different","disarray","disfigured","dishevelled",
     "disorderly","disorganized","disorientated","dispersed","distorted",
     "distraught","distressed","disturbance","disturbed","dizzy","doctor",
     "doddery","dodgy","dreadful","dressed","drunk","errant","erratic",
     "erroneous","evolution","excited","exotic","extraordinary",
     "fabricated","fancy","flustered","frantic","frenzied","frenzy","fresh",
     "freshly","frightfully","funny","garbled","haphazard","harassed",
     "hectic","horribly","horrid","horrific","in a mess","in disarray",
     "in disguise","in error","in ruins","incorrect","inebriated","injured",
     "insanely","irritated","juggle","jumble","jumbled","ludicrous","lunatic",
     "mad","madness","malformed","maltreated","mangle","manic","manipulate",
     "manufacturing","mash","mashed","maul","mayhem","meandering","mess",
     "misbehaves","misbehaving","miserable","misshapen","mix","mixed",
     "mixed up","mixture","modification","muddled","mutation","mutilation",
     "nastily","naughty","new","novel","odd","organization","organized",
     "ornate","outlandish","outrageous","peculiar","perverse","perversion",
     "pervert","pickle","plastered","preparation","processed","pulverize",
     "randomly","rearranged","reassembled","rebuilt","recast","reconfigured",
     "reconstructed","reconstruction","recreated","rectified","recycled",
     "redeveloped","redistributed","reeling","refashioned","refined",
     "reformed","refurbished","regenerated","rehash","rejig","remade",
     "remodel","renovated","reorder","reshaped","reshuffle","revamp",
     "reviewed","revision","revolting","revolutionary","rework","rewritten",
     "ridiculous","riotously","rocky","rotten","ruin","ruination",
     "run wild","sadly","scramble","shaken","shambles","shattered","shot",
     "shuffled","sick","silly","slapdash","slaughtered","smash","smashed",
     "smashing","snarling","somehow","somewhat","sort","sort of","special",
     "spoilt","spurious","stew","strangely","swimming","swirling","tailor",
     "tainted","tormented","tortuous","tortured","transformed","translated",
     "translation","transposed","trashed","treated","treatment","tricky",
     "tumbling","turbulent","tweaked","twisted","unkempt","unruly",
     "unsettled","unstable","unusual","upheaval","vandalized","variation",
     "variety","vary","violently","volatile","warped","wayward","weird",
     "whisk","wild","winding","wrecked","writhing","wrong","zany"]

reversalIndicators
  = ["about","after recovery","back","backed","backward","backwards",
     "flipped","flipping","from east to west","returned","returning",
     "returns","reversed","revolves","springs back","to return",
     "turn","turned","turning","turns","up","upending"]

insertionIndicators
  = ["admitted to","amidst","amidst","amongst","amongst",
     "between","carried in by","entered","entering","enters",
     "featured in","filled","filling","fills","hidden in","in",
     "included in","inherent in","inside","inside","internal to",
     "intrinsic to","involved in","occupied","occupies","occupying",
     "penetrated","penetrates","penetrating","pierced","pierces",
     "piercing","put in","put in","put inside","put into",
     "ringed by","taken in by","tucked into","wearing","within"]

envelopeIndicators
  = ["about","about","accommodated","accommodates","accommodating",
     "admits","admitted","admitting","around","boxed","boxes",
     "boxing","bracketed","bracketing","brackets","buried","buries",
     "burying","carried","carries","carrying","catches","catching",
     "caught","circled","circles","circling","clutched","clutches",
     "clutching","consumed","consumes","consuming","contained",
     "containing","contains","crossing","embraced","embraces",
     "embracing","encased","encases","encasing","encircled",
     "encircles","encircling","enveloped","envelopes","enveloping",
     "flanked","flanking","flanks","going about","grasped","grasping",
     "grasps","harboured","harbouring","harbours","held","hid",
     "hides","hiding","holding","holding","holds","housed","houses",
     "housing","included","includes","including","inhabited by",
     "inhaled","inhales","inhaling","internalised","internalises",
     "internalising","outside","packed","packing","packs","receives",
     "restrained","restraining","restrains","ringed","ringing",
     "ringing","rings","sheltered","sheltering","shelters","surrounded",
     "surrounding","surrounds","swallowed","swallowing","swallows",
     "takes","takes in","taking in","took in","without","wrapped",
     "wrapping","wraps"]

beforeIndicators
  = ["","above","and","before","beside","by","given","has","having",
     "needing","next to","on","over","takes","to","with"]

afterIndicators
  = ["after","behind","follows","following","opposite"]

hiddenWordIndicators
  = ["amid","caught in","during","dwelling in","found in","from",
     "furnishing","hidden in","hiding in","in","inside","protecting",
     "some","within","witholding","witholds"]


