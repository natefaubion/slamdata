{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Setups.ColorScheme where

import SlamData.Prelude

import Color (Color, fromHexString, toHSLA, hsla)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Array as A

import SlamData.Form.Select (class OptionVal, Select(..))

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

colors ∷ Array Color
colors =
  A.catMaybes
  $ map fromHexString
  [ "#93A9A6"
  , "#CDA71F"
  , "#EB6F76"
  , "#66B35B"
  , "#BD97E2"
  , "#5B5925"
  , "#884A6F"
  , "#51B3F6"
  , "#CCA067"
  , "#398465"
  , "#3C607A"
  , "#81463C"
  , "#B65F33"
  , "#9AAE31"
  , "#CE8B9E"
  , "#6C6356"
  , "#95A779"
  , "#44AECB"
  , "#E987C2"
  , "#8A7EA0"
  , "#3D6C2F"
  , "#40B994"
  , "#87984A"
  , "#C1A088"
  , "#9B6E2D"
  , "#428C8D"
  , "#B8766A"
  , "#EB8666"
  , "#DF883E"
  , "#BB6273"
  , "#C994BF"
  , "#929DE0"
  , "#7BA5CB"
  , "#AE9093"
  , "#66557B"
  , "#936370"
  , "#3C6D64"
  , "#84693F"
  , "#C19744"
  , "#E77AA1"
  , "#5D555F"
  , "#4078A2"
  , "#3FAFAF"
  , "#698B99"
  , "#486A4C"
  , "#7EA48B"
  , "#B57E57"
  , "#8C72AA"
  , "#609050"
  , "#56B379"
  , "#489F8B"
  , "#714D4A"
  , "#9A8867"
  , "#93B66B"
  , "#7DA93F"
  , "#877424"
  , "#C75D56"
  , "#B774AC"
  , "#7B7A3E"
  , "#73581C"
  , "#398EA3"
  , "#964734"
  , "#DF8D89"
  , "#AF97CC"
  , "#96951F"
  , "#A37791"
  , "#7C4D2E"
  , "#78865F"
  , "#216B74"
  , "#935524"
  , "#6FAFB6"
  , "#75AB76"
  , "#A48B50"
  , "#D28DD0"
  , "#BE9AAF"
  , "#AD8D22"
  , "#D89576"
  , "#964860"
  , "#9B9A61"
  , "#4DAADB"
  , "#A9628D"
  , "#98943B"
  , "#486366"
  , "#6D7E2B"
  , "#CF9A2F"
  , "#827A8B"
  , "#876A69"
  , "#495F23"
  , "#677F45"
  , "#805845"
  , "#A2544D"
  , "#8C5157"
  , "#6B6C9E"
  , "#236443"
  , "#919B82"
  , "#CC8E55"
  , "#3E8555"
  , "#A08A7A"
  , "#767870"
  , "#6D9643"
  , "#87658F"
  , "#3BB069"
  , "#6A5D42"
  , "#586249"
  , "#1F7769"
  , "#6DAF8E"
  , "#8FA7BE"
  , "#B7A82C"
  , "#A09DA0"
  , "#7D8AA6"
  , "#78A3E0"
  , "#719186"
  , "#765771"
  , "#A37EA7"
  , "#8E8CBC"
  , "#A76840"
  , "#49934B"
  , "#A27C62"
  , "#3DA27B"
  , "#A9AC53"
  , "#6685B4"
  , "#5F728A"
  , "#CB6B4A"
  , "#9F8DD3"
  , "#B7A66E"
  , "#A998B3"
  , "#85A362"
  , "#595146"
  ]

blueGreen ∷ Array Color
blueGreen =
  A.catMaybes
  $ map fromHexString
  [ "#f7fcfd"
  , "#e5f5f9"
  , "#ccece6"
  , "#99d8c9"
  , "#66c2a4"
  , "#41ae76"
  , "#238b45"
  , "#006d2c"
  , "#00441b"
  ]

bluePurple ∷ Array Color
bluePurple =
  A.catMaybes
  $ map fromHexString
  [ "#f7fcfd"
  , "#e0ecf4"
  , "#bfd3e6"
  , "#9ebcda"
  , "#8c96c6"
  , "#8c6bb1"
  , "#88419d"
  , "#810f7c"
  , "#4d004b"
  ]

greenBlue ∷ Array Color
greenBlue =
  A.catMaybes
  $ map fromHexString
  [ "#f7fcf0"
  , "#e0f3db"
  , "#ccebc5"
  , "#a8ddb5"
  , "#7bccc4"
  , "#4eb3d3"
  , "#2b8cbe"
  , "#0868ac"
  , "#084081"
  ]

orangeRed ∷ Array Color
orangeRed =
  A.catMaybes
  $ map fromHexString
  [ "#fff7ec"
  , "#fee8c8"
  , "#fdd49e"
  , "#fdbb84"
  , "#fc8d59"
  , "#ef6548"
  , "#d7301f"
  , "#b30000"
  , "#7f0000"
  ]

purpleBlue ∷ Array Color
purpleBlue =
  A.catMaybes
  $ map fromHexString
  [ "#fff7fb"
  , "#ece7f2"
  , "#d0d1e6"
  , "#a6bddb"
  , "#74a9cf"
  , "#3690c0"
  , "#0570b0"
  , "#045a8d"
  , "#023858"
  ]

purpleBlueGreen ∷ Array Color
purpleBlueGreen =
  A.catMaybes
  $ map fromHexString
  [ "#fff7fb"
  , "#ece2f0"
  , "#d0d1e6"
  , "#a6bddb"
  , "#67a9cf"
  , "#3690c0"
  , "#02818a"
  , "#016c59"
  , "#014636"
  ]

purpleRed ∷ Array Color
purpleRed =
  A.catMaybes
  $ map fromHexString
  [ "#f7f4f9"
  , "#e7e1ef"
  , "#d4b9da"
  , "#c994c7"
  , "#df65b0"
  , "#e7298a"
  , "#ce1256"
  , "#980043"
  , "#67001f"
  ]

redPurple ∷ Array Color
redPurple =
  A.catMaybes
  $ map fromHexString
  [ "#fff7f3"
  , "#fde0dd"
  , "#fcc5c0"
  , "#fa9fb5"
  , "#f768a1"
  , "#dd3497"
  , "#ae017e"
  , "#7a0177"
  , "#49006a"
  ]

yellowGreen ∷ Array Color
yellowGreen =
  A.catMaybes
  $ map fromHexString
  [ "#ffffe5"
  , "#f7fcb9"
  , "#d9f0a3"
  , "#addd8e"
  , "#78c679"
  , "#41ab5d"
  , "#238443"
  , "#006837"
  , "#004529"
  ]

yellowGreenBlue ∷ Array Color
yellowGreenBlue =
  A.catMaybes
  $ map fromHexString
  [ "#ffffd9"
  , "#edf8b1"
  , "#c7e9b4"
  , "#7fcdbb"
  , "#41b6c4"
  , "#1d91c0"
  , "#225ea8"
  , "#253494"
  , "#081d58"
  ]

yellowOrangeBrown ∷ Array Color
yellowOrangeBrown =
  A.catMaybes
  $ map fromHexString
  [ "#ffffe5"
  , "#fff7bc"
  , "#fee391"
  , "#fec44f"
  , "#fe9929"
  , "#ec7014"
  , "#cc4c02"
  , "#993404"
  , "#662506"
  ]

yellowOrangeRed ∷ Array Color
yellowOrangeRed =
  A.catMaybes
  $ map fromHexString
  [ "#ffffcc"
  , "#ffeda0"
  , "#fed976"
  , "#feb24c"
  , "#fd8d3c"
  , "#fc4e2a"
  , "#e31a1c"
  , "#bd0026"
  , "#800026"
  ]

blues ∷ Array Color
blues =
  A.catMaybes
  $ map fromHexString
  [ "#f7fbff"
  , "#deebf7"
  , "#c6dbef"
  , "#9ecae1"
  , "#6baed6"
  , "#4292c6"
  , "#2171b5"
  , "#08519c"
  , "#08306b"
  ]

greens ∷ Array Color
greens =
  A.catMaybes
  $ map fromHexString
  [ "#f7fcf5"
  , "#e5f5e0"
  , "#c7e9c0"
  , "#a1d99b"
  , "#74c476"
  , "#41ab5d"
  , "#238b45"
  , "#006d2c"
  , "#00441b"
  ]

greys ∷ Array Color
greys =
  A.catMaybes
  $ map fromHexString
  [ "#ffffff"
  , "#f0f0f0"
  , "#d9d9d9"
  , "#bdbdbd"
  , "#969696"
  , "#737373"
  , "#525252"
  , "#252525"
  , "#000000"
  ]

oranges ∷ Array Color
oranges =
  A.catMaybes
  $ map fromHexString
  [ "#fff5eb"
  , "#fee6ce"
  , "#fdd0a2"
  , "#fdae6b"
  , "#fd8d3c"
  , "#f16913"
  , "#d94801"
  , "#a63603"
  , "#7f2704"
  ]

purples ∷ Array Color
purples =
  A.catMaybes
  $ map fromHexString
  [ "#fcfbfd"
  , "#efedf5"
  , "#dadaeb"
  , "#bcbddc"
  , "#9e9ac8"
  , "#807dba"
  , "#6a51a3"
  , "#54278f"
  , "#3f007d"
  ]

reds ∷ Array Color
reds =
  A.catMaybes
  $ map fromHexString
  [ "#fff5f0"
  , "#fee0d2"
  , "#fcbba1"
  , "#fc9272"
  , "#fb6a4a"
  , "#ef3b2c"
  , "#cb181d"
  , "#a50f15"
  , "#67000d"
  ]

brownToBlueGreen ∷ Array Color
brownToBlueGreen =
  A.catMaybes
  $ map fromHexString
  [ "#8c510a"
  , "#bf812d"
  , "#dfc27d"
  , "#f6e8c3"
  , "#f5f5f5"
  , "#c7eae5"
  , "#80cdc1"
  , "#35978f"
  , "#01665e"
  ]

pinkToYellowGreen ∷ Array Color
pinkToYellowGreen =
  A.catMaybes
  $ map fromHexString
  [ "#c51b7d"
  , "#de77ae"
  , "#f1b6da"
  , "#fde0ef"
  , "#f7f7f7"
  , "#e6f5d0"
  , "#b8e186"
  , "#7fbc41"
  , "#4d9221"
  ]

purpleToGreen ∷ Array Color
purpleToGreen =
  A.catMaybes
  $ map fromHexString
  [ "#762a83"
  , "#9970ab"
  , "#c2a5cf"
  , "#e7d4e8"
  , "#f7f7f7"
  , "#d9f0d3"
  , "#a6dba0"
  , "#5aae61"
  , "#1b7837"
  ]

purpleToOrange ∷ Array Color
purpleToOrange =
  A.catMaybes
  $ map fromHexString
  [ "#b35806"
  , "#e08214"
  , "#fdb863"
  , "#fee0b6"
  , "#f7f7f7"
  , "#d8daeb"
  , "#b2abd2"
  , "#8073ac"
  , "#542788"
  ]

redToBlue ∷ Array Color
redToBlue =
  A.catMaybes
  $ map fromHexString
  [ "#b2182b"
  , "#d6604d"
  , "#f4a582"
  , "#fddbc7"
  , "#f7f7f7"
  , "#d1e5f0"
  , "#92c5de"
  , "#4393c3"
  , "#2166ac"
  ]

redToYellowToBlue ∷ Array Color
redToYellowToBlue =
  A.catMaybes
  $ map fromHexString
  [ "#d73027"
  , "#f46d43"
  , "#fdae61"
  , "#fee090"
  , "#ffffbf"
  , "#e0f3f8"
  , "#abd9e9"
  , "#74add1"
  , "#4575b4"
  ]

data ColorScheme
  = BlueGreen
  | BluePurple
  | GreenBlue
  | OrangeRed
  | PurpleBlue
  | PurpleBlueGreen
  | PurpleRed
  | RedPurple
  | YellowGreen
  | YellowGreenBlue
  | YellowOrangeBrown
  | YellowOrangeRed
  | Blues
  | Greens
  | Greys
  | Oranges
  | Purples
  | Reds
  | BrownToBlueGreen
  | PinkToYellowGreen
  | PurpleToGreen
  | PurpleToOrange
  | RedToBlue
  | RedToYellowToBlue

colorSchemes ∷ Array ColorScheme
colorSchemes =
  [ BlueGreen
  , BluePurple
  , GreenBlue
  , OrangeRed
  , PurpleBlue
  , PurpleBlueGreen
  , PurpleRed
  , RedPurple
  , YellowGreen
  , YellowGreenBlue
  , YellowOrangeBrown
  , YellowOrangeRed
  , Blues
  , Greens
  , Greys
  , Oranges
  , Purples
  , Reds
  , BrownToBlueGreen
  , PinkToYellowGreen
  , PurpleToGreen
  , PurpleToOrange
  , RedToBlue
  , RedToYellowToBlue
  ]

printColorScheme ∷ ColorScheme → String
printColorScheme = case _ of
  BlueGreen → "sequential: blue green"
  BluePurple → "sequential: blue purple"
  GreenBlue → "sequential: green blue"
  OrangeRed → "sequential: orange red"
  PurpleBlue → "sequential: purple blue"
  PurpleBlueGreen → "sequential: purple blue green"
  PurpleRed → "sequential: purple red"
  RedPurple → "sequential: red purple"
  YellowGreen → "sequential: yellow green"
  YellowGreenBlue → "sequential: yellow green blue"
  YellowOrangeBrown → "sequential: yellow orange brown"
  YellowOrangeRed → "sequential: yellow orange red"
  Blues → "sequential: blues"
  Greens → "sequential: greens"
  Greys → "sequential: greys"
  Oranges → "sequential: oranges"
  Purples → "sequential: purples"
  Reds → "sequential: reds"
  BrownToBlueGreen → "diverging: brown-blue green"
  PinkToYellowGreen → "diverging: pink-yellow green"
  PurpleToGreen → "diverging: purple-green"
  PurpleToOrange → "diverging: purple-orange"
  RedToBlue → "diverging: red-blue"
  RedToYellowToBlue → "diverging: red-yellow-blue"

parseColorScheme ∷ String → String ⊹ ColorScheme
parseColorScheme = case _ of
  "sequential: blue green" → pure BlueGreen
  "sequential: blue purple" → pure BluePurple
  "sequential: green blue" → pure GreenBlue
  "sequential: orange red" → pure OrangeRed
  "sequential: purple blue" → pure PurpleBlue
  "sequential: purple blue green" → pure PurpleBlueGreen
  "sequential: purple red" → pure PurpleRed
  "sequential: red purple" → pure RedPurple
  "sequential: yellow green" → pure YellowGreen
  "sequential: yellow green blue" → pure YellowGreenBlue
  "sequential: yellow orange brown" → pure YellowOrangeBrown
  "sequential: yellow orange red" → pure YellowOrangeRed
  "sequential: blues" → pure Blues
  "sequential: greens" → pure Greens
  "sequential: greys" → pure Greys
  "sequential: oranges" → pure Oranges
  "sequential: purples" → pure Purples
  "sequential: reds" → pure Reds
  "diverging: brown-blue green" → pure BrownToBlueGreen
  "diverging: pink-yellow green" → pure PinkToYellowGreen
  "diverging: purple-green" → pure PurpleToGreen
  "diverging: purple-orange" → pure PurpleToOrange
  "diverging: red-blue" → pure RedToBlue
  "diverging: red-yellow-blue" → pure RedToYellowToBlue
  _ → throwError "This is not color scheme"


getColorScheme ∷ ColorScheme → Array Color
getColorScheme = case _ of
  BlueGreen → blueGreen
  BluePurple → bluePurple
  GreenBlue → greenBlue
  OrangeRed → orangeRed
  PurpleBlue → purpleBlue
  PurpleBlueGreen → purpleBlueGreen
  PurpleRed → purpleRed
  RedPurple → redPurple
  YellowGreen → yellowGreen
  YellowGreenBlue → yellowGreenBlue
  YellowOrangeBrown → yellowOrangeBrown
  YellowOrangeRed → yellowOrangeRed
  Blues → blues
  Greens → greens
  Greys → greys
  Oranges → oranges
  Purples → purples
  Reds → reds
  BrownToBlueGreen → brownToBlueGreen
  PinkToYellowGreen → pinkToYellowGreen
  PurpleToGreen → purpleToGreen
  PurpleToOrange → purpleToOrange
  RedToBlue → redToBlue
  RedToYellowToBlue → redToYellowToBlue


derive instance eqColorScheme ∷ Eq ColorScheme
derive instance ordColorScheme ∷ Ord ColorScheme

instance arbitraryColorScheme ∷ Arbitrary ColorScheme where
  arbitrary = allInArray colorSchemes

instance encodeJsonColorScheme ∷ EncodeJson ColorScheme where
  encodeJson = encodeJson ∘ printColorScheme

instance decodeJsonColorScheme ∷ DecodeJson ColorScheme where
  decodeJson = decodeJson >=> parseColorScheme

instance optionValColorScheme ∷ OptionVal ColorScheme where
  stringVal = printColorScheme

colorSchemeSelect ∷ Select ColorScheme
colorSchemeSelect =
  Select { options: colorSchemes
         , value: Just RedToBlue
         }

getShadeColor ∷ Color → Number → Color
getShadeColor color alpha =
  setAlpha (lightenTo color 0.95) alpha

getTransparentColor ∷ Color → Number → Color
getTransparentColor color alpha =
  setAlpha color alpha

lightenTo ∷ Color → Number → Color
lightenTo col l' = hsla c.h c.s l' c.a
  where
  c = toHSLA col

setAlpha ∷ Color → Number → Color
setAlpha col a' = hsla c.h c.s c.l a'
  where
  c = toHSLA col
