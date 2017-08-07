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

module SlamData.Workspace.Card.InsertableCardType where

import SlamData.Prelude
import Data.Array as Array
import Data.Foldable as Foldable
import Data.String as String
import SlamData.Workspace.Card.CardType as CardType
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.CardType (CardType)

data InsertableCardType
  = CacheCard
  | DraftboardCard
  | OpenCard
  | QueryCard
  | SearchCard
  | SetupChartCard
  | SetupFormCard
  | SetupDownloadCard
  | SetupMarkdownCard
  | SetupVariablesCard
  | ShowChartCard
  | ShowFormCard
  | ShowGeoChartCard
  | ShowDownloadCard
  | ShowMarkdownCard
  | TableCard
  | TroubleshootCard
  | TabsCard
  | StructureEditorCard
  | SetupGeoChartCard

data InsertableCardIOType
  = Chart
  | GeoChart
  | Form
  | Data
  | Process
  | Download
  | Markdown
  | Variables
  | Terminal
  | None

derive instance eqInsertableCardType ∷ Eq InsertableCardType
derive instance eqInsertableCardIOType ∷ Eq InsertableCardIOType

-- Cards can have multiple input types
inputs ∷ Array (InsertableCardType × Array InsertableCardIOType)
inputs =
  [ CacheCard × [ Data ]
  , DraftboardCard × [ None ]
  , OpenCard × [ None, Variables ]
  , QueryCard × [ None, Data, Process, Variables ]
  , SearchCard × [ Data, Process ]
  , SetupChartCard × [ Data, Process ]
  , SetupGeoChartCard × [ Data, Process ]
  , SetupDownloadCard × [ Data, Process ]
  , SetupFormCard × [ Data, Process ]
  , SetupMarkdownCard × [ None, Data, Process, Variables ]
  , SetupVariablesCard × [ None ]
  , ShowGeoChartCard × [ GeoChart ]
  , ShowChartCard × [ Chart ]
  , ShowFormCard × [ Form ]
  , ShowDownloadCard × [ Download ]
  , ShowMarkdownCard × [ Markdown ]
  , TableCard × [ Data, Process ]
  , TroubleshootCard × [ Chart, Form, Data, Process, Download, Markdown, Variables ]
  , TabsCard × [ None ]
  , StructureEditorCard × [ Data, Process ]
  ]

outputs ∷ Array (InsertableCardType × Array InsertableCardIOType)
outputs =
  [ CacheCard × [ Data ]
  , DraftboardCard × [ Variables ]
  , OpenCard × [ Data, Process ]
  , QueryCard × [ Data, Process ]
  , SearchCard × [ Data, Process ]
  , SetupChartCard × [ Chart ]
  , SetupFormCard × [ Form ]
  , SetupDownloadCard × [ Download ]
  , SetupMarkdownCard × [ Markdown ]
  , SetupVariablesCard × [ Variables ]
  , ShowChartCard × [ Data, Process ]
  , ShowFormCard × [ Data, Process ]
  , ShowDownloadCard × [ Download ]
  , ShowMarkdownCard × [ Variables ]
  , TableCard × [ Data, Process ]
  , TroubleshootCard × [ Variables ]
  , StructureEditorCard × [ Data, Process ]
  ]

cardsToExcludeFromPaths ∷ Array InsertableCardType
cardsToExcludeFromPaths =
  [ TroubleshootCard
  , TableCard
  , DraftboardCard
  , CacheCard
  , TabsCard
  ]

contains ∷ ∀ a. (Eq a) ⇒ a → Array a → Boolean
contains x = isJust ∘ Array.elemIndex x

takesInput ∷ InsertableCardIOType → InsertableCardType → Boolean
takesInput io card = Foldable.elem card $ cardsThatTakeInput io

inputsFor ∷ InsertableCardType → Array InsertableCardIOType
inputsFor card = snd =<< Array.filter (eq card ∘ fst) inputs

outputsFor ∷ InsertableCardType → Array InsertableCardIOType
outputsFor card = snd =<< Array.filter (eq card ∘ fst) outputs

cardsThatOutput ∷ InsertableCardIOType → Array InsertableCardType
cardsThatOutput io =
  map fst $ Array.filter (Foldable.elem io ∘ snd) outputs

cardsThatTakeInput ∷ InsertableCardIOType → Array InsertableCardType
cardsThatTakeInput io =
  map fst $ Array.filter (contains io ∘ snd) inputs

type Path =
  { c ∷ Array InsertableCardType, io ∷ Array InsertableCardIOType, f ∷ Boolean }

purePath ∷ InsertableCardIOType → Path
purePath io =
  { c: [], io: [io], f: false }

cardPathsFromPaths ∷ Array Path → Array (Array InsertableCardType)
cardPathsFromPaths = map _.c

allPathsFinished ∷ Array Path → Boolean
allPathsFinished = Foldable.all _.f

cardPathsBetween ∷ InsertableCardIOType → InsertableCardType → Array (Array InsertableCardType)
cardPathsBetween fromIO toCard =
  Array.filter (not ∘ Foldable.elem toCard) $ cardPathsFromPaths $ pathsBetween fromIO toCard

pathsBetween ∷ InsertableCardIOType → InsertableCardType → Array Path
pathsBetween fromIO toCard =
  Array.concat $ pathsBetweenIO fromIO <$> inputsFor toCard

cardPathNeeded ∷ Array (Array InsertableCardType) → Boolean
cardPathNeeded cardPaths =
  not $ Array.null cardPaths || [] `Foldable.elem` cardPaths

possibleToGetTo ∷ InsertableCardIOType → InsertableCardType → Boolean
possibleToGetTo io card =
  not $ Array.null $ Array.concat $ pathsBetweenIO io <$> inputsFor card

-- This produces an array of sensible suggested paths. It excludes paths which
-- include cards in the cardsToExcludeFromPaths array and paths which include
-- the same io type more than twice.
--
-- Allowing the same io type twice rather than only once allows sets of cards
-- such as (SearchCard) and (SetupFormCard, ShowFormCard) to be included in
-- paths.
--
-- Not allowing the same data type more than twice prevents duplicate sets of
-- cards such as (SearchCard) and (SetupFormCard, ShowFormCard) as well as
-- preventing infinite loops.
pathsBetweenIO ∷ InsertableCardIOType → InsertableCardIOType → Array Path
pathsBetweenIO fromIO toIO =
  go $ [ purePath toIO ]
  where
  go ∷ Array Path → Array Path
  go paths | allPathsFinished paths = paths
  go paths | otherwise =
    go $ Array.concat $ expandPath fromIO <$> paths

expandPath ∷ InsertableCardIOType → Path → Array Path
expandPath fromIO initialPath | initialPath.f =
  [ initialPath ]
expandPath fromIO initialPath | maybe true (eq fromIO) (Array.head initialPath.io) =
  [ initialPath { f = true } ]
expandPath fromIO initialPath | otherwise =
  maybe [] f $ Array.head initialPath.io
  where
  f ∷ InsertableCardIOType → Array Path
  f initialIO = Array.concat $ g <$> (cardsThatOutput initialIO)

  g ∷ InsertableCardType → Array Path
  g card = h card `Array.mapMaybe` (inputsFor card)

  h ∷ InsertableCardType → InsertableCardIOType → Maybe Path
  h card io
    | card `Foldable.elem` initialPath.c
        || (Array.length $ Array.filter (eq io) initialPath.io) >= 2
        || card `Foldable.elem` cardsToExcludeFromPaths =
          Nothing
  h card io =
    Just { c: pathCards, io: pathIO, f: pathFinished }
    where
    pathFinished = fromIO `Foldable.elem` pathIO
    pathIO = Array.cons io initialPath.io
    pathCards = Array.cons card initialPath.c

fromMaybeOut ∷ Maybe Port.Out → InsertableCardIOType
fromMaybeOut input = maybe None fromOut input

printIOType ∷ InsertableCardIOType → String
printIOType = case _ of
  Form → "a form"
  Chart → "a chart"
  GeoChart → "geo chart"
  Data → "data"
  Process → "process"
  Download → "a download"
  Markdown → "markdown"
  None → "to be the first card in a deck"
  Variables → "variables"
  Terminal → "nothing"


printIOType' ∷ InsertableCardIOType → Maybe String
printIOType' = case _ of
  Form → Just "the form"
  Chart → Just "the chart"
  Data → Just "the data"
  Download → Just "the download"
  Markdown → Just "the markdown"
  Variables → Just "the variables"
  _ → Nothing

eitherOr ∷ Array String → String
eitherOr strings =
  case Array.length strings of
    1 → String.joinWith "" strings
    n →
      "either "
        <> String.joinWith ", " (Array.take (n - 1) strings)
        <> " or "
        <> String.joinWith "" (Array.drop (n - 1) strings)

fromOut ∷ Port.Out → InsertableCardIOType
fromOut = uncurry case _, _ of
  Port.ResourceKey key, varMap
    | Just (VM.Resource (VM.Process _ _ _)) ← VM.lookup (VM.Var key) varMap → Process
    | otherwise → Data
  Port.DownloadOptions _, _ → Download
  Port.SlamDown _, _ → Markdown
  Port.ChartInstructions _, _ → Chart
  Port.PivotTable _, _ → Chart
  Port.SetupLabeledFormInput _, _ → Form
  Port.SetupTextLikeFormInput _, _ → Form
  Port.Variables, _ → Variables
  Port.ValueMetric _, _ → Chart
  Port.CategoricalMetric _, _ → Form
  Port.GeoChart _, _ → GeoChart
  Port.Terminal, _ → Terminal
  _, _ → None

toCardType ∷ InsertableCardType → Maybe CardType
toCardType = case _ of
  CacheCard → Just CardType.Cache
  DraftboardCard → Just CardType.Draftboard
  OpenCard → Just CardType.Open
  QueryCard → Just $ CardType.Ace CardType.SQLMode
  SearchCard → Just CardType.Search
  SetupChartCard → Nothing
  SetupGeoChartCard → Nothing
  SetupFormCard → Nothing
  SetupDownloadCard → Just CardType.DownloadOptions
  SetupMarkdownCard → Just $ CardType.Ace CardType.MarkdownMode
  SetupVariablesCard → Just $ CardType.Variables
  ShowChartCard → Just CardType.Chart
  ShowGeoChartCard → Just CardType.GeoChart
  ShowFormCard → Just CardType.FormInput
  ShowDownloadCard → Just CardType.Download
  ShowMarkdownCard → Just CardType.Markdown
  TableCard → Just CardType.Table
  TroubleshootCard → Just CardType.Troubleshoot
  TabsCard → Just CardType.Tabs
  StructureEditorCard → Just CardType.StructureEditor

print ∷ InsertableCardType → String
print = case _ of
  SetupChartCard → "Setup Chart"
  SetupFormCard → "Setup Form"
  a → foldMap CardType.cardName $ toCardType a

aAn ∷ String → String
aAn s =
  if String.toLower (String.take 1 s) `Foldable.elem` vowels
    then "An"
    else "A"
  where
  vowels = [ "a", "e", "i", "o", "u" ]

reason ∷ InsertableCardIOType → CardType → String
reason io card = fold
  [ aAn $ CardType.cardName card
  , " "
  , show $ CardType.cardName card
  , " card can't "
  , actual
  , " because it needs "
  , expected
  , action
  , "."
  ]
  where
  ictCardType =
    fromCardType card
  actual = case io of
    None → "be the first card in a deck"
    _ → "follow a card which outputs " <> printIOType io
  expected =
    eitherOr $ map printIOType $ inputsFor ictCardType
  action =
    foldMap (append " to ") $ printAction ictCardType

printAction ∷ InsertableCardType → Maybe String
printAction = case _ of
  CacheCard → Just "cache"
  DraftboardCard → Nothing
  OpenCard → Nothing
  QueryCard → Nothing
  SearchCard → Just "search"
  SetupChartCard → Just "set up a chart for"
  SetupGeoChartCard → Just "set up geo chart for"
  SetupFormCard → Just "set up a form for"
  SetupDownloadCard → Just "setup a download for"
  SetupMarkdownCard → Nothing
  SetupVariablesCard → Nothing
  ShowChartCard → Just "show"
  ShowGeoChartCard → Just "show"
  ShowFormCard → Just "show"
  ShowDownloadCard → Just "show"
  ShowMarkdownCard → Just "show"
  TableCard → Just "tabulate"
  TroubleshootCard → Just "troubleshoot"
  TabsCard → Nothing
  StructureEditorCard → Nothing

fromCardType ∷ CardType → InsertableCardType
fromCardType = case _ of
  CardType.Cache → CacheCard
  CardType.Draftboard → DraftboardCard
  CardType.Open → OpenCard
  CardType.Ace CardType.SQLMode → QueryCard
  CardType.Search → SearchCard
  CardType.ChartOptions _ → SetupChartCard
  CardType.SetupGeoChart _ → SetupGeoChartCard
  CardType.GeoChart → ShowGeoChartCard
  CardType.DownloadOptions → SetupDownloadCard
  CardType.Ace CardType.MarkdownMode → SetupMarkdownCard
  CardType.Variables → SetupVariablesCard
  CardType.Chart → ShowChartCard
  CardType.Download → ShowDownloadCard
  CardType.Markdown → ShowMarkdownCard
  CardType.Table → TableCard
  CardType.Troubleshoot → TroubleshootCard
  CardType.SetupFormInput _ → SetupFormCard
  CardType.FormInput → ShowFormCard
  CardType.Tabs → TabsCard
  CardType.StructureEditor → StructureEditorCard


all ∷ Array InsertableCardType
all =
  [ OpenCard
  , QueryCard
  , SearchCard
  , TableCard
  , SetupChartCard
  , ShowChartCard
  , SetupFormCard
  , ShowFormCard
  , SetupGeoChartCard
  , ShowGeoChartCard
  , SetupMarkdownCard
  , ShowMarkdownCard
  , DraftboardCard
  , TabsCard
  , SetupDownloadCard
  , ShowDownloadCard
  , CacheCard
  , SetupVariablesCard
  , TroubleshootCard
  , StructureEditorCard
  ]
