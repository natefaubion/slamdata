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
import Data.Variant (on)

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.CardType (CardType)

data InsertableCardType
  = CacheCard
  | DraftboardCard
  | OpenCard
  | QueryCard
  | SearchCard
  | SetupDownloadCard
  | SetupMarkdownCard
  | SetupVariablesCard
  | ShowDownloadCard
  | ShowMarkdownCard
  | TableCard
  | TroubleshootCard
  | TabsCard
  | StructureEditorCard
  | ShowVizCard
  | SetupVizCard

data InsertableCardIOType
  = Viz
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
  , SetupDownloadCard × [ Data, Process ]
  , SetupMarkdownCard × [ None, Data, Process, Variables ]
  , ShowMarkdownCard × [ Markdown ]
  , QueryCard × [ None, Data, Variables ]
  , SearchCard × [ Data ]
  , SetupDownloadCard × [ Data ]
  , SetupMarkdownCard × [ None, Variables ]
  , TableCard × [ Data, Process ]
  , TroubleshootCard × [ Viz, Data, Process, Download, Markdown, Variables ]
  , TabsCard × [ None ]
  , StructureEditorCard × [ Data, Process ]
  , ShowVizCard × [ Viz ]
  , SetupVizCard × [ Data, Process ]
  , SetupVariablesCard × [ None ]
  ]

outputs ∷ Array (InsertableCardType × Array InsertableCardIOType)
outputs =
  [ CacheCard × [ Data ]
  , DraftboardCard × [ Variables ]
  , OpenCard × [ Data, Process ]
  , QueryCard × [ Data, Process ]
  , SearchCard × [ Data, Process ]
  , SetupDownloadCard × [ Download ]
  , SetupMarkdownCard × [ Markdown ]
  , SetupVariablesCard × [ Variables ]
  , ShowDownloadCard × [ Download ]
  , ShowMarkdownCard × [ Variables ]
  , TableCard × [ Data, Process ]
  , TroubleshootCard × [ Variables ]
  , StructureEditorCard × [ Data, Process ]
  , SetupVizCard × [ Viz ]
  , ShowVizCard × [ Data, Process ]
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
  Viz → "a visualization"
  Data → "data"
  Process → "process"
  Download → "a download"
  Markdown → "markdown"
  None → "to be the first card in a deck"
  Variables → "variables"
  Terminal → "nothing"


printIOType' ∷ InsertableCardIOType → Maybe String
printIOType' = case _ of
  Data → Just "the data"
  Viz → Just "the visualization"
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
  Port.ChartInstructions _, _ → Viz
  Port.PivotTable _, _ → Viz
  Port.SetupSelect _, _ → Viz
  Port.SetupInput _, _ → Viz
  Port.Variables, _ → Variables
  Port.ValueMetric _, _ → Viz
  Port.CategoricalMetric _, _ → Viz
  Port.GeoChart _, _ → Viz
  Port.Terminal, _ → Terminal
  _, _ → None

toCardType ∷ InsertableCardType → Maybe CardType
toCardType = case _ of
  CacheCard → Just CT.cache
  DraftboardCard → Just CT.draftboard
  OpenCard → Just CT.open
  QueryCard → Just CT.aceSql
  SearchCard → Just CT.search
  SetupVizCard → Just CT.setupViz
  SetupDownloadCard → Just CT.downloadOptions
  SetupMarkdownCard → Just CT.aceMarkdown
  SetupVariablesCard → Just CT.variables
  ShowDownloadCard → Just CT.download
  ShowVizCard → Just CT.viz
  ShowMarkdownCard → Just CT.markdown
  TableCard → Just CT.table
  TroubleshootCard → Just CT.troubleshoot
  TabsCard → Just CT.tabs
  StructureEditorCard → Just CT.structureEditor

print ∷ InsertableCardType → String
print = foldMap CT.name ∘ toCardType

aAn ∷ String → String
aAn s =
  if String.toLower (String.take 1 s) `Foldable.elem` vowels
    then "An"
    else "A"
  where
  vowels = [ "a", "e", "i", "o", "u" ]

reason ∷ InsertableCardIOType → CardType → String
reason io card = fold
  [ aAn $ CT.name card
  , " "
  , show $ CT.name card
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
  SetupVizCard → Just "set up a visualization for"
  SetupDownloadCard → Just "setup a download for"
  SetupMarkdownCard → Nothing
  SetupVariablesCard → Nothing
  ShowVizCard → Just "show"
  ShowDownloadCard → Just "show"
  ShowMarkdownCard → Just "show"
  TableCard → Just "tabulate"
  TroubleshootCard → Just "troubleshoot"
  TabsCard → Nothing
  StructureEditorCard → Nothing

fromCardType ∷ CardType → InsertableCardType
fromCardType = case_
  # fromSimple
  # fromAce
  where
  fromSimple ∷ ∀ r. (Variant r → InsertableCardType) → CT.Simple r → InsertableCardType
  fromSimple cb = cb
    # on CT._cache (const CacheCard)
    # on CT._draftboard (const DraftboardCard)
    # on CT._open (const OpenCard)
    # on CT._search (const SearchCard)
    # on CT._downloadOptions (const SetupDownloadCard)
    # on CT._variables (const SetupVariablesCard)
    # on CT._download (const ShowDownloadCard)
    # on CT._markdown (const ShowMarkdownCard)
    # on CT._table (const TableCard)
    # on CT._troubleshoot (const TroubleshootCard)
    # on CT._tabs (const TabsCard)
    # on CT._structureEditor (const StructureEditorCard)
    # on CT._viz (const ShowVizCard)
    # on CT._setupViz (const SetupVizCard)

  fromAce ∷ ∀ r. (Variant r → InsertableCardType) → CT.Ace r → InsertableCardType
  fromAce cb = cb
    # on CT._aceSql (const QueryCard)
    # on CT._aceMarkdown (const SetupMarkdownCard)

all ∷ Array InsertableCardType
all =
  [ OpenCard
  , QueryCard
  , SearchCard
  , TableCard
  , ShowVizCard
  , SetupVizCard
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
