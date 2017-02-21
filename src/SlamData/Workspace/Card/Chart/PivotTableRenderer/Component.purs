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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Component where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.Formatter.Number as FN
import Data.Int as Int
import Data.Lens ((^.))
import Data.List (List, (:))
import Data.List as List
import Data.String as String

import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Quasar.Query as QQ
import SlamData.Render.CSS.New as CSS
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTRM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (Column(..))

import Global (readFloat)
import Utils (hush)

type Input =
  { port ∷ Port.PivotTablePort
  , resource ∷ Port.Resource
  }

type State =
  { input ∷ Maybe Input
  , count ∷ Int
  , pageCount ∷ Int
  , pageIndex ∷ Int
  , pageSize ∷ Int
  , rawRecords ∷ Maybe (Array J.Json)
  , records ∷ Maybe (PTree J.Json J.Json)
  , customPage ∷ Maybe String
  , loading ∷ Boolean
  }

initialState ∷ State
initialState =
  { input: Nothing
  , count: 0
  , pageCount: 0
  , pageIndex: 0
  , pageSize: PTRM.initialModel.pageSize
  , rawRecords: Nothing
  , records: Nothing
  , customPage: Nothing
  , loading: false
  }

data Query a
  = Update Port.PivotTablePort Port.Resource a
  | Load PTRM.Model a
  | Save (PTRM.Model → a)
  | StepPage PageStep a
  | SetCustomPage String a
  | UpdatePage Event a
  | ChangePageSize String a

data Message
  = ModelUpdated

data PageStep
  = First
  | Prev
  | Next
  | Last

type DSL = H.ComponentDSL State Query Message Slam
type HTML = H.ComponentHTML Query

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render st =
  case st.input of
    Just { port } →
      HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-table" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "sd-pivot-table-content" ] ]
            [ maybe (HH.text "") (renderTable port.dimensions port.columns) st.records ]
        , HH.div
            [ HP.classes
                [ HH.ClassName "sd-pagination"
                , HH.ClassName "sd-form"
                ]
            ]
            [ prevButtons (st.pageIndex > 0)
            , pageField st.pageIndex st.customPage st.pageCount
            , nextButtons (st.pageIndex < st.pageCount - 1)
            , pageSizeControls st.pageSize
            ]
            , if st.loading
                then HH.div [ HP.classes [ HH.ClassName "loading" ] ] []
                else HH.text ""
        ]
    _ →
      HH.text ""
  where
  renderTable dims cols tree =
    if st.count ≡ 0
      then
        HH.div
          [ HP.classes [ HH.ClassName "no-results" ] ]
          [ HH.text "No results" ]
      else
        HH.table_
            $ [ HH.tr_
                $ (dims <#> \d → HH.th_ [ HH.text (fst d) ])
                ⊕ (cols <#> case _ of
                      n × Column { value } →
                        HH.th_ [ HH.text n ]
                      _ × Count →
                        HH.th_ [ HH.text "COUNT" ])
            ]
            ⊕ renderRows cols tree


  renderRows cols =
    map HH.tr_ ∘ foldTree (renderLeaves cols) renderHeadings

  renderLeaves cols =
    foldMap (renderLeaf cols)

  renderLeaf cols row =
    let
      rowLen = sizeOfRow cols row
    in
      Array.range 0 (rowLen - 1) <#> \rowIx →
        flip foldMap cols \(c × col) →
          let
            text = J.cursorGet (topField c) row <#> case rowIx, col of
              0, Column { valueAggregation: Just ag } →
                foldJsonArray'
                  renderJson
                  (show ∘ Ag.runAggregation ag ∘ jsonNumbers)
              _, Column { valueAggregation: Just ag } →
                foldJsonArray'
                  (const "")
                  (maybe "" renderJson ∘ flip Array.index rowIx)
              _, Column _ →
                foldJsonArray'
                  renderJson
                  (maybe "" renderJson ∘ flip Array.index rowIx)
              0, Count →
                J.foldJsonNumber "" (FN.format numFormatter)
              _, Count →
                const ""
            in
              [ HH.td_ [ HH.text (fromMaybe "" text) ] ]

  jsonNumbers =
    Array.mapMaybe (J.foldJsonNumber Nothing Just)

  renderHeadings =
    foldMap renderHeading

  renderHeading (k × rs) =
    case Array.uncons rs of
      Just { head, tail } →
        Array.cons
          (Array.cons
            (HH.th [ HP.rowSpan (Array.length rs) ] [ HH.text (renderJson k) ])
            head)
          tail
      Nothing →
        []

  renderJson =
    J.foldJson show show showPrettyNum id show show

  showJCursor (J.JField i c) = i <> show c
  showJCursor c = show c

  showPrettyNum n =
    let s = show n
    in fromMaybe s (String.stripSuffix (String.Pattern ".0") s)

  numFormatter =
    { comma: true
    , before: 0
    , after: 0
    , abbreviations: false
    , sign: false
    }

  prevButtons enabled =
    HH.div
      [ HP.class_ CSS.formButtonGroup ]
      [ HH.button
          [ HP.class_ CSS.formButton
          , HP.disabled (not enabled)
          , HE.onClick $ HE.input_ (StepPage First)
          ]
          [ glyph B.glyphiconFastBackward ]
      , HH.button
          [ HP.class_ CSS.formButton
          , HP.disabled (not enabled)
          , HE.onClick $ HE.input_ (StepPage Prev)
          ]
          [ glyph B.glyphiconStepBackward ]
      ]

  pageField currentPage customPage totalPages =
    HH.div_
      [ HH.form
          [ HE.onSubmit (HE.input UpdatePage) ]
          [ HH.text "Page"
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.value (fromMaybe (show (currentPage + 1)) customPage)
              , HE.onValueInput (HE.input SetCustomPage)
              ]
          , HH.text $ "of " <> show totalPages
          ]
      ]

  nextButtons enabled =
    HH.div
      [ HP.class_ CSS.formButtonGroup ]
      [ HH.button
          [ HP.disabled (not enabled)
          , HE.onClick $ HE.input_ (StepPage Next)
          ]
          [ glyph B.glyphiconStepForward ]
      , HH.button
          [ HP.disabled (not enabled)
          , HE.onClick $ HE.input_ (StepPage Last)
          ]
          [ glyph B.glyphiconFastForward ]
      ]

  pageSizeControls pageSize =
    let
      sizeValues = [10, 25, 50, 100]
      options = sizeValues <#> \value →
        HH.option
          [ HP.selected (value ≡ pageSize) ]
          [ HH.text (show value) ]
    in
      HH.div_
        [ HH.select
            [ HE.onValueChange (HE.input ChangePageSize) ]
            options
        ]

eval ∷ Query ~> DSL
eval = case _ of
  Update port resource next → do
    st ← H.get
    let input = { port, resource }
    if fromMaybe false (eq resource ∘ _.resource <$> st.input)
      then
        H.modify _ { input = Just input }
      else do
        H.modify _ { input = Just input, pageIndex = 0, count = 0, pageCount = 0 }
        ifSimpleQuery pageQuery loadTree input
    pure next
  StepPage step next → do
    st ← H.get
    let
      pageIndex = clamp 0 (st.pageCount - 1) case step of
        First → 0
        Prev  → st.pageIndex - 1
        Next  → st.pageIndex + 1
        Last  → st.pageCount - 1
    H.modify _ { pageIndex = pageIndex }
    for st.input (ifSimpleQuery pageQuery pageTree)
    pure next
  SetCustomPage page next → do
    H.modify _ { customPage = Just page }
    pure next
  UpdatePage ev next → do
    H.liftEff $ preventDefault ev
    st ← H.get
    for_ st.customPage \page → do
      let
        pageIndex = clamp 0 (st.pageCount - 1) (Int.floor (readFloat page) - 1)
      H.modify _
        { pageIndex = pageIndex
        , customPage = Nothing
        }
      for st.input (ifSimpleQuery pageQuery pageTree)
    pure next
  ChangePageSize size next → do
    st ← H.get
    let
      pageSize  = Int.floor (readFloat size)
    H.modify _ { pageSize = pageSize }
    for st.input (ifSimpleQuery pageQuery pageTree)
    H.raise ModelUpdated
    pure next
  Load model next → do
    H.modify _ { pageSize = model.pageSize }
    pure next
  Save k → do
    { pageSize } ← H.get
    pure $ k { pageSize }

ifSimpleQuery
  ∷ (Input → DSL Unit)
  → (Input → DSL Unit)
  → Input
  → DSL Unit
ifSimpleQuery f g p =
  if p.port.isSimpleQuery then f p else g p

pageQuery ∷ Input → DSL Unit
pageQuery input = do
  st ← H.get
  let
    filePath = input.resource ^. Port._filePath
    offset = st.pageIndex * st.pageSize
    limit = st.pageSize
  H.modify _ { loading = true }
  if st.count ≡ 0
    then do
      count ← either (const 0) id <$> QQ.count filePath
      H.modify _
        { count = count
        , pageCount = calcPageCount count st.pageSize
        }
    else do
      H.modify _
        { pageCount = calcPageCount st.count st.pageSize
        }
  records ← QQ.sample filePath offset limit
  H.modify _ { loading = false }
  for_ records \recs →
    H.modify _ { records = Just (buildTree mempty Bucket Grouped recs) }

pageTree ∷ Input → DSL Unit
pageTree input = do
  st ← H.get
  for_ st.rawRecords \records → do
    let
      dims = List.fromFoldable $ J.cursorGet ∘ topField ∘ fst <$> input.port.dimensions
      tree = buildTree dims Bucket Grouped records
      pages = pagedTree st.pageSize (sizeOfRow input.port.columns) tree
      pageCount = Array.length (snd pages)
      pageIndex = clamp 0 (pageCount - 1) st.pageIndex
    H.modify _
      { records = Array.index (snd pages) pageIndex
      , count = fst pages
      , pageCount = pageCount
      , pageIndex = pageIndex
      }

loadTree ∷ Input → DSL Unit
loadTree input = do
  let filePath = input.resource ^. Port._filePath
  H.modify _ { loading = true }
  records ← QQ.all filePath
  H.modify _
    { loading = false
    , rawRecords = hush records
    }
  pageTree input

calcPageCount ∷ Int → Int → Int
calcPageCount count size =
  Int.ceil (Int.toNumber count / Int.toNumber size)

sizeOfRow ∷ Array (String × Column) → J.Json → Int
sizeOfRow columns row =
  fromMaybe 1
    (F.maximum
      (Array.mapMaybe
        case _ of
          c × Column { valueAggregation: Just _ } → Just 1
          c × _ → J.foldJsonArray 1 Array.length <$> J.cursorGet (topField c) row
        columns))

topField ∷ String → J.JCursor
topField c = J.JField c J.JCursorTop

data PTree k a
  = Bucket (Array a)
  | Grouped (Array (k × PTree k a))

foldTree
  ∷ ∀ k a r
  . (Array a → r)
  → (Array (k × r) → r)
  → PTree k a
  → r
foldTree f g (Bucket a) = f a
foldTree f g (Grouped as) = g (map (foldTree f g) <$> as)

buildTree
  ∷ ∀ k a r
  . Eq k
  ⇒ List (a → Maybe k)
  → (Array a → r)
  → (Array (k × r) → r)
  → Array a
  → r
buildTree List.Nil f g as = f as
buildTree (k : ks) f g as =
  g (fin (foldl go { key: Nothing, group: [], acc: [] } as))
  where
  go res@{ key: mbKey, group, acc } a =
    case mbKey, k a of
      Just key, Just key' | key == key' →
        { key: mbKey, group: Array.snoc group a, acc }
      _, Just key' →
        { key: Just key', group: [a], acc: fin res }
      _, Nothing →
        res
  fin { key, group, acc } =
    case key of
      Just key' →
        Array.snoc acc (key' × (buildTree ks f g group))
      Nothing →
        acc

pagedTree
  ∷ ∀ k a
  . Int
  → (a → Int)
  → PTree k a
  → Int × Array (PTree k a)
pagedTree page sizeOf tree =
  case tree of
    Bucket as → map Bucket <$> chunked page sizeOf as
    Grouped gs → map Grouped <$> chunked page (sizeOf' ∘ snd) gs
  where
  sizeOf' (Bucket as) = F.sum (sizeOf <$> as)
  sizeOf' (Grouped gs) = F.sum (sizeOf' ∘ snd <$> gs)

chunked
  ∷ ∀ b
  . Int
  → (b → Int)
  → Array b
  → Int × Array (Array b)
chunked page sizeOf arr = res.total × Array.snoc res.chunks res.chunk
  where
  res =
    foldl go { total: 0, size: 0, chunk: [], chunks: []} arr

  go { total, size, chunk, chunks } a =
    let
      s2 = sizeOf a
      t2 = total + s2
    in
      case size + s2, chunk of
        size', [] | size' > page →
          { total: t2, size: 0, chunk, chunks: Array.snoc chunks [a] }
        size', _  | size' > page →
          { total: t2, size: s2, chunk: [a], chunks: Array.snoc chunks chunk }
        size', _ →
          { total: t2, size: size', chunk: Array.snoc chunk a, chunks }

foldJsonArray'
  ∷ ∀ a
  . (J.Json → a)
  → (J.JArray → a)
  → J.Json
  → a
foldJsonArray' f g j = J.foldJson f' f' f' f' g f' j
  where
  f' ∷ ∀ b. b → a
  f' _ = f j
