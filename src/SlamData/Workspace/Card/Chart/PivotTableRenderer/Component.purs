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
import Data.List (List, (:))
import Data.List as List
import Data.Path.Pathy as P
import Data.String as String

import Halogen as H
import Halogen.Component.Utils (raise)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (JSONMode(..))

import SlamData.Monad (Slam)
import SlamData.Quasar.Class (liftQuasar)
import SlamData.Quasar.Query as QQ
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS.New as CSS
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (Column(..), isSimple)
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTRM
import SlamData.Workspace.Card.Port (PivotTablePort, eqTaggedResourcePort)

import Global (readFloat)

type State =
  { input ∷ Maybe PivotTablePort
  , count ∷ Int
  , pageCount ∷ Int
  , pageIndex ∷ Int
  , pageSize ∷ Int
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
  , records: Nothing
  , customPage: Nothing
  , loading: false
  }

data Query a
  = Update PivotTablePort a
  | Load PTRM.Model a
  | Save (PTRM.Model → a)
  | StepPage PageStep a
  | SetCustomPage String a
  | UpdatePage a
  | ChangePageSize String a
  | ModelUpdated a

data PageStep
  = First
  | Prev
  | Next
  | Last

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render st =
  case st.input of
    Just { options } →
      HH.div
        [ HP.classes [ HH.className "sd-pivot-table" ] ]
        [ HH.div
            [ HP.classes [ HH.className "sd-pivot-table-content" ] ]
            [ maybe (HH.text "") (renderTable options.dimensions options.columns) st.records ]
        , HH.div
            [ HP.classes
                [ HH.className "sd-pagination"
                , HH.className "sd-form"
                ]
            ]
            [ prevButtons (st.pageIndex > 0)
            , pageField st.pageIndex st.customPage st.pageCount
            , nextButtons (st.pageIndex < st.pageCount - 1)
            , pageSizeControls st.pageSize
            ]
            , if st.loading
                then HH.div [ HP.classes [ HH.className "loading" ] ] []
                else HH.text ""
        ]
    _ →
      HH.text ""
  where
  renderTable dims cols tree =
    let
      cols' = Array.mapWithIndex (Tuple ∘ add (Array.length dims)) cols
    in
      if st.count ≡ 0
        then
          HH.div
            [ HP.classes [ HH.className "no-results" ] ]
            [ HH.text "No results" ]
        else
          HH.table_
              $ [ HH.tr_
                  $ (if Array.null dims
                     then []
                     else [ HH.td [ HP.colSpan (Array.length dims) ] [] ])
                  ⊕ (cols <#> case _ of
                        Column { value } →
                          HH.th_ [ HH.text (showJCursor value) ]
                        Count →
                          HH.th_ [ HH.text "COUNT" ])
              ]
              ⊕ renderRows cols' tree


  renderRows cols =
    map HH.tr_ ∘ foldTree (renderLeaves cols) renderHeadings

  renderLeaves cols =
    foldMap (renderLeaf cols)

  renderLeaf cols row =
    let
      rowLen = sizeOfRow cols row
    in
      Array.range 0 (rowLen - 1) <#> \rowIx →
        flip foldMap cols \(ix × col) →
          let
            text = J.cursorGet (tupleN ix) row <#> case rowIx, col of
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
    in fromMaybe s (String.stripSuffix ".0" s)

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
      [ submittable UpdatePage
          [ HH.text "Page"
          , HH.input
              [ HP.inputType HP.InputNumber
              , HP.value (fromMaybe (show (currentPage + 1)) customPage)
              , HE.onValueInput (HE.input SetCustomPage)
              ]
          , HH.text $ "of " <> show totalPages
          ]
      ]

  submittable ctr =
    HH.form
      [ HE.onSubmit \_ →
          HEH.preventDefault $> Just (H.action ctr)
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
  Update input next | isSimple input.options → do
    st ← H.get
    let
      sameResource =
        case input.taggedResource, st.input of
          tr1, Just { query, options, taggedResource: tr2 } →
            eqTaggedResourcePort tr1 tr2 && input.query ≡ query
          _, _ → false
    if sameResource
      then do
        H.modify _ { input = Just input }
        when (isNothing st.records) do
          pageQuery input
      else do
        H.modify _ { input = Just input, pageIndex = 0, count = 0, pageCount = 0 }
        pageQuery input
    pure next
  Update input next → do
    st ← H.get
    H.modify _ { input = Just input }
    pageTree input
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
    for st.input (ifSimpleInput pageQuery pageTree)
    pure next
  SetCustomPage page next → do
    H.modify _ { customPage = Just page }
    pure next
  UpdatePage next → do
    st ← H.get
    for_ st.customPage \page → do
      let
        pageIndex = clamp 0 (st.pageCount - 1) (Int.floor (readFloat page) - 1)
      H.modify _
        { pageIndex = pageIndex
        , customPage = Nothing
        }
      for st.input (ifSimpleInput pageQuery pageTree)
    pure next
  ChangePageSize size next → do
    st ← H.get
    let
      pageSize  = Int.floor (readFloat size)
    H.modify _ { pageSize = pageSize }
    for st.input (ifSimpleInput pageQuery pageTree)
    raise (H.action ModelUpdated)
    pure next
  Load model next → do
    H.modify _ { pageSize = model.pageSize }
    pure next
  Save k → do
    { pageSize } ← H.get
    pure $ k { pageSize }
  ModelUpdated next →
    pure next

ifSimpleInput
  ∷ (PivotTablePort → DSL Unit)
  → (PivotTablePort → DSL Unit)
  → PivotTablePort
  → DSL Unit
ifSimpleInput f g p =
  if isSimple (p.options) then f p else g p

pageQuery ∷ PivotTablePort → DSL Unit
pageQuery input = do
  st ← H.get
  let
    path   = fromMaybe P.rootDir (P.parentDir input.taggedResource.resource)
    offset = st.pageIndex * st.pageSize
    limit  = st.pageSize
  H.modify _ { loading = true }
  if st.count ≡ 0
    then do
      count ← either (const 0) id <$>
        QQ.count input.taggedResource.resource
      H.modify _
        { count = count
        , pageCount = calcPageCount count st.pageSize
        }
    else do
      H.modify _
        { pageCount = calcPageCount st.count st.pageSize
        }
  records ← liftQuasar (QF.readQuery Readable path input.query mempty (Just { offset, limit }))
  H.modify _ { loading = false }
  for_ records \recs →
    H.modify _
      { records = Just (buildTree mempty Bucket Grouped recs)
      }

pageTree ∷ PivotTablePort → DSL Unit
pageTree input = do
  st ← H.get
  let
    dlen      = Array.length input.options.dimensions
    dims      = List.fromFoldable (dimensionsN (Array.length input.options.dimensions))
    cols      = Array.mapWithIndex (Tuple ∘ add dlen) input.options.columns
    records   = buildTree dims Bucket Grouped input.records
    pages     = pagedTree st.pageSize (sizeOfRow cols) records
    pageCount = Array.length (snd pages)
    pageIndex = clamp 0 (pageCount - 1) st.pageIndex
  H.modify _
    { records = Array.index (snd pages) pageIndex
    , count = fst pages
    , pageCount = pageCount
    , pageIndex = pageIndex
    }

calcPageCount ∷ Int → Int → Int
calcPageCount count size =
  Int.ceil (Int.toNumber count / Int.toNumber size)

tupleN ∷ Int → J.JCursor
tupleN int = J.JField ("_" <> show int) J.JCursorTop

dimensionsN ∷ Int → Array (J.Json → Maybe J.Json)
dimensionsN 0 = []
dimensionsN n = J.cursorGet ∘ tupleN <$> Array.range 0 (n - 1)

sizeOfRow ∷ Array (Int × Column) → J.Json → Int
sizeOfRow columns row =
  fromMaybe 1
    (F.maximum
      (Array.mapMaybe
        case _ of
          ix × Column { valueAggregation: Just _ } → Just 1
          ix × _ → J.foldJsonArray 1 Array.length <$> J.cursorGet (tupleN ix) row
        columns))

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
