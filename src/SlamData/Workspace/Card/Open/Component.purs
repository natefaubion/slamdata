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

module SlamData.Workspace.Card.Open.Component
  ( openComponent
  , module SlamData.Workspace.Card.Open.Component.Query
  , module SlamData.Workspace.Card.Open.Component.State
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Lens as Lens
import Data.Lens ((?~), (.~), (%~))
import Data.Path.Pathy (printPath, peel)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Open.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.Open.Component.State (State, initialState, _selected, _browsing, _items, _levelOfDetails)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

openComponent ∷ Maybe R.Resource → H.Component CC.CardStateP CC.CardQueryP Slam
openComponent mres =
  CC.makeCardComponent
    { cardType: CT.Open
    , component: H.lifecycleComponent
        { render
        , eval
        , initializer: Just (H.action (right ∘ Init mres))
        , finalizer: Nothing
        }
    , initialState: initialState { selected = Lens.preview Lens._Right ∘ R.getPath =<< mres  }
    , _State: CC._OpenState
    , _Query: CC.makeQueryPrism CC._OpenQuery
    }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD B.glyphiconFolderOpen left state.levelOfDetails
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
         $ [ HH.className "card-input-maximum-lod" ]
         ⊕ (B.hidden <$ guard (state.levelOfDetails ≠ High))
    ]
    [ HH.div [ HP.classes [ RC.openCardMenu ] ]
      [ HH.button
          ([ HP.class_ RC.formButton
           ] ⊕ case parentDir of
                Nothing →
                  [ HP.disabled true ]
                Just r →
                  [ HE.onClick (HE.input_ (right ∘ (ResourceSelected r)))
                  , HP.title "Up a directory"
                  , ARIA.label "Up a directory"
                  ]
          )
          [ glyphForDeselectOrUp ]
      , HH.p
          [ ARIA.label $ "Selected resource: " ⊕ selectedLabel ]
          [ HH.text selectedLabel ]
      ]
    , HH.ul_ $ map renderItem state.items
    ]

  where
  glyphForDeselectOrUp ∷ HTML
  glyphForDeselectOrUp | isJust state.selected = glyph B.glyphiconRemove
  glyphForDeselectOrUp | otherwise = glyph B.glyphiconChevronUp

  selectedLabel ∷ String
  selectedLabel =
    fromMaybe ""
    $ map printPath state.selected
    <|> (pure $ printPath state.browsing)

  parentDir ∷ Maybe R.Resource
  parentDir =
    R.Directory
    <$> ((state.selected $> state.browsing)
         <|> (fst <$> peel state.browsing))

  renderItem ∷ R.Resource → HTML
  renderItem r =
    HH.li
      [ HP.classes
          $ ((guard (Just (R.getPath r) ≡ (Right <$> state.selected))) $> B.active)
          ⊕ ((guard (R.hiddenTopLevel r)) $> RC.itemHidden)
      , HE.onClick (HE.input_ (right ∘ ResourceSelected r))
      , ARIA.label labelTitle

      ]
      [ HH.a
          [ HP.title labelTitle ]
          [ glyphForResource r
          , HH.text $ R.resourceName r
          ]
      ]
    where
    labelTitle ∷ String
    labelTitle =
      "Select " ⊕ R.resourcePath r

glyphForResource ∷ R.Resource → HTML
glyphForResource = case _ of
  R.File _ → glyph B.glyphiconFile
  R.Workspace _ → glyph B.glyphiconBook
  R.Directory _ → glyph B.glyphiconFolderOpen
  R.Mount (R.Database _) → glyph B.glyphiconHdd
  R.Mount (R.View _) → glyph B.glyphiconFile

eval ∷ QueryP ~> DSL
eval = cardEval ⨁ openEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next →
    pure next
  CC.Activate next →
    pure next
  CC.Save k → do
    mbRes ← H.gets _.selected
    k ∘ Card.Open <$>
      case mbRes of
        Just res → pure ∘ Just $ R.File res
        Nothing → do
          br ← H.gets _.browsing
          pure ∘ Just $ R.Directory br
  CC.Load card next → do
    case card of
      Card.Open (Just (res @ R.File _)) → resourceSelected res
      _ → pure unit
    pure next
  CC.SetDimensions dims next → do
    H.modify
      $ (_levelOfDetails
         .~ if dims.width < 288.0 ∨ dims.height < 240.0
              then Low
              else High)
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

openEval ∷ Query ~> DSL
openEval (ResourceSelected r next) = do
  resourceSelected r
  CC.raiseUpdatedC' CC.EvalModelUpdate
  pure next
openEval (Init mres next) = do
  updateItems *> rearrangeItems
  traverse_ resourceSelected mres
  pure next

resourceSelected ∷ R.Resource → DSL Unit
resourceSelected r = do
  case R.getPath r of
    Right fp → do
      for_ (fst <$> peel fp) \dp → do
        oldBrowsing ← H.gets _.browsing
        unless (oldBrowsing ≡ dp) do
          H.modify (_browsing .~ dp)
          updateItems
      H.modify (_selected ?~ fp)
    Left dp → do
      H.modify
        $ (_browsing .~ dp)
        ∘ (_selected .~ Nothing)
      updateItems
  rearrangeItems

updateItems ∷ DSL Unit
updateItems = do
  cs ← Quasar.children =<< H.gets _.browsing
  mbSel ← H.gets _.selected
  H.modify (_items .~ foldMap id cs)

rearrangeItems ∷ DSL Unit
rearrangeItems = do
  H.modify $ _items %~ Arr.sortBy sortFn
  where
  sortFn ∷ R.Resource → R.Resource → Ordering
  sortFn a b
    | R.hiddenTopLevel a && R.hiddenTopLevel b = compare a b
    | R.hiddenTopLevel a = GT
    | R.hiddenTopLevel b = LT
    | otherwise = compare a b
