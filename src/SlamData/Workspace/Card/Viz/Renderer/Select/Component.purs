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

module SlamData.Workspace.Card.Viz.Renderer.Select.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor(..))
import Data.Array as Arr
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (on)
import DOM.Classy.Event as DOM
import DOM.Event.Types (Event)
import Halogen as H
import Halogen.Component.Utils (sendAfter)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.Viz.Renderer.Select.Model as M
import SlamData.Workspace.Card.Port (SetupSelectPort)
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Dimension as D
import Data.Lens ((^?))

type State =
  { formInputType ∷ Sel.Select ()
  , selected ∷ Set.Set Sem.Semantics
  , valueLabelMap ∷ Map.Map Sem.Semantics (Maybe String)
  , label ∷ Maybe String
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { formInputType: Sel.dropdown
  , selected: Set.empty
  , valueLabelMap: Map.empty
  , label: Nothing
  , cursor: JCursorTop
  }

optionList ∷ State → Array (Sem.Semantics × Maybe String)
optionList state =
  Arr.sortBy sortFn $ Map.toUnfoldable state.valueLabelMap
  where
  sortFn (a × al) (b × bl) =
    compare al bl
    ⊕ compare (Sem.printSemantics a) (Sem.printSemantics b)

data Query a
  = Setup SetupSelectPort a
  | ItemSelected Sem.Semantics a
  | SetSelected (Set.Set Sem.Semantics) a
  | Load M.Model a
  | Save (M.Model → a)
  | PreventDefault Event a
  | RaiseUpdated a

data Message = Updated

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
render state =
  HH.form
    [ HE.onSubmit (HE.input PreventDefault) ]
    $ foldMap (\n → [ HH.h3_ [ HH.text n ] ]) state.label
    ⊕ ( case_
        # on Sel._dropdown ( const $ renderDropdown state )
        # on Sel._checkbox ( const $ renderCheckbox state )
        # on Sel._radio ( const $ renderRadio state )
        $ state.formInputType )

renderDropdown ∷ State → Array HTML
renderDropdown state =
  [ HH.select
      [ HP.classes [ CN.formControl ]
      , HE.onSelectedIndexChange \ix →
          H.action ∘ ItemSelected ∘ fst <$> Arr.index options ix
      ]
      $ map renderOption options
  ]
  where
  options = optionList state

  renderOption ∷ Sem.Semantics × Maybe String → HTML
  renderOption (sem × label) =
    HH.option
      [ HP.selected $ Set.member sem state.selected ]
      [ HH.text $ fromMaybe (Sem.printSemantics sem) label ]

renderCheckbox ∷ State → Array HTML
renderCheckbox state =
  [ HH.form
     [ HE.onSubmit (HE.input PreventDefault) ]
     $ map renderOneInput options
  ]
  where
  options = optionList state

  renderOneInput ∷ Sem.Semantics × Maybe String → HTML
  renderOneInput (sem × label) =
    HH.div
      [ HP.classes [ CN.checkbox ] ]
      [ HH.label_
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked $ Set.member sem state.selected
            , HE.onValueChange (HE.input_ $ ItemSelected sem)
            ]
        , HH.text $ fromMaybe (Sem.printSemantics sem) label
        ]
      ]

renderRadio ∷ State → Array HTML
renderRadio state =
  [ HH.form
     [ HE.onSubmit (HE.input PreventDefault) ]
     $ map renderOneInput options
  ]
  where
  options = optionList state

  renderOneInput ∷ Sem.Semantics × Maybe String → HTML
  renderOneInput (sem × label) =
    HH.div
      [ HP.classes [ CN.radio ]  ]
      [ HH.label_
          [ HH.input
            [ HP.type_ HP.InputRadio
            , HP.checked $ Set.member sem state.selected
            , HE.onValueChange (HE.input_ $ ItemSelected sem)
            ]
          , HH.text $ fromMaybe (Sem.printSemantics sem) label
          ]
      ]

eval ∷ Query ~> DSL
eval = case _ of
  Setup conf next → do
    st ← H.get
    let
      selectedFromConf
        -- If this is checkbox and selected values field is empty then
        -- there is no sense in setting default value (and it's actually empty :) )
        | Set.isEmpty conf.selectedValues ∧ (conf.formInputType ≠ Sel.checkbox) =
            foldMap Set.singleton $ List.head $ Map.keys conf.valueLabelMap
        | otherwise =
            conf.selectedValues
      selected
        -- When cursor is changed we use default selection from input
        | Just st.cursor ≠ (conf.projection ^? D._value ∘ D._projection) =
            selectedFromConf
        -- Same if user didn't interact with form input
        | Set.isEmpty st.selected =
            selectedFromConf
        -- If port is the same and user already selected something we preserve selected values
        | otherwise =
            st.selected


    H.modify _
      { formInputType = conf.formInputType
      , selected = selected
      , valueLabelMap = conf.valueLabelMap
      , label = conf.projection ^? D._staticCategory

      }
    for_ (conf.projection ^? D._value ∘ D._projection) \cursor → do
      H.modify _{ cursor = cursor }
      when (st.cursor ≠ cursor)
        $ void $ sendAfter (Milliseconds 200.0) RaiseUpdated
    pure next
  ItemSelected sem next → do
    st ← H.get
    if st.formInputType ≡ Sel.checkbox
      then do
        let
          selected =
            if Set.member sem st.selected
              then Set.delete sem st.selected
              else Set.insert sem st.selected
        H.modify _{ selected = selected }
      else H.modify _{ selected = Set.singleton sem }
    H.raise Updated
    pure next
  SetSelected set next → do
    st ← H.get
    let
      selected
        | st.formInputType ≡ Sel.checkbox = set
        | otherwise = Set.fromFoldable $ List.head $ List.fromFoldable set
    when (selected ≠ st.selected)
      $ H.modify _{ selected = selected }
    pure next
  Load m next → do
    H.modify _
      { formInputType = m.formInputType
      , selected = m.selected
      , cursor = m.cursor
      }
    pure next
  Save continue → do
    st ← H.get
    pure
      $ continue
        { formInputType: st.formInputType
        , selected: st.selected
        , cursor: st.cursor
        }
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next
  RaiseUpdated next → do
    H.raise Updated
    pure next
