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

module SlamData.Workspace.Card.Table.Component.Render (HTML, render) where

import SlamData.Prelude

import Data.Argonaut.JCursor (runJsonPrim)
import Data.Array as A
import Data.Char (fromCharCode)
import Data.Int as Int
import Data.Json.JTable as JT
import Data.String (singleton)

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.CSS.New as CSS
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Table.Component.Query (PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.State (State, currentPageInfo)

import Utils (showPrettyNumber)

type HTML = CC.InnerCardHTML Query

-- | A value that holds all possible states for an inputtable value: the current
-- | actual value, and a possible pending user-entered or selected value.
type InputValue a =
  { current ∷ a
  , pending ∷ Maybe (Either String a)
  }

-- | Converts an `InputValue` into a string for use as a HTML form field value.
fromInputValue ∷ ∀ a. Show a ⇒ InputValue a → String
fromInputValue { current, pending } =
  case pending of
    Nothing → show current
    Just pending' → either id show pending'

render ∷ State → HTML
render st =
  HH.div_ $ case st.result of
    Nothing → renderEmpty
    Just result → renderResult result
  where
  renderEmpty =
    A.singleton
    $ HH.div
      [ HP.classes [ B.alert, B.alertWarning ] ]
      [ HH.text "Selected resource is empty" ]
  renderResult result =
    let
      p = currentPageInfo st
    in
     [ HH.div
       [ HP.classes [ HH.ClassName "sd-card-table-content" ] ]
       [ right <$> JT.renderJTable jTableOpts result.json ]
     , HH.div
       [ HP.classes [CSS.pagination, CSS.form] ]
       [ prevButtons (p.page <= 1)
       , pageField { current: p.page, pending: st.page } p.totalPages
       , nextButtons (p.page >= p.totalPages)
       , pageSizeControls st.isEnteringPageSize { current: p.pageSize, pending: st.pageSize }
       ]
     ]
jTableOpts ∷ JT.JTableOpts
jTableOpts = JT.jTableOptsDefault
  { style = JT.noStyle renderJson
  , columnOrdering = JT.alphaOrdering
  }
  where
  renderJson j =
    runJsonPrim j (const "") show showPrettyNumber id

prevButtons ∷ Boolean → HTML
prevButtons enabled =
  HH.div
    [ HP.class_ CSS.formButtonGroup ]
    [ HH.button
        [ HP.class_ CSS.formButton
        , HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage First)
        ]
        [ I.playerRewind ]
    , HH.button
        [ HP.class_ CSS.formButton
        , HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Prev)
        ]
        [ I.playerPrevious ]
    ]

pageField ∷ InputValue Int → Int → HTML
pageField pageValue totalPages =
  HH.div_
    [ submittable
        [ HH.text "Page"
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.value (fromInputValue pageValue)
            , HE.onValueInput (HE.input (map right ∘ SetCustomPage))
            ]
        , HH.text $ "of " <> show totalPages
        ]
    ]

submittable ∷ Array HTML → HTML
submittable =
  HH.form
    [ HE.onSubmit (HE.input (map right ∘ PreventDefault))
    ]

nextButtons ∷ Boolean → HTML
nextButtons enabled =
  HH.div
    [ HP.class_ CSS.formButtonGroup ]
    [ HH.button
        [ HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Next)
        ]
        [ I.playerNext ]
    , HH.button
        [ HP.disabled enabled
        , HE.onClick $ HE.input_ (right ∘ StepPage Last)
        ]
        [ I.playerFastForward ]
    ]

pageSizeControls ∷ Boolean → InputValue Int → HTML
pageSizeControls showCustom pageSize =
  HH.div_
    [ submittable
         $ [ HH.text "Per page:" ]
         ⊕ [ if showCustom
             then HH.input
                [ HP.type_ HP.InputNumber
                , HP.value (fromInputValue pageSize)
                , HE.onValueInput (HE.input (map right ∘ SetCustomPageSize))
                ]
             else HH.select
                [ HE.onValueChange
                    (HE.input \v → map right $
                      if v ≡ "Custom"
                      then StartEnterCustomPageSize
                      else ChangePageSize v)
                ]
                pageOptions
           ]
    ]
  where

  sizeNum = fromMaybe 10 $ Int.fromString (fromInputValue pageSize)

  sizeValues = [10, 25, 50, 100]

  pageOptions = join
    [ option <$> sizeValues
    , dividerOption
    , customOption
    , [ HH.option_ [ HH.text "Custom" ] ]
    ]

  option value =
    HH.option
      [ HP.selected (value ≡ sizeNum) ]
      [ HH.text (show value) ]

  -- An unselectable option dividing the custom values from the presets
  dividerOption =
    [ HH.option
        [ HP.disabled true ]
        [ HH.text $ singleton $ fromCharCode 8212 ]
    ]

  -- If a custom value has been entered, create an entry for it in the dropdown
  customOption =
    if isNothing (A.elemIndex sizeNum sizeValues)
    then [ HH.option [HP.selected true] [HH.text (show sizeNum)] ]
    else []
