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

module SlamData.Workspace.Card.Viz.Renderer.Input.Component where

import SlamData.Prelude

import DOM.Classy.Event as DOM
import DOM.Event.Types (Event)
import Data.Argonaut (JCursor(..))
import Data.BrowserFeatures.InputType as IT
import Data.DateTime as DT
import Data.Either.Nested (Either3)
import Data.Formatter.DateTime as FD
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Lens ((^?))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (on, default)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Component.Utils (sendAfter)
import Halogen.Datepicker.Component.Date as DatePicker
import Halogen.Datepicker.Component.DateTime as DateTimePicker
import Halogen.Datepicker.Component.Time as TimePicker
import Halogen.Datepicker.Component.Types as DPT
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port (SetupInputPort)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Viz.Renderer.Input.Model as M
import SlamData.Workspace.PickerUtils as PU
import Text.Markdown.SlamDown.Halogen.Component as TMH

type State =
  { label ∷ Maybe String
  , value ∷ String
  , formInputType ∷ CT.Input ()
  , inputTypeSupported ∷ IT.InputType → Boolean
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { label: Nothing
  , value: ""
  , formInputType: CT.text
  , cursor: JCursorTop
  , inputTypeSupported: TMH.defaultBrowserFeatures.inputTypeSupported
  }

data Query a
  = Setup SetupInputPort a
  | ValueChanged String a
  | Save (M.Model → a)
  | Load M.Model a
  | PreventDefault Event a
  | RaiseUpdated a
  | Init a

data Message = Updated

type ChildQuery = Coproduct3 DatePicker.Query TimePicker.Query DateTimePicker.Query
type Slot = Either3 Unit Unit Unit

cpDatePicker ∷ CP.ChildPath DatePicker.Query ChildQuery Unit Slot
cpDatePicker = CP.cp1

cpTimePicker ∷ CP.ChildPath TimePicker.Query ChildQuery Unit Slot
cpTimePicker = CP.cp2

cpDateTimePicker ∷ CP.ChildPath DateTimePicker.Query ChildQuery Unit Slot
cpDateTimePicker = CP.cp3

type DSL = H.ParentDSL State Query ChildQuery Slot Message Slam
type HTML m = H.ParentHTML Query ChildQuery Slot m

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ ∀ m
  . State
  → HTML m
render state =
  HH.form
    [ HE.onSubmit (HE.input PreventDefault) ]
    $ foldMap (\n → [ HH.h3_  [ HH.text n ] ]) state.label
    ⊕ [ let
           inp =
             HH.input
               [ HP.classes [ CN.formControl ]
               , HP.value state.value
               , HP.type_ $ halInputTypeFromFIT state.formInputType
               , HE.onValueInput $ HE.input ValueChanged
               ]
        in if state.inputTypeSupported $ inputTypeFromFIT state.formInputType
        then inp
        else default inp
          # on CT._datetime
              (const
                 $ HH.slot'
                   cpDateTimePicker
                   unit
                   (DateTimePicker.pickerWithConfig PU.config PU.dateTimePickerFormat)
                   unit
                   $ HE.input
                   $ \(DPT.NotifyChange n) →
                     ValueChanged $ fromMaybe "" $ DPT.value n >>= formatDateTime )
          # on CT._date
              (const
                 $ HH.slot'
                   cpDatePicker
                   unit
                   (DatePicker.pickerWithConfig PU.config PU.datePickerFormat)
                   unit
                   $ HE.input
                   $ \(DPT.NotifyChange n) →
                     ValueChanged $ fromMaybe "" $ DPT.value n >>= formatDate )
          # on CT._time
              (const
                 $ HH.slot'
                   cpTimePicker
                   unit
                   (TimePicker.pickerWithConfig PU.config PU.timePickerFormat)
                   unit
                   $ HE.input
                   $ \(DPT.NotifyChange n) →
                     ValueChanged $ fromMaybe "" $ DPT.value n >>= formatTime )
          $ state.formInputType

      ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    w ← Wiring.expose
    H.modify _{inputTypeSupported = w.browserFeatures.inputTypeSupported}
    pure next
  ValueChanged s next → do
    H.modify _{ value = s }
    H.raise Updated
    pure next
  Setup p next → do
    st ← H.get
    H.modify _
      { label =  p.projection ^? D._staticCategory
      , formInputType = p.formInputType
      }
    for_ (p.projection ^? D._value ∘ D._projection) \cursor → do
      H.modify _
        { cursor = cursor
        , value = if cursor ≠ st.cursor then "" else st.value
        }
      when (st.cursor ≠ cursor) $
        void $ sendAfter (Milliseconds 200.0) RaiseUpdated
    pure next
  Save cont → do
    st ← H.get
    pure
      $ cont
        { value: st.value
        , cursor: st.cursor
        , formInputType: st.formInputType
        }
  Load m next → do
    H.modify _
      { value = m.value
      , formInputType = m.formInputType
      , cursor = m.cursor
      }
    s ← H.get
    unless (s.inputTypeSupported $ inputTypeFromFIT s.formInputType) $ void do
      default ( pure $ pure unit )
        # on CT._datetime
          ( const
            $ H.query' cpDateTimePicker unit
            $ DPT.setValue
            $ Right <$> unformatDateTime s.value )
        # on CT._date
          ( const
            $ H.query' cpDatePicker unit
            $ DPT.setValue
            $ Right <$> unformatDate s.value )
        # on CT._time
          ( const
            $ H.query' cpTimePicker unit
            $ DPT.setValue
            $ Right <$> unformatTime s.value )


        $ s.formInputType
    pure next
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next
  RaiseUpdated next → do
    H.raise Updated
    pure next

halInputTypeFromFIT ∷ CT.Input () → HP.InputType
halInputTypeFromFIT = inputTypeFromFIT ⋙ case _ of
  IT.Number → HP.InputNumber
  IT.Date → HP.InputDate
  IT.DateTimeLocal → HP.InputDatetimeLocal
  IT.Time → HP.InputTime
  _ → HP.InputText

inputTypeFromFIT ∷ CT.Input () → IT.InputType
inputTypeFromFIT = case_
  # on CT._numeric (const $ IT.Number)
  # on CT._date (const $ IT.Date)
  # on CT._time (const $ IT.Time)
  # on CT._datetime (const $ IT.DateTimeLocal)
  # on CT._text (const $ IT.Text)

formatDateTime ∷ DT.DateTime → Maybe String
formatDateTime x = hush $ FD.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" x

formatDate ∷ DT.Date → Maybe String
formatDate x = hush $ FD.formatDateTime "YYYY-MM-DD" $ DT.DateTime x bottom

formatTime ∷ DT.Time → Maybe String
formatTime x = hush $ FD.formatDateTime "HH:mm:ss" $ DT.DateTime bottom x

unformatDateTime ∷ String → Maybe DT.DateTime
unformatDateTime x = hush $ FD.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" x

unformatDate ∷ String → Maybe DT.Date
unformatDate x = hush $ map DT.date $ FD.unformatDateTime "YYYY-MM-DD" x

unformatTime ∷ String → Maybe DT.Time
unformatTime x = hush $ map DT.time $ FD.unformatDateTime "HH:mm:ss" x
