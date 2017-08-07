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

module SlamData.Workspace.Card.Next.Component
 ( nextCardComponent
 , Message(..)
 , module SlamData.Workspace.Card.Next.Component.Query
 , module SlamData.Workspace.Card.Next.Component.State
 , module NA
 ) where

import SlamData.Prelude

import CSS as CSS
import Data.Argonaut as J
import Data.Lens ((.~))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import SlamData.ActionList.Component as ActionList
import SlamData.ActionList.Filter.Component as ActionFilter
import SlamData.Hint as Hint
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Next.Component.ChildSlot as CS
import SlamData.Workspace.Card.Next.Component.Query (Query(..))
import SlamData.Workspace.Card.Next.Component.State (State, initialState)
import SlamData.Workspace.Card.Next.Component.State as State
import SlamData.Workspace.Card.Next.NextAction as NA
import SlamData.Workspace.Card.Port as Port

type HTML =
  H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam

type DSL =
  H.ParentDSL State Query CS.ChildQuery CS.ChildSlot Message Slam

data Message
  = AddCard CT.CardType
  | PresentReason Port.Out CT.CardType

nextCardComponent ∷ H.Component HH.HTML Query Port.Out Message Slam
nextCardComponent = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ State → HTML
render state =
  HH.div
    [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
    $ (guard state.presentAddCardHint $>
        Hint.render
          Hint.DownArrow
          (HH.ClassName "sd-add-card-guide")
          (Just DismissAddCardHint)
          (addCardHintText (fst state.input)))
    ⊕ [ HH.slot' CS.cpActionFilter unit
          ActionFilter.component
          "Filter next actions"
          case _ of
            ActionFilter.FilterChanged str →
              Just $ H.action $ HandleFilter str
      , HH.slot' CS.cpActionList unit
          (ActionList.actionListComp ActionList.defaultConf (NA.fromOut state.input))
          unit
          case _ of
            ActionList.Selected a →
              Just $ H.action $ HandleAction a
      ]
  where
  addCardHintText = case _ of
    Port.Initial → "To get this deck started press one of these buttons to add a card."
    _            → "To do more with this deck press one of these buttons to add a card."

updateActions ∷ Port.Out → DSL Unit
updateActions =
  void
    ∘ H.query' CS.cpActionList unit
    ∘ H.action
    ∘ ActionList.UpdateActions
    ∘ NA.fromOut

takesInput ∷ Port.Out → CT.CardType → Boolean
takesInput input =
  ICT.takesInput (ICT.fromOut input) ∘ ICT.fromCardType

possibleToGetTo ∷ Port.Out → CT.CardType → Boolean
possibleToGetTo input =
  ICT.possibleToGetTo (ICT.fromOut input) ∘ ICT.fromCardType

getDismissedAddCardHintBefore ∷ DSL Boolean
getDismissedAddCardHintBefore =
  either (const $ false) id <$> LS.retrieve J.decodeJson LSK.dismissedAddCardHintKey

storeDismissedAddCardHint ∷ DSL Unit
storeDismissedAddCardHint =
  LS.persist J.encodeJson LSK.dismissedAddCardHintKey true

dismissAddCardHint ∷ DSL Unit
dismissAddCardHint =
  H.modify (State._presentAddCardHint .~ false)
    *> storeDismissedAddCardHint

eval ∷ Query ~> DSL
eval = case _ of
  UpdateInput input next → updateActions input $> next
  DismissAddCardHint next → dismissAddCardHint $> next
  PresentAddCardHint next → do
    H.modify
      ∘ (State._presentAddCardHint .~ _)
      ∘ not =<< getDismissedAddCardHintBefore
    pure next
  HandleFilter str next → do
    _ ← H.query' CS.cpActionList unit
      $ H.action
      $ ActionList.UpdateFilter str
    pure next
  HandleAction act next → do
    case act of
      NA.Insert cardType → do
        dismissAddCardHint
        H.raise $ AddCard cardType
      NA.FindOutHowToInsert cardType → do
        input ← H.gets _.input
        H.raise $ PresentReason input cardType
    pure next
  ToActionList query → unsafePartial do
    Just res ← H.query' CS.cpActionList unit query
    pure res
