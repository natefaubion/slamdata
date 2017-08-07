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

module SlamData.Workspace.Card.Next.NextAction where

import SlamData.Prelude

import Data.Array as A

import SlamData.ActionList.Action as Action
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.CardType.FormInputType as FiT
import SlamData.Workspace.Card.CardType.GeoChartType (GeoChartType)
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Port as Port

data NextAction
  = Insert CardType
  | FindOutHowToInsert CardType

derive instance eqNextAction ∷ Eq NextAction

isInsert ∷ NextAction → Boolean
isInsert (Insert _) = true
isInsert _ = false

pluckCardType ∷ NextAction → CardType
pluckCardType (Insert ct) = ct
pluckCardType (FindOutHowToInsert ct) = ct

geoChartSubmenu
  ∷ (GeoChartType → Action.Action NextAction)
  → Action.Action NextAction
geoChartSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Geo Chart"
    , icon: Just $ I.IconHTML I.cardsSetupGeoChart
    , description: "Select Setup Geo Chart card category"
    , children: map mkAction GcT.all
    }

insertGeoChartSubmenu ∷ Action.Action NextAction
insertGeoChartSubmenu =
  geoChartSubmenu $ toAction ∘ Insert ∘ CT.SetupGeoChart

findOutHowToGeoChartSubmenu ∷ Action.Action NextAction
findOutHowToGeoChartSubmenu =
  geoChartSubmenu $ toAction ∘ FindOutHowToInsert ∘ CT.SetupGeoChart

chartSubmenu
  ∷ (ChartType → Action.Action NextAction)
  → Action.Action NextAction
chartSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Chart"
    , icon: Just $ I.IconHTML I.cardsSetupChart
    , description: "Select Setup Chart card category"
    , children: map mkAction ChT.all
    }

insertChartSubmenu ∷ Action.Action NextAction
insertChartSubmenu =
  chartSubmenu $ toAction ∘ Insert ∘ CT.ChartOptions

findOutHowToChartSubmenu ∷ Action.Action NextAction
findOutHowToChartSubmenu =
  chartSubmenu $ toAction ∘ FindOutHowToInsert ∘ CT.ChartOptions

formInputSubmenu
  ∷ (FormInputType → Action.Action NextAction)
  → Action.Action NextAction
formInputSubmenu mkAction =
  Action.mkDrill
    { name: "Setup Form"
    , icon: Just $ I.IconHTML I.cardsSetupFormInput
    , description: "Select Setup Form card category"
    , children: map mkAction FiT.all
    }

insertFormInputSubmenu ∷ Action.Action NextAction
insertFormInputSubmenu =
  formInputSubmenu $ toAction ∘ Insert ∘ CT.SetupFormInput

findOutHowToFormInputSubmenu ∷ Action.Action NextAction
findOutHowToFormInputSubmenu =
  formInputSubmenu $ toAction ∘ FindOutHowToInsert ∘ CT.SetupFormInput

toAction ∷ NextAction → Action.Action NextAction
toAction na =
  Action.mkDo
    { name: CT.cardName cardType
    , icon: Just $ CT.cardIcon cardType
    , highlighted: isInsert na
    , disabled: false
    , description: description na
    , action: na
    }
  where
  cardType = pluckCardType na

description ∷ NextAction → String
description = case _ of
  Insert cty → "Insert a " ⊕ CT.cardName cty ⊕ " card"
  FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.cardName cty ⊕ " card"

insert ∷ InsertableCardType → Array (Action.Action NextAction)
insert = case _ of
  ICT.SetupChartCard → A.singleton insertChartSubmenu
  ICT.SetupFormCard → A.singleton insertFormInputSubmenu
  ICT.SetupGeoChartCard → A.singleton insertGeoChartSubmenu
  iCardType → foldMap (A.singleton ∘ toAction ∘ Insert) $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → Array (Action.Action NextAction)
findOutHowToInsert = case _ of
  ICT.SetupChartCard → A.singleton findOutHowToChartSubmenu
  ICT.SetupFormCard → A.singleton findOutHowToFormInputSubmenu
  ICT.SetupGeoChartCard → A.singleton findOutHowToGeoChartSubmenu
  iCardType → foldMap (A.singleton ∘ toAction ∘ FindOutHowToInsert) $ ICT.toCardType iCardType

fromInsertableCard
  ∷ InsertableCardType
  → Array InsertableCardType
  → Array (Action.Action NextAction)
fromInsertableCard icard allCards = case A.elemIndex icard allCards of
  Nothing → findOutHowToInsert icard
  Just _ → insert icard

fromOut ∷ Port.Out → Array (Action.Action NextAction)
fromOut out = do
  sample ← ICT.all
  fromInsertableCard sample cardsTakingInputFromPort
  where
  ioCard = ICT.fromOut out
  cardsTakingInputFromPort = ICT.cardsThatTakeInput ioCard
