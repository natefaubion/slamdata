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
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CT
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

toAction ∷ NextAction → Action.Action NextAction
toAction na =
  Action.mkDo
    { name: CT.name cardType
    , icon: Just $ CT.icon cardType
    , highlighted: isInsert na
    , disabled: false
    , description: description na
    , action: na
    }
  where
  cardType = pluckCardType na

description ∷ NextAction → String
description = case _ of
  Insert cty → "Insert a " ⊕ CT.name cty ⊕ " card"
  FindOutHowToInsert cty → "Find out how to insert a " ⊕ CT.name cty ⊕ " card"

insert ∷ InsertableCardType → Array (Action.Action NextAction)
insert = case _ of
  iCardType → foldMap (A.singleton ∘ toAction ∘ Insert) $ ICT.toCardType iCardType

findOutHowToInsert ∷ InsertableCardType → Array (Action.Action NextAction)
findOutHowToInsert = case _ of
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
