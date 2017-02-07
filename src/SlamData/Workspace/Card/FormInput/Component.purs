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

module SlamData.Workspace.Card.FormInput.Component (formInputComponent) where

import SlamData.Prelude

import Data.Int (floor)
import Data.Lens ((^?))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Chart.MetricRenderer.Component as Metric
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State (_AutoSelect)
import SlamData.Workspace.Card.FormInput.Component.ChildSlot as CS
import SlamData.Workspace.Card.FormInput.Component.State as ST
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Component as Labeled
import SlamData.Workspace.Card.FormInput.Model as M
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Component as TextLike
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type HTML =
  H.ParentHTML CS.ChildState CC.CardEvalQuery CS.ChildQuery Slam CS.ChildSlot
type DSL =
  H.ParentDSL ST.State CS.ChildState CC.CardEvalQuery CS.ChildQuery Slam CS.ChildSlot

formInputComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
formInputComponent options = CC.makeCardComponent
  { options
  , cardType: CT.FormInput
  , component: H.parentComponent
      { render
      , eval
      , peek: Just (peek ∘ H.runChildF)
      }
  , initialState: H.parentState ST.initialState
  , _State: CC._FormInputState
  , _Query: CC.makeQueryPrism CC._FormInputQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg CT.FormInput) id state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        (guard (state.levelOfDetails ≠ High) $> B.hidden )
    ]
    $ flip foldMap state.formInputType \fit →
        if FIT.isLabeled fit
          then labeled
          else if FIT.isTextLike fit
                 then textLike
                 else metric
  where
  textLike =
    [ HH.slot' CS.cpTextLike unit \_ →
       { component: TextLike.comp
       , initialState: TextLike.initialState
       }
    ]
  labeled =
    [ HH.slot' CS.cpLabeled unit \_ →
       { component: Labeled.comp
       , initialState: Labeled.initialState
       }
    ]
  metric =
    [ HH.slot' CS.cpMetric unit \_ →
       { component: Metric.comp
       , initialState: Metric.initialState
       }
    ]

eval ∷ CC.CardEvalQuery ~> DSL
eval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    mbTextLike ← H.query' CS.cpTextLike unit $ H.request TextLike.Save
    mbLabeled ← H.query' CS.cpLabeled unit $ H.request Labeled.Save
    let
      mbModel =
        Card.FormInput
          $ fromMaybe M.Static
          $ map M.TextLike mbTextLike
          <|> map M.Labeled mbLabeled
    pure $ k mbModel
  CC.Load model next → do
    case model of
      Card.FormInput (M.TextLike r) → do
        H.modify _{ formInputType = Just r.formInputType }
        H.query' CS.cpTextLike unit $ H.action $ TextLike.Load r
        pure next
      Card.FormInput (M.Labeled r) → do
        H.modify _{ formInputType = Just r.formInputType }
        H.query' CS.cpLabeled unit $ H.action $ Labeled.Load r
        pure next
      Card.FormInput M.Static → do
        H.modify _{ formInputType = Just FIT.Static }
        pure next
      _ →
        pure next
  CC.ReceiveInput input _ next → do
    case input of
      SetupTextLikeFormInput p → do
        H.modify _{ formInputType = Just p.formInputType }
        H.query' CS.cpTextLike unit $ H.action $ TextLike.Setup p
        pure next
      SetupLabeledFormInput p → do
        H.modify _{ formInputType = Just p.formInputType }
        H.query' CS.cpLabeled unit $ H.action $ Labeled.Setup p
        pure next
      CategoricalMetric metric → do
        H.modify _{ formInputType = Just FIT.Static }
        H.query' CS.cpMetric unit $ H.action $ Metric.SetMetric metric
        mbLod ← H.query' CS.cpMetric unit $ H.request Metric.GetLOD
        for mbLod \lod →
          H.modify _{ levelOfDetails = lod }
        pure next
      _ →
        pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _AutoSelect)
      $ H.query' CS.cpLabeled unit ∘ H.action ∘ Labeled.SetSelected
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify _{levelOfDetails = if dims.width < 240.0 then Low else High}
    let
      heightPadding = 60
      widthPadding = 6
      intWidth = floor dims.width - widthPadding
      intHeight = floor dims.height
    H.query' CS.cpMetric unit $ H.action $ Metric.SetDimensions {width: intWidth, height: intHeight}
    mbLod ← H.query' CS.cpMetric unit $ H.request Metric.GetLOD
    H.modify case mbLod of
      Nothing →
        _{ levelOfDetails = if dims.width < 240.0 then Low else High }
      Just lod →
        _{ levelOfDetails = lod }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = textLikePeek ⨁ labeledPeek ⨁ const (pure unit)
  where
  textLikePeek = case _ of
    TextLike.Updated _ →
      CC.raiseUpdatedP CC.EvalModelUpdate
    _ → pure unit
  labeledPeek = case _ of
    Labeled.Updated _ →
      CC.raiseUpdatedP CC.EvalModelUpdate
    _ → pure unit
