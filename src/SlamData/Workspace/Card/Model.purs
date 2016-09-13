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

module SlamData.Workspace.Card.Model
  ( Model
  , AnyCardModel(..)
  , encode
  , decode
  , modelToEval
  , cardModelOfType
  , modelCardType
  ) where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J

import SlamData.FileSystem.Resource as R

import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.PivotTable.Model as PT
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.ChartOptions.Model as ChartOptions
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | ChartOptions ChartOptions.Model
  | Chart
  | Markdown MD.Model
  | Table JT.Model
  | PivotTable PT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open (Maybe R.Resource)
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | ErrorCard
  | NextAction
  | PendingCard

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary = do
    Gen.oneOf (pure ErrorCard)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , ChartOptions <$> ChartOptions.genModel
      , pure Chart
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , PivotTable <$> PT.genModel
      , pure Download
      , Variables <$> Variables.genModel
      , pure Troubleshoot
      , Cache <$> SC.arbitrary
      , Open <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , pure ErrorCard
      , pure NextAction
      ]

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq =
    case _, _ of
      Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
      Search s1, Search s2 → s1 ≡ s2
      ChartOptions x1, ChartOptions x2 → ChartOptions.eqModel x1 x2
      Chart, Chart → true
      Markdown x, Markdown y → MD.eqModel x y
      Table x, Table y → JT.eqModel x y
      PivotTable x, PivotTable y → PT.eqModel x y
      Download, Download → true
      Variables x, Variables y → Variables.eqModel x y
      Troubleshoot, Troubleshoot → true
      Cache x, Cache y → x ≡ y
      Open x, Open y → x ≡ y
      DownloadOptions x, DownloadOptions y → DLO.eqState x y
      Draftboard x, Draftboard y → DB.eqModel x y
      ErrorCard, ErrorCard → true
      NextAction, NextAction → true
      _,_ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encodeCardModel

modelCardType
  ∷ AnyCardModel
  → CT.CardType
modelCardType =
  case _ of
    Ace mode _ → CT.Ace mode
    Search _ → CT.Search
    ChartOptions _ → CT.ChartOptions
    Chart → CT.Chart
    Markdown _ → CT.Markdown
    Table _ → CT.Table
    PivotTable _ → CT.PivotTable
    Download → CT.Download
    Variables _ → CT.Variables
    Troubleshoot → CT.Troubleshoot
    Cache _ → CT.Cache
    Open _ → CT.Open
    DownloadOptions _ → CT.DownloadOptions
    Draftboard _ → CT.Draftboard
    ErrorCard → CT.ErrorCard
    NextAction → CT.NextAction
    PendingCard → CT.PendingCard

type Model =
  { cardId ∷ CID.CardId
  , model ∷ AnyCardModel
  }

encode
  ∷ Model
  → J.Json
encode card =
  "cardId" := card.cardId
    ~> "cardType" := modelCardType card.model
    ~> "model" := encodeCardModel card.model
    ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    cardId ← obj .? "cardId"
    cardType ← obj .? "cardType"
    model ← decodeCardModel cardType =<< obj .? "model"
    pure { cardId, model }

encodeCardModel
  ∷ AnyCardModel
  → J.Json
encodeCardModel =
  case _ of
    Ace mode model → Ace.encode model
    Search txt → J.encodeJson txt
    ChartOptions model → ChartOptions.encode model
    Chart → J.jsonEmptyObject
    Markdown model → MD.encode model
    Table model → JT.encode model
    PivotTable model → PT.encode model
    Download → J.jsonEmptyObject
    Variables model → Variables.encode model
    Troubleshoot → J.jsonEmptyObject
    Cache model → J.encodeJson model
    Open mres → J.encodeJson mres
    DownloadOptions model → DLO.encode model
    Draftboard model → DB.encode model
    ErrorCard → J.jsonEmptyObject
    NextAction → J.jsonEmptyObject
    PendingCard → J.jsonEmptyObject

decodeCardModel
  ∷ CT.CardType
  → J.Json
  → Either String AnyCardModel
decodeCardModel ty =
  case ty of
    CT.Ace mode → map (Ace mode) ∘ Ace.decode
    CT.Search → map Search ∘ J.decodeJson
    CT.ChartOptions → map ChartOptions ∘ ChartOptions.decode
    CT.Chart → const $ pure Chart
    CT.Markdown → map Markdown ∘ MD.decode
    CT.Table → map Table ∘ JT.decode
    CT.PivotTable → map PivotTable ∘ PT.decode
    CT.Download → const $ pure Download
    CT.Variables → map Variables ∘ Variables.decode
    CT.Troubleshoot → const $ pure Troubleshoot
    CT.Cache → map Cache ∘ J.decodeJson
    CT.Open → map Open ∘ J.decodeJson
    CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
    CT.Draftboard → map Draftboard ∘ DB.decode
    CT.ErrorCard → const $ pure ErrorCard
    CT.NextAction → const $ pure NextAction
    CT.PendingCard → const $ pure PendingCard


cardModelOfType
  ∷ CT.CardType
  → AnyCardModel
cardModelOfType =
  case _ of
    CT.Ace mode → Ace mode Ace.emptyModel
    CT.Search → Search ""
    CT.ChartOptions → ChartOptions ChartOptions.initialModel
    CT.Chart → Chart
    CT.Markdown → Markdown MD.emptyModel
    CT.Table → Table JT.emptyModel
    CT.PivotTable → PivotTable PT.emptyModel
    CT.Download → Download
    CT.Variables → Variables Variables.emptyModel
    CT.Troubleshoot → Troubleshoot
    CT.Cache → Cache Nothing
    CT.Open → Open Nothing
    CT.DownloadOptions → DownloadOptions DLO.initialState
    CT.Draftboard → Draftboard DB.emptyModel
    CT.ErrorCard → ErrorCard
    CT.NextAction → NextAction
    CT.PendingCard → PendingCard

modelToEval
  ∷ AnyCardModel
  → Either String Eval.Eval
modelToEval =
  case _ of
    Ace CT.SQLMode model → pure $ Eval.Query $ fromMaybe "" $ _.text <$> model
    Ace CT.MarkdownMode model → pure $ Eval.Markdown $ fromMaybe "" $ _.text <$> model
    Markdown model → pure $ Eval.MarkdownForm model
    Search txt → pure $ Eval.Search txt
    Cache fp → pure $ Eval.Cache fp
    Open (Just res) → pure $ Eval.Open res
    Open _ → Left $ "Open model missing resource"
    Variables model → pure $ Eval.Variables model
    ChartOptions model → pure $ Eval.ChartOptions model
    DownloadOptions model → pure $ Eval.DownloadOptions model
    Draftboard _ → pure Eval.Draftboard
    _ → pure Eval.Pass
