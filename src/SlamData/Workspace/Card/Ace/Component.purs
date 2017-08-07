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

module SlamData.Workspace.Card.Ace.Component
  ( aceComponent
  , DSL
  , HTML
  , module SlamData.Workspace.Card.Ace.Component.Query
  , module SlamData.Workspace.Card.Ace.Component.State
  ) where

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component as AC
import Ace.Types (Editor)

import Control.Monad.Aff.AVar (makeVar, takeVar)

import Data.Array as Array
import Data.String as Str

import Halogen as H
import Halogen.Component.Utils (affEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Prelude
import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType as AccessType
import SlamData.Workspace.Card.Ace.Component.Query (Query(..))
import SlamData.Workspace.Card.Ace.Component.State (State, initialState)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import SqlSquared as Sql

import Utils.Ace (getRangeRecs, readOnly)

type DSL = CC.InnerCardParentDSL State Query AC.AceQuery Unit
type HTML = CC.InnerCardParentHTML Query AC.AceQuery Unit

aceComponent ∷ CT.AceMode → CC.CardOptions → CC.CardComponent
aceComponent mode =
  CC.makeCardComponent (CT.Ace mode) $ H.lifecycleParentComponent
    { render: render mode
    , eval: evalCard mode ⨁ evalComponent
    , initialState: const initialState
    , initializer: Just $ right $ H.action Init
    , finalizer: Nothing
    , receiver: const Nothing
    }

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  Init next → do
    trigger ← H.liftAff $ makeVar
    H.modify _ { trigger = Just trigger }
    H.subscribe $ affEventSource
      (const $ right $ RunQuery H.Listening)
      (takeVar trigger)
    pure next
  RunQuery next → do
    H.raise CC.modelUpdate
    H.modify _ { dirty = false }
    pure next
  HandleAce (AC.TextChanged str) next → do
    unlessM (H.gets _.dirty) do
      H.modify _ { dirty = true }
    pure next

evalCard ∷ CT.AceMode → CC.CardEvalQuery ~> DSL
evalCard mode = case _ of
  CC.Activate next → do
    mbEditor ← H.query unit $ H.request AC.GetEditor
    for_ (join mbEditor) $ H.liftEff ∘ Editor.focus
    pure next
  CC.Deactivate next → do
    st ← H.get
    for_ st.trigger \trigger →
      when st.dirty do
        N.info "Don't forget to run your query to see the latest result."
          Nothing
          Nothing
          (Just $ N.ActionOptions
            { message: ""
            , actionMessage: "Run query now"
            , action: N.Fulfill trigger
            })
    pure next
  CC.Save k → do
    content ← fromMaybe "" <$> H.query unit (H.request AC.GetText)
    mbEditor ← H.query unit (H.request AC.GetEditor)
    rrs ← H.liftEff $ maybe (pure []) getRangeRecs $ join mbEditor
    pure ∘ k
      $ Card.Ace mode { text: content, ranges: rrs }
  CC.Load card next → do
    case card of
      Card.Ace _ { text, ranges } → do
        _ ← H.query unit $ H.action $ AC.SetText text
        mbEditor ← H.query unit $ H.request AC.GetEditor
        H.liftEff $ for_ (join mbEditor) \editor → do
          traverse_ (readOnly editor) ranges
          Editor.navigateFileEnd editor
      _ → pure unit
    H.modify _ { dirty = false }
    pure next
  CC.ReceiveInput _ varMaps next → do
    let vars = VM.varNames varMaps
    _ ← H.query unit $ H.action $ AC.SetCompleteFn \_ _ _ inp → do
      let inp' = Str.toLower inp
      pure $ flip Array.mapMaybe vars \var → do
        guard $ Str.contains (Str.Pattern inp') (Str.toLower var)
        pure
          { value: ":" <> Sql.printIdent var
          , score: 200.0
          , caption: Just var
          , meta: "var"
          }
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions dims reply → do
    mbEditor ← H.query unit $ H.request AC.GetEditor
    for_ (join mbEditor) $ H.liftEff ∘ Editor.resize Nothing
    pure $ reply if dims.width < 240.0 then Low else High

aceSetup ∷ CT.AceMode → Editor → Slam Unit
aceSetup mode editor = do
  H.liftEff $ Editor.setTheme "ace/theme/chrome" editor
  H.liftEff $ Editor.setEnableLiveAutocompletion true editor
  H.liftEff $ Editor.setEnableBasicAutocompletion true editor
  H.liftEff $ Session.setMode (CT.aceMode mode) =<< Editor.getSession editor
  whenM (AccessType.isReadOnly ∘ _.accessType <$> Wiring.expose) do
    H.liftEff $ Editor.setReadOnly true editor
    H.liftEff $ Editor.setHighlightActiveLine false editor
    H.liftEff $ Editor.setStyle "sd-disabled" editor

render ∷ CT.AceMode → State → HTML
render mode state =
  HH.div
    [ HP.classes [ CN.cardInput, CN.aceContainer ] ]
    [ HH.div [ HP.class_ (HH.ClassName "sd-ace-inset-shadow") ] []
    , HH.div
        [ HP.class_ (HH.ClassName "sd-ace-toolbar") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "sd-ace-run")
            , HP.disabled (not state.dirty)
            , HP.title "Run Query"
            , ARIA.label "Run query"
            , HE.onClick (HE.input_ (right ∘ RunQuery))
            ]
            [ I.playerPlay
            , HH.text "Run Query"
            ]
        ]
    , HH.slot unit (AC.aceComponent (aceSetup mode) (Just AC.Live)) unit
        (Just ∘ right ∘ H.action ∘ HandleAce)
    ]
