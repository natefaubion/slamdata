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

import Data.List as L
import Data.List ((:))
import Data.Lens ((?~), (.~), (%~))
import Data.Path.Pathy as Path
import Data.Unfoldable (unfoldr)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Open.Component.Query (QueryP)
import SlamData.Workspace.Card.Open.Component.State (State, StateP, _levelOfDetails, _selected, initialState)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.MillerColumns.Component as MC

import Utils.Path (AnyPath)

type DSL = H.ParentDSL State (MC.State R.Resource AnyPath) CC.CardEvalQuery (MC.Query AnyPath) Slam Unit
type HTML = H.ParentHTML (MC.State R.Resource AnyPath) CC.CardEvalQuery (MC.Query AnyPath) Slam Unit

openComponent ∷ Maybe R.Resource → H.Component CC.CardStateP CC.CardQueryP Slam
openComponent mres =
  let
    initSelection = toPathList ∘ R.getPath <$> mres
    initPath = fromMaybe (pure (Left Path.rootDir)) initSelection
  in
    CC.makeCardComponent
      { cardType: CT.Open
      , component: H.parentComponent
          { render: render initPath
          , eval
          , peek: Just (peek ∘ H.runChildF)
          }
      , initialState: H.parentState initialState { selected = initSelection }
      , _State: CC._OpenState
      , _Query: CC.makeQueryPrism CC._OpenQuery
      }

render ∷ L.List AnyPath → State → HTML
render initPath state =
  HH.div_
    [ renderHighLOD initPath state
    , renderLowLOD (CT.lightCardGlyph CT.Open) id state.levelOfDetails
    ]

renderHighLOD ∷ L.List AnyPath → State → HTML
renderHighLOD initPath state =
  HH.slot unit \_ →
    { component: MC.component itemSpec (Just initPath)
    , initialState: MC.initialState
    }

renderItem ∷ R.Resource -> MC.ItemHTML
renderItem r =
  HH.div
    [ HP.classes
        [ HH.className "sd-open-item"
        , either
            (const $ HH.className "sd-open-item-node")
            (const $ HH.className "sd-open-item-leaf")
            (R.getPath r)
        ]
    ]
    [ glyphForResource r
    , HH.span_ [ HH.text (R.resourceName r) ]
    ]

glyphForResource ∷ R.Resource → MC.ItemHTML
glyphForResource = case _ of
  R.File _ → glyph B.glyphiconFile
  R.Workspace _ → glyph B.glyphiconBook
  R.Directory _ → glyph B.glyphiconFolderOpen
  R.Mount (R.Database _) → glyph B.glyphiconHdd
  R.Mount (R.View _) → glyph B.glyphiconFile

eval ∷ CC.CardEvalQuery ~> DSL
eval = case _ of
  CC.EvalCard info output next →
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    mbRes ← H.gets _.selected
    pure $ k $ Card.Open (map (either R.Directory R.File) ∘ L.head =<< mbRes)
  CC.Load (Card.Open (Just res)) next → do
    void $ H.query unit $ H.action $ MC.Populate $ toPathList $ R.getPath res
    pure next
  CC.Load _ next →
    pure next
  CC.SetDimensions dims next → do
    H.modify $
      _levelOfDetails .~
        if dims.width < 250.0 ∨ dims.height < 50.0
        then Low
        else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ x. MC.Query AnyPath x → DSL Unit
peek = case _ of
  MC.Populate rs _ → do
    H.modify (_selected ?~ rs)
    CC.raiseUpdatedP CC.EvalModelUpdate
  _ → pure unit

itemSpec ∷ MC.ItemSpec R.Resource AnyPath Slam
itemSpec =
  { label: R.resourceName
  , render: renderItem
  , load
  , id: R.getPath
  }

load ∷ L.List AnyPath → Slam (Maybe (L.List R.Resource))
load ps =
  case L.head ps of
    Just (Left p) → do
      qe ← Quasar.children p
      case qe of
        Left err → handleError err $> Nothing
        Right rs → pure $ Just $ L.fromFoldable rs
    _ → pure Nothing

handleError ∷ QE.QError → Slam Unit
handleError err =
  case GE.fromQError err of
    Left msg →
      N.error
        "There was a problem fetching the directory listing"
        (Just msg)
        Nothing
    Right ge →
      GE.raiseGlobalError ge

-- | For a filesystem path, construct a list of all of the sub-paths up to the
-- | current point. This is required as column-view paths (in contrast to
-- | filesystem paths) are required to be in an unfolded form.
toPathList ∷ AnyPath → L.List AnyPath
toPathList res =
  (unfoldr \r → Tuple r <$> either go go r) res `L.snoc` Left Path.rootDir
  where
  go ∷ ∀ b. Path.Path Path.Abs b Path.Sandboxed -> Maybe AnyPath
  go = map (Left ∘ fst) ∘ Path.peel
