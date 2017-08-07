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
  ) where

import SlamData.Prelude

import Control.Monad.State as CMS
import Data.Array as A
import Data.Lens (view)
import Data.List as L
import Data.Path.Pathy as Path
import Data.String as S
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Quasar.Types (Pagination)
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Bus as GMB
import SlamData.Notification as N
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Open.Component.Query (Query)
import SlamData.Workspace.Card.Open.Component.Query as Q
import SlamData.Workspace.Card.Open.Component.State (State, initialState)
import SlamData.Workspace.Card.Open.Item (AnyItem(..), AnyItem', AnyPath', anyItemToOpen)
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.LevelOfDetails as LOD
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Column.BasicFilter as MCF
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Column.Component.Request as MCR
import SlamData.Workspace.MillerColumns.Component as MC
import Utils.Path (toPathList)

type ColumnsQuery = MC.Query AnyItem' AnyPath' Void
type DSL = CC.InnerCardParentDSL State Query ColumnsQuery Unit
type HTML =  CC.InnerCardParentHTML Query ColumnsQuery Unit

openComponent ∷ CC.CardOptions → CC.CardComponent
openComponent =
  CC.makeCardComponent CT.Open $ H.lifecycleParentComponent
    { render: render
    , eval: evalCard ⨁ evalOpen
    , initialState: const initialState
    , receiver: const Nothing
    , initializer: Just $ right $ H.action Q.Init
    , finalizer: Nothing
    }

render ∷ State → HTML
render _ = HH.slot unit columnsComponent (pathToColumnData Root) handleMessage

handleMessage ∷ MC.Message' AnyItem' AnyPath' Void → Maybe (CC.InnerCardQuery Query Unit)
handleMessage = either handleSelection absurd
  where
  runInput ∷ Maybe Open.Open → Maybe (CC.InnerCardQuery Query Unit)
  runInput sel = Just $ H.action $ right ∘ Q.UpdateSelection sel

  handleSelection ∷ MC.Message AnyItem' AnyPath' → Maybe (CC.InnerCardQuery Query Unit)
  handleSelection = case _ of
    MC.SelectionChanged _ _ sel → runInput (anyItemToOpen =<< sel)
    MC.LoadRequest req → Just $ right $ H.action $ Q.HandleLoadRequest req

renderItem ∷ AnyItem' → MCI.BasicItemHTML
renderItem r =
  HH.div
    [ HP.class_ (HH.ClassName "sd-miller-column-item-inner") ]
    $ join
        [ pure $ HH.span_
            [ itemGlyph r
            , HH.text (itemName r)
            ]
        , guard (not (isLeaf (isRight ∘ R.getPath)) r) $> I.chevronRightSm
        ]

isLeaf ∷ ∀ a. (a → Boolean) → AnyItem a → Boolean
isLeaf f = case _ of
  Root → false
  Variables → false
  Variable _ → true
  Resource r → f r

isActionable ∷ AnyItem' → Boolean
isActionable = isLeaf (isRight ∘ R.getPath) || isResource

isResource ∷ ∀ a. AnyItem a → Boolean
isResource = case _ of
  Resource _ → true
  _ → false


itemName ∷ AnyItem' → String
itemName = case _ of
  Root → ""
  Variables → "Variables"
  Variable v → unwrap v
  Resource r
    | r ≡ R.Directory Path.rootDir → "Filesystem"
    | otherwise → R.resourceName r

itemGlyph ∷ AnyItem' → MCI.BasicItemHTML
itemGlyph = case _ of
  Root → HH.text ""
  Variables → I.tagsSm
  Variable _ → I.tagSm
  Resource r → iconForResource r

iconForResource ∷ R.Resource → MCI.BasicItemHTML
iconForResource = case _ of
  R.File _ → I.file
  R.Workspace _ → I.workspaceSm
  R.Directory _ → I.folderSm
  R.Mount (R.Database _) → I.database
  R.Mount (R.View _) → I.file
  -- TODO: Different icon
  R.Mount (R.Module _) → I.database

evalOpen ∷ Query ~> DSL
evalOpen = case _ of
  Q.Init next → do
    { auth } ← Wiring.expose
    H.subscribe
      $ busEventSource (\msg -> right (Q.HandleSignInMessage msg H.Listening)) auth.signIn
    pure next
  Q.HandleSignInMessage message next → do
    when (message ≡ GMB.SignInSuccess) do
      void $ H.query unit $ H.action $ MC.Reload
    pure next
  Q.UpdateSelection selection next → do
    H.modify _ { selection = selection }
    H.raise CC.modelUpdate
    pure next
  Q.HandleLoadRequest req@(path × _) next → do
    res ← load req
    _ ← H.query unit $ H.action $ MC.FulfilLoadRequest (path × res)
    pure next

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    k ∘ Card.Open <$> H.gets _.selection
  CC.Load (Card.Open res) next → do
    queryPopulate res
    H.modify _ { selection = res }
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ varMap next → do
    let vars = VM.variables varMap
    selection ← CMS.state \st -> st.selection × (st { currentVars = vars })
    -- We need to reload the variables column in response to a new varMap,
    -- but only if it might be visible.
    case selection of
      Just (Open.Resource _) → pure unit
      _ → void $ H.query unit $ H.action $ MC.Reload
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 250.0 ∨ dims.height < 50.0
      then LOD.Low
      else LOD.High

queryPopulate ∷ Maybe Open.Open → DSL Unit
queryPopulate = case _ of
  Just (Open.Resource r) → query $ Resource $ R.getPath r
  Just (Open.Variable v) → query $ Variable v
  Nothing → query Root
  where
  query = void ∘ H.query unit ∘ H.action ∘ MC.Populate ∘ pathToColumnData

columnsComponent ∷ MC.MillerColumnsComponent AnyItem' AnyPath' Void
columnsComponent =
  MC.component $ MC.ColumnOptions
    { renderColumn: MCC.component
    , renderItem:
        MCI.component
          { label: itemName
          , render: renderItem
          , isActionable
          }
    , label: itemName
    , isLeaf: isLeaf isRight
    , id: map R.getPath
    }

makePagination ∷ Int → Pagination
makePagination = { offset: _, limit: 250 }

load ∷ AnyPath' × MCR.LoadRequest → DSL (MCR.LoadResponse AnyItem')
load (path × { filter, requestId, offset }) =
  case path of
    Root →
      pure
        { requestId
        , items: L.fromFoldable [ rootDirectory, Variables ]
        , nextOffset: Nothing
        }
    Variables → do
      vars ← H.gets _.currentVars
      let
        filter' = S.toLower filter
        filterFn (VM.Var v) = S.contains (S.Pattern filter') $ S.toLower v
        vars'
          | filter ≡ "" = vars
          | otherwise   = L.filter filterFn vars
      pure
        { requestId
        , items: Variable <$> vars'
        , nextOffset: Nothing
        }
    Resource (Left r) → do
      let pagination = makePagination (fromMaybe 0 offset)
      Quasar.childrenPaginated r (Just pagination) >>= case _ of
        Left err → handleError err $> noResult
        Right rs → do
          let filenameFilter = MCF.mkFilter filter
          pure
            { requestId
            , items: L.fromFoldable (Resource <$> A.filter (filenameFilter ∘ view R._name) rs)
            , nextOffset: if A.length rs < pagination.limit then Nothing else Just $ pagination.offset + pagination.limit
            }
    Resource _ → pure noResult
    Variable v → pure noResult
  where
  noResult ∷ MCR.LoadResponse AnyItem'
  noResult = { requestId, items: L.Nil, nextOffset: Nothing }

handleError ∷ QE.QError → DSL Unit
handleError err =
  case GE.fromQError err of
    Left msg →
      N.error
        "There was a problem fetching the directory listing"
        (Just $ N.Details msg)
        Nothing
        Nothing
    Right ge →
      GE.raiseGlobalError ge

-- | For a filesystem path, construct a list of all of the sub-paths up to the
-- | current point. This is required as column-view paths (in contrast to
-- | filesystem paths) are required to be in an unfolded form. It's cheating a
-- | little bit, as some of the intermediate steps may be mounts, etc. but we
-- | don't know that so always produce directories, but for the usage here that
-- | doesn't matter.
pathToColumnData ∷ AnyPath' → AnyPath' × L.List AnyItem'
pathToColumnData = case _ of
  Root → Root × L.Nil
  Variables → Root × pure Variables
  Variable v → Root × L.fromFoldable [ Variable v, Variables ]
  Resource r → Root × map (Resource ∘ either R.Directory R.File) (toPathList r)

rootDirectory ∷ AnyItem'
rootDirectory = Resource (R.Directory Path.rootDir)
