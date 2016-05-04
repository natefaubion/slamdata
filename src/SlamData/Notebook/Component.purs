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

module SlamData.Notebook.Component
  ( comp
  , module SlamData.Notebook.Component.State
  , module SlamData.Notebook.Component.Query
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.UI.Browser (locationString)

import Data.Functor.Coproduct.Nested (coproduct3)
import Data.Lens ((^.), (.~), (?~))
import Data.List as L
import Data.StrMap as SM
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import Halogen as H
import Halogen.Component.ChildPath (injSlot)
import Halogen.HTML.Core (ClassName, className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Header.Component as Header
import SlamData.Notebook.AccessType (isReadOnly)
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Component.Query as CQ
import SlamData.Notebook.Card.Port.VarMap as VM
import SlamData.Notebook.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDialog, cpDeck, cpHeader)
import SlamData.Notebook.Component.Query (QueryP, Query(..), fromDraftboard, fromDeck, toDraftboard, toDeck)
import SlamData.Notebook.Component.State (State, _accessType, _browserFeatures,  _loaded, _parentHref, _path, _version, _viewingCard, initialState)
import SlamData.Notebook.Deck.BackSide.Component as Back
import SlamData.Notebook.Deck.Component as Deck
import SlamData.Notebook.Deck.Component.ChildSlot (CardSlot(..))
import SlamData.Notebook.Deck.DeckId (DeckId(..))
import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Model as Model
import SlamData.Notebook.FormBuilder.Component as FB
import SlamData.Notebook.FormBuilder.Item.Model as FBI
import SlamData.Notebook.Routing (mkNotebookCardURL)
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn

import Utils.Path as UP

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type DraftboardHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DraftboardDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent
    { render
    , eval
    , peek: Just (peek ∘ H.runChildF)
    }

render ∷ State → DraftboardHTML
render state =
  HH.div
    [ HP.classes classes
    , HE.onClick (HE.input_ DismissAll)
    ]
    $ header ⊕ [ deck, dialog ]
  where
  header ∷ Array DraftboardHTML
  header = do
    guard (not shouldHideTopMenu)
    pure $ HH.slot' cpHeader unit \_→
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck ∷ DraftboardHTML
  deck =
    HH.div [ HP.classes [ notebookClass ] ]
      [ HH.slot' cpDeck unit \_ →
         { component: Deck.comp
         , initialState: Deck.initialState (state.browserFeatures)
         }
      ]

  dialog ∷ DraftboardHTML
  dialog =
    HH.slot' cpDialog unit \_ →
      { component: Dialog.comp
      , initialState: H.parentState Dialog.initialState
      }

  shouldHideTopMenu ∷ Boolean
  shouldHideTopMenu =
    isJust (state ^. _viewingCard)
    ∨ isReadOnly (state ^. _accessType)

  shouldHideEditors ∷ Boolean
  shouldHideEditors =
    isReadOnly (state ^. _accessType)

  classes ∷ Array ClassName
  classes =
    if shouldHideEditors
      then [ Rc.notebookViewHack ]
      else [ Rc.dashboard ]

  notebookClass ∷ ClassName
  notebookClass =
    if shouldHideTopMenu
      then className "sd-notebook-hidden-top-menu"
      else className "sd-notebook"

eval ∷ Natural Query DraftboardDSL
eval (SetAccessType aType next) = do
  H.modify (_accessType .~ aType)
  queryDeck $ H.action $ Deck.SetAccessType aType
  pure next
eval (GetAccessType k) = k <$> H.gets _.accessType
eval (SetViewingCard mbcid next) = do
  H.modify (_viewingCard .~ mbcid)
  queryDeck $ H.action $ Deck.SetViewingCard mbcid
  pure next
eval (GetViewingCard k) = k <$> H.gets _.viewingCard
eval (SetParentHref href next) = H.modify (_parentHref ?~ href) $> next
eval (DismissAll next) = do
  querySignIn $ H.action SignIn.DismissSubmenu
  pure next
eval (GetPath k) = k <$> H.gets _.path
eval (Reset features path next) = do
  H.modify (_path .~ Just path)
  queryDeck $ H.action $ Deck.Reset features path Nothing
  pure next
eval (Load features path deckIds next) = do
  H.modify (_path .~ Just path)
  let load = void ∘ queryDeck ∘ H.action ∘ Deck.LoadNotebook features path
  case L.head deckIds of
    Just deckId → load deckId
    Nothing → rootDeck path >>= either (const (pure unit)) load
    -- Show load error somehow
  pure next

rootDeck ∷ UP.DirPath → DraftboardDSL (Either String DeckId)
rootDeck path = map (map DeckId) $ Model.getRoot (path </> Pathy.file "index")


peek ∷ ∀ a. ChildQuery a → DraftboardDSL Unit
peek =
  coproduct3
    dialogParentPeek
    deckPeek
    (const $ pure unit)

dialogParentPeek ∷ ∀ a. Dialog.QueryP a → DraftboardDSL Unit
dialogParentPeek = coproduct dialogPeek (const (pure unit))

dialogPeek ∷ ∀ a. Dialog.Query a → DraftboardDSL Unit
dialogPeek _ = pure unit

deckPeek ∷ ∀ a. Deck.QueryP a → DraftboardDSL Unit
deckPeek =
  coproduct
    (const (pure unit))
    \(H.ChildF s q) →
      coproduct
        (either peekCards (\_ _ → pure unit) s)
        backsidePeek
        q
  where
  peekCards (CardSlot cid) q =
    coproduct
      (cardPeek cid)
      (const $ pure unit)
      q

  backsidePeek (Back.UpdateFilter _ _) = pure unit
  backsidePeek (Back.DoAction action _) = case action of
    Back.Trash →
      pure unit
    Back.Share → do
      loc ← H.fromEff locationString
      queryDeck (H.request Deck.GetNotebookPath)
        <#> join
        >>= traverse_ \nPath →
          showDialog
          $ Dialog.Share
          $ loc
          ⊕ "/"
          ⊕ SlamData.Config.notebookUrl
          ⊕ "#"
          ⊕ UP.encodeURIPath (Pathy.printPath nPath)
          ⊕ "view"
    Back.Embed →
      queryDeck (H.request Deck.GetActiveCardId)
        <#> join
        >>= traverse_ shareCard
    Back.Publish →
      pure unit
    Back.Mirror →
      pure unit
    Back.Wrap →
      pure unit

shareCard ∷ CID.CardId → DraftboardDSL Unit
shareCard cid = do
  root ← H.fromEff locationString
  showDialog ∘ either Dialog.Error (uncurry Dialog.Embed) =<< ET.runExceptT do
    liftDeckQuery $ H.action Deck.SaveNotebook
    path ←
      liftDeckQuery (H.request Deck.GetNotebookPath)
        >>= maybe (EC.throwError "Could not determine notebook path") pure
    varMap ←
      liftDeckQuery (H.request (Deck.FindCardParent cid))
        >>= maybe (pure SM.empty) hereditaryVarMapDefaults
    pure $
      Tuple
      (root ⊕ "/" ⊕ mkNotebookCardURL path cid NA.ReadOnly SM.empty)
      varMap

hereditaryVarMapDefaults ∷ CID.CardId → ET.ExceptT String DraftboardDSL VM.VarMap
hereditaryVarMapDefaults cid = do
  pid ← liftDeckQuery (H.request (Deck.FindCardParent cid))
  SM.union
    <$> varMapDefaults cid
    <*> (traverse hereditaryVarMapDefaults pid <#> fromMaybe SM.empty)


varMapDefaults ∷ CID.CardId → ET.ExceptT String DraftboardDSL VM.VarMap
varMapDefaults cid = do
  τ ←
    liftDeckQuery (H.request (Deck.GetCardType cid))
      >>= maybe (EC.throwError "Could not determine card type") pure
  case τ of
    CT.API → do
      let
        defaultVarMapValue { defaultValue, fieldType } =
          case FBI.defaultValueToVarMapValue fieldType =<< defaultValue of
            Just val → val
            Nothing → FBI.emptyValueOfFieldType fieldType
        alg =
          SM.insert
          <$> _.name
          <*> defaultVarMapValue
      liftFormBuilderQuery cid (H.request FB.GetItems)
        <#> foldl (flip alg) SM.empty
    _ →
      pure SM.empty



cardPeek ∷ ∀ a. CID.CardId → CQ.CardQuery a → DraftboardDSL Unit
cardPeek cid q =
  case q of
    CQ.ShareCard _ → shareCard cid
    _ → pure unit


liftFormBuilderQuery
  ∷ CID.CardId → Natural FB.Query (ET.ExceptT String DraftboardDSL)
liftFormBuilderQuery cid =
  liftCardQuery cid
    ∘ CQ.APIQuery
    ∘ right
    ∘ H.ChildF unit
    ∘ left

liftCardQuery
  ∷ CID.CardId
  → Natural CQ.AnyCardQuery (ET.ExceptT String DraftboardDSL)
liftCardQuery cid =
  queryCard cid ⋙ lift
    >=> maybe (EC.throwError "Error querying card") pure

liftDeckQuery
  ∷ Natural Deck.Query (ET.ExceptT String DraftboardDSL)
liftDeckQuery =
  queryDeck ⋙ lift
    >=> maybe (EC.throwError "Error querying notebook") pure

showDialog ∷ Dialog.Dialog → DraftboardDSL Unit
showDialog =
  queryDialog
    ∘ H.action
    ∘ Dialog.Show

queryDialog ∷ Dialog.Query Unit → DraftboardDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryDeck ∷ ∀ a. Deck.Query a → DraftboardDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ left

queryCard ∷ ∀ a. CID.CardId → CQ.AnyCardQuery a → DraftboardDSL (Maybe a)
queryCard cid =
  H.query' cpDeck unit
    ∘ right
    ∘ H.ChildF (Left $ CardSlot cid)
    ∘ left
    ∘ right
    ∘ H.ChildF unit
    ∘ right

querySignIn ∷ ∀ a. SignIn.Query a → DraftboardDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpSignIn unit)
    ∘ right
    ∘ left
