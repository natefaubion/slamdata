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

module SlamData.Workspace.Card.Draftboard.Component where

import SlamData.Prelude

import Data.Array as Array
import Data.Foldable (and, all, find)
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Ord (abs)
import Data.Rational (Rational, (%))

import Halogen as H
import Halogen.Component.Utils (raise')
import Halogen.Component.Utils.Drag as Drag

import SlamData.Analytics as SA
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Draftboard.Common (deleteGraph, clearDeckId, unsafeUpdateCachedDraftboard)
import SlamData.Workspace.Card.Draftboard.Component.Common (DraftboardDSL)
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..))
import SlamData.Workspace.Card.Draftboard.Component.Render (render)
import SlamData.Workspace.Card.Draftboard.Component.State (MoveLocation(..), initialState, modelFromState, childSlots, updateRect, updateLayout)
import SlamData.Workspace.Deck.Common (DeckOptions, wrappedDeck, splitDeck)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId, freshDeckId)
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Notification as Notify
import SlamData.Wiring as W

import Utils.DOM (getOffsetClientRect)
import Utils.Path (DirPath)

draftboardComponent ∷ CardOptions → CC.CardComponent
draftboardComponent opts = CC.makeCardComponent
  { cardType: CT.Draftboard
  , component: H.parentComponent
      { render: render opts
      , eval: coproduct evalCard (evalBoard opts)
      , peek: Just (peek opts)
      }
  , initialState: H.parentState initialState
  , _State: CC._DraftboardState
  , _Query: CC.makeQueryPrism' CC._DraftboardQuery
  }

evalCard ∷ CC.CardEvalQuery ~> DraftboardDSL
evalCard = case _ of
  CC.EvalCard _ _ next →
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.SetDimensions _ next → do
    recalcRect
    H.queryAll (right (H.action DCQ.UpdateCardSize))
    pure next
  CC.Save k →
    map (k ∘ Card.Draftboard ∘ modelFromState) H.get
  CC.Load card next → do
    case card of
      Card.Draftboard model → do
        H.modify (updateLayout model.layout)
        loadDecks
      _ → pure unit
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalBoard ∷ CardOptions → Query ~> DraftboardDSL
evalBoard opts = case _ of
  SetRoot el next → do
    H.modify _ { root = el }
    pure next
  SplitStart orientation bias root ev next → do
    st ← H.get
    Drag.subscribe' ev (right ∘ H.action ∘ Splitting)
    H.modify _ { splitOpts = Just { orientation, bias, root } }
    pure next
  Splitting ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          x = d.x - st.rootRect.left
          y = d.y - st.rootRect.top
          result = do
            { orientation, bias, root } ← st.splitOpts
            { cursor } ← overlapping x y st.cellLayout
            let
              rectR = st.rootRect
              cursor' =
                if not root
                  then fromMaybe Nil (Layout.findSplit orientation cursor st.layout)
                  else Nil
            rs ←
              if List.null cursor'
                then pure Nil
                else getRatios =<< Pane.getAt cursor' st.layout
            rectT ← Layout.absoluteRect rectR <$> Layout.rectAt cursor' st.layout
            let
              relX = d.x - (rectR.left + rectT.left)
              relY = d.y - (rectR.top + rectT.top)
            pure case orientation of
              Orn.Horizontal →
                let
                  ratio = Layout.closestSnapRatio (relX / rectT.width)
                  valid = isSensibleSplit ratio rs && not (isEdge ratio)
                in
                  { orientation
                  , bias
                  , cursor: cursor'
                  , ratio
                  , valid
                  , x: d.x - rectR.left
                  , y: rectT.top
                  , z: rectT.height
                  }
              Orn.Vertical →
                let
                  ratio = Layout.closestSnapRatio (relY / rectT.height)
                  valid = isSensibleSplit ratio rs && not (isEdge ratio)
                in
                  { orientation
                  , bias
                  , cursor: cursor'
                  , ratio
                  , valid
                  , x: rectT.left
                  , y: d.y - rectR.top
                  , z: rectT.width
                  }
        H.modify _ { splitLocation = result }
      Drag.Done _ → do
        let
          result = do
            opts ← st.splitOpts
            loc ← st.splitLocation
            guard loc.valid
            if opts.root
              then pure (Layout.insertRootSplit (Pane.Cell Nothing) loc.orientation loc.ratio loc.bias st.layout)
              else Layout.insertSplit (Pane.Cell Nothing) loc.orientation loc.ratio loc.bias loc.cursor st.layout
        H.modify
          $ updateLayout (fromMaybe st.layout result)
          ∘ _ { splitOpts = Nothing, splitLocation = Nothing }
        H.queryAll (right (H.action DCQ.UpdateCardSize))
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  ResizeStart edge ev next → do
    let
      loc =
        { edge
        , ratio: edge.ratio
        , valid: true
        , collapse: Nothing
        , offset: 0.0
        , initial: case edge.orientation of
            Orn.Horizontal → ev.pageX
            Orn.Vertical   → ev.pageY
        }
    Drag.subscribe' ev (right ∘ H.action ∘ Resizing)
    H.modify _ { resizeLocation = Just loc }
    pure next
  Resizing ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          rectR = st.rootRect
          result = do
            loc   ← st.resizeLocation
            rectT ← Layout.absoluteRect rectR <$> Layout.rectAt loc.edge.parent st.layout
            sibs  ← Pane.children <$> Pane.getAt loc.edge.parent st.layout
            let
              ix   = loc.edge.index + 1
              relX = d.x - (rectR.left + rectT.left)
              relY = d.y - (rectR.top + rectT.top)
              pre  = List.take ix sibs
              post = List.drop ix sibs
            pure case loc.edge.orientation of
              Orn.Horizontal →
                let
                  ratio = Layout.closestSnapRatio (relX / rectT.width)
                  collapse = isCollapsing ratio pre post
                  valid = not (isEdge ratio) || isJust collapse
                in loc
                  { offset = d.x - loc.initial
                  , ratio = ratio
                  , valid = valid
                  , collapse = collapse
                  }
              Orn.Vertical →
                let
                  ratio = Layout.closestSnapRatio (relY / rectT.height)
                  collapse = isCollapsing ratio pre post
                  valid = not (isEdge ratio) || isJust collapse
                in loc
                  { offset = d.y - loc.initial
                  , ratio = ratio
                  , valid = valid
                  , collapse = collapse
                  }
        H.modify _ { resizeLocation = result }
      Drag.Done _ → do
        let
          result = do
            loc ← st.resizeLocation
            guard loc.valid
            Layout.resizeEdge' loc.edge.index loc.ratio loc.edge.parent st.layout
        H.modify
          $ updateLayout (fromMaybe st.layout result)
          ∘ _ { resizeLocation = Nothing }
        H.queryAll (right (H.action DCQ.UpdateCardSize))
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  DeleteCell cursor next → do
    st ← H.get
    let
      result = Layout.deleteCell cursor st.layout
    H.modify (updateLayout (fromMaybe st.layout result))
    H.queryAll (right (H.action DCQ.UpdateCardSize))
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Grabbing (deckId × cursor) ev next → do
    st ← H.get
    case ev of
      Drag.Move _ d → do
        let
          x = d.x - st.rootRect.left
          y = d.y - st.rootRect.top
        case overlapping x y st.cellLayout of
          Just cell@{ value: Nothing } →
            H.modify _ { moveLocation = Just (Move cell) }
          Just cell@{ value: (Just deckId'), rect } | deckId /= deckId' → do
            let
              relX = Int.floor (x - rect.left) % Int.floor rect.width
              relY = Int.floor (y - rect.top) % Int.floor rect.height
              move = case relX < 1%2, relY < 1%2 of
                xa, ya | xa && ya && relY < relX   → Group cell Orn.Vertical Layout.SideA
                xa, ya | xa && ya                  → Group cell Orn.Horizontal Layout.SideA
                xa, ya | xa && one - relY < relX   → Group cell Orn.Vertical Layout.SideB
                xa, ya | xa                        → Group cell Orn.Horizontal Layout.SideA
                xa, ya | ya && relY < one - relX   → Group cell Orn.Vertical Layout.SideA
                xa, ya | ya                        → Group cell Orn.Horizontal Layout.SideB
                xa, ya | one - relY < one - relX   → Group cell Orn.Vertical Layout.SideB
                _ , _                              → Group cell Orn.Horizontal Layout.SideB
            H.modify _ { moveLocation = Just move }
          _ →
            H.modify _ { moveLocation = Just (Floating x y) }
      Drag.Done _ → do
        for_ st.moveLocation case _ of
          Floating _ _ →
            H.modify _ { moveLocation = Nothing }
          Group cell orn bias →
            for_ cell.value \deckId' → do
              H.modify _ { moveLocation = Nothing }
              groupDecks opts orn bias (deckId × cursor) (deckId' × cell.cursor)
          Move cell → do
            let
              result =
                pure st.layout
                  >>= Pane.modifyAt (const (Pane.Cell Nothing)) cursor
                  >>= Pane.modifyAt (const (Pane.Cell (Just deckId))) cell.cursor
            H.modify
              $ updateLayout (fromMaybe st.layout result)
              ∘ _ { moveLocation = Nothing }
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  LoadDeck deckId next → do
    queryDeck deckId (H.action (DCQ.Load opts.deck.path deckId))
    pure next
  AddDeck cursor next → do
    addDeck opts DM.emptyDeck cursor
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  GetDecks k → do
    st ← H.get
    decks ←
      for st.layout case _ of
        Just deckId → do
          deck ← queryDeck deckId (H.request DCQ.GetModel)
          pure ((deckId × _) <$> deck)
        Nothing →
          pure Nothing
    pure (k decks)
  GetDecksSharingInput k → do
    decks ← H.gets _.cursors
    deckMbList ← for (Map.keys decks) \deckId → do
      mbInput ← queryDeck deckId $ H.request DCQ.GetSharingInput
      pure $ deckId × mbInput
    pure $ k
      $ foldMap (\(deckId × sharing) → foldMap (Map.singleton deckId) sharing) deckMbList

peek ∷ ∀ a. CardOptions → H.ChildF DeckId DNQ.QueryP a → DraftboardDSL Unit
peek opts (H.ChildF deckId q) = coproduct (const (pure unit)) peekDeck q
  where
  peekDeck ∷ DCQ.Query a → DraftboardDSL Unit
  peekDeck q = do
    st ← H.get
    for_ (Map.lookup deckId st.cursors) \cursor →
      case q of
        DCQ.GrabDeck ev _ → do
          startDragging ev (Grabbing (deckId × cursor))
        DCQ.DoAction DCQ.DeleteDeck _ → do
          SA.track (SA.Delete deckId)
          deleteDeck opts deckId
          CC.raiseUpdatedP' CC.EvalModelUpdate
        DCQ.DoAction DCQ.Wrap _ → do
          SA.track (SA.Wrap deckId)
          wrapDeck opts deckId cursor
          CC.raiseUpdatedP' CC.EvalModelUpdate
        DCQ.DoAction (DCQ.Unwrap decks) _ → do
          SA.track (SA.Collapse deckId)
          unwrapDeck opts deckId cursor decks
          CC.raiseUpdatedP' CC.EvalModelUpdate
        DCQ.DoAction DCQ.Mirror _ → do
          SA.track (SA.Mirror deckId)
          mirrorDeck opts deckId cursor
          CC.raiseUpdatedP' CC.EvalModelUpdate
        _ → pure unit

  startDragging ev tag =
    void $ Drag.subscribe' ev $ right ∘ H.action ∘ tag

isCollapsing
  ∷ ∀ a
  . Rational
  → List (Pane.Pane (Maybe a))
  → List (Pane.Pane (Maybe a))
  → Maybe Layout.SplitBias
isCollapsing ratio pre post
  | ratio == zero && and (map (all isNothing) pre) = Just Layout.SideA
  | ratio == one && and (map (all isNothing) post) = Just Layout.SideB
  | otherwise = Nothing

isSensibleSplit ∷ Rational → List (Rational) → Boolean
isSensibleSplit ratio = go zero
  where
  go off Nil = true
  go off (r : xs) =
    if abs (ratio - off) < 1%16
      then false
      else go (off + r) xs

getRatios ∷ ∀ a . Pane.Pane a → Maybe (List Rational)
getRatios (Pane.Split _ ps) = Just (map fst ps)
getRatios _ = Nothing

isEdge ∷ Rational → Boolean
isEdge r = r == zero || r == one

overlapping
  ∷ ∀ a
  . Number
  → Number
  → List (Layout.Cell a Number)
  → Maybe (Layout.Cell a Number)
overlapping x y = find go
  where
  go { rect: { top, left, width, height } } =
    y >= top && x >= left && y < top + height && x < left + width

recalcRect ∷ DraftboardDSL Unit
recalcRect = do
  st ← H.get
  for_ st.root \root → do
    rect ← H.fromEff (getOffsetClientRect root)
    H.modify (updateRect rect)

saveChildDeck ∷ CardOptions → DM.Deck → DraftboardDSL (Maybe DeckId)
saveChildDeck { deck: opts, deckId: parentId, cardId } deck = do
  let deck' = deck { parent = Just (parentId × cardId) }
  H.modify _ { inserting = true }
  deckId ← H.fromEff freshDeckId
  putDeck opts.path deckId deck' >>= case _ of
    Left err → do
      H.modify _ { inserting = false }
      Notify.saveDeckFail err
      pure Nothing
    Right _ → do
      pure (Just deckId)

addDeck ∷ CardOptions → DM.Deck → Pane.Cursor → DraftboardDSL Unit
addDeck opts deck cursor = do
  saveChildDeck opts deck >>= traverse_ \deckId → do
    st ← H.get
    let
      layout = Pane.modifyAt (const (Pane.Cell (Just deckId))) cursor st.layout
    H.modify
      $ updateLayout (fromMaybe st.layout layout)
      ∘ _ { inserting = false }
    loadAndFocus opts.deck deckId

deleteDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
deleteDeck { deck } deckId = do
  res ← H.liftH $ H.liftH $ deleteGraph deck.path deckId
  st ← H.get
  case res of
    Left err →
      H.liftH $ H.liftH $ Notify.deleteDeckFail err
    Right _ →
      H.modify (updateLayout (clearDeckId deckId st.layout))

wrapDeck ∷ CardOptions → DeckId → Pane.Cursor → DraftboardDSL Unit
wrapDeck { cardId, deckId: parentId, deck } oldId cursor = do
  let
    newDeck = (wrappedDeck oldId) { parent = Just (parentId × cardId) }
  newId ← H.fromEff freshDeckId
  putDeck deck.path newId newDeck >>= case _ of
    Left err →
      Notify.saveDeckFail err
    Right _ → void do
      traverse_ (queryDeck oldId ∘ H.action)
        [ DCQ.SetParent (newId × CID.CardId 0)
        , DCQ.Save Nothing
        ]
      st ← H.get
      let
        layout = Pane.modifyAt (const (Pane.Cell (Just newId))) cursor st.layout
      H.modify (updateLayout (fromMaybe st.layout layout))
      loadAndFocus deck newId

unwrapDeck
  ∷ CardOptions
  → DeckId
  → Pane.Cursor
  → Pane.Pane (Maybe (DeckId × DM.Deck))
  → DraftboardDSL Unit
unwrapDeck { deckId, cardId, deck: opts } oldId cursor decks = do
  let
    coord = deckId × cardId
  subLayout ←
    for decks case _ of
      Nothing → pure Nothing
      Just (deckId × deck) → do
        let
          deck' = deck { parent = Just coord }
        putDeck opts.path deckId deck'
        pure (Just deckId)
  st ← H.get
  let
    layout = Pane.modifyAt (const subLayout) cursor st.layout
  H.modify (updateLayout (fromMaybe st.layout layout))
  for_ subLayout $ traverse_ \deckId →
    raise' (right (H.action (LoadDeck deckId)))

mirrorDeck ∷ CardOptions → DeckId → Pane.Cursor → DraftboardDSL Unit
mirrorDeck opts oldId cursor = do
  queryDeck oldId (H.request DCQ.GetModelCards) >>= traverse_ \modelCards → do
    let modelCards' = Array.span (not ∘ eq oldId ∘ fst) modelCards
    if Array.null modelCards'.rest
      then insertNewDeck DM.emptyDeck { mirror = DCS.coordModelToCoord <$> modelCards'.init }
      else do
        let
          newDeck =
            { parent: Nothing
            , mirror: map _.cardId <$> modelCards'.init
            , cards: snd <$> modelCards'.rest
            , name: ""
            }
        newId ← H.fromEff freshDeckId
        putDeck opts.deck.path newId newDeck >>= case _ of
          Left err →
            Notify.saveDeckFail err
          Right _ → do
            let
              modelCards'' =
                modelCards'.init <> map (lmap (const newId)) modelCards'.rest
            queryDeck oldId $ H.action $ DCQ.SetModelCards modelCards''
            insertNewDeck DM.emptyDeck { mirror = DCS.coordModelToCoord <$> modelCards'' }
  where
  insertNewDeck deck = do
    st ← H.get
    let
      cursor' = case cursor of
        _ : cs → cs
        _ → Nil
      orn = case Pane.getAt cursor' st.layout of
        Just (Pane.Split o _) → o
        _ → Orn.Vertical
    saveChildDeck opts deck >>= traverse_ \deckId → do
      let
        layout = Layout.insertSplit (Pane.Cell (Just deckId)) orn (1%2) Layout.SideB cursor' st.layout
      H.modify (updateLayout (fromMaybe st.layout layout))
      loadAndFocus opts.deck deckId

groupDecks
  ∷ CardOptions
  → Orn.Orientation
  → Layout.SplitBias
  → (DeckId × Pane.Cursor)
  → (DeckId × Pane.Cursor)
  → DraftboardDSL Unit
groupDecks opts orn bias (deckFrom × cursorFrom) (deckTo × cursorTo) = do
  st ← H.get
  queryDeck deckTo (H.request DCQ.GetModelCards) >>= case _ of
    Just [ deckId' × { cardId, model: Card.Draftboard { layout } } ] → void do
      let
        layout' = Layout.insertRootSplit (Pane.Cell (Just deckFrom)) orn (1%2) bias layout
        card = { cardId, model: Card.Draftboard { layout: layout' } }
      H.liftH $ H.liftH $ unsafeUpdateCachedDraftboard deckId' card
      H.modify \s →
        let
          result =
            Pane.modifyAt (const (Pane.Cell Nothing)) cursorFrom st.layout
        in updateLayout (fromMaybe s.layout result) s
      queryDeck deckTo
        $ H.action
        $ DCQ.SetModelCards [ deckId' × card ]

    _ → do
      let
        splits =
          List.fromFoldable case bias of
            Layout.SideA → [ deckFrom, deckTo ]
            Layout.SideB → [ deckTo, deckFrom ]
        newDeck = (splitDeck orn splits) { parent = Just (opts.deckId × opts.cardId) }
      newId ← H.fromEff freshDeckId
      putDeck opts.deck.path newId newDeck >>= case _ of
        Left err →
          Notify.saveDeckFail err
        Right _ → void do
          H.modify \s →
            let
              result =
                pure s.layout
                  >>= Pane.modifyAt (const (Pane.Cell Nothing)) cursorFrom
                  >>= Pane.modifyAt (const (Pane.Cell (Just newId))) cursorTo
            in updateLayout (fromMaybe s.layout result) s
          loadAndFocus opts.deck newId

loadDecks ∷ DraftboardDSL Unit
loadDecks =
  H.gets childSlots >>=
    traverse_ (raise' ∘ right ∘ H.action ∘ LoadDeck)

queryDeck ∷ ∀ a. DeckId → DCQ.Query a → DraftboardDSL (Maybe a)
queryDeck slot = H.query slot ∘ right

loadAndFocus ∷ DeckOptions → DeckId → DraftboardDSL Unit
loadAndFocus opts deckId =
  traverse_ (queryDeck deckId ∘ H.action)
    [ DCQ.Load opts.path deckId
    , DCQ.Focus
    ]

putDeck
  ∷ DirPath
  → DeckId
  → DM.Deck
  → DraftboardDSL (Either QE.QError Unit)
putDeck path deckId deck =
  H.liftH $ H.liftH $ W.putDeck path deckId deck
