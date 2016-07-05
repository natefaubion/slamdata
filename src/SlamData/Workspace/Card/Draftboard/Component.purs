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

module SlamData.Workspace.Card.Draftboard.Component
  ( draftboardComponent
  , module SlamData.Workspace.Card.Draftboard.Component.Query
  , module SlamData.Workspace.Card.Draftboard.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Data.Array as Array
import Data.Function (on)
import Data.Lens ((.~), (?~))
import Data.List as List
import Data.Map as Map
import CSS as CSS

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils.Drag as Drag
import Halogen.Component.Utils (raise')
import Halogen.CustomEvents (mouseEventToPageEvent)
import Halogen.HTML.CSS.Indexed as HC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Math (round, floor)

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Draftboard.Common (deleteGraph, unsafeUpdateCachedDraftboard)
import SlamData.Workspace.Card.Draftboard.Component.CSS as CCSS
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..), QueryP, QueryC)
import SlamData.Workspace.Card.Draftboard.Component.State (State, DeckPosition, initialState, encode, decode, _moving, _inserting, _grouping, modelFromState)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.Common (wrappedDeck, defaultPosition)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString, freshDeckId)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Wiring (putDeck)

import Utils.CSS (zIndex)
import Utils.DOM (elementEq, scrollTop, scrollLeft, getOffsetClientRect)

type DraftboardDSL = H.ParentDSL State DNS.State QueryC DNQ.QueryP Slam DeckId

type DraftboardHTML = H.ParentHTML DNS.State QueryC DNQ.QueryP Slam DeckId

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

render ∷ CardOptions → State → DraftboardHTML
render opts state =
  HH.div_
    [ HH.div [ HP.class_ CCSS.insetShadow ] []
    , HH.div
        [ HP.class_ CCSS.grid
        , HC.style bgSize
        , HP.ref (right ∘ H.action ∘ SetElement)
        , HE.onClick \e → pure $
            guard (AT.isEditable opts.deck.accessType && not state.inserting) $>
            right (H.action $ AddDeck e)
        ]
        $ map renderDeck (foldl Array.snoc [] $ Map.toList state.decks)
    ]

  where
  renderDeck (deckId × rect) =
    HH.div
      [ HP.key $ deckIdToString deckId
      , HP.classes $
          case state.grouping of
            Just deckId' | deckId == deckId' → [ HH.className "grouping" ]
            _ → []
      , HC.style $
          case state.moving of
            Just (deckId' × rect') | deckId == deckId' → zIndex 2 *> cssPos rect'
            _ → cssPos rect
      ]
      [ HH.slot deckId $ mkDeckComponent deckId ]

  mkDeckComponent id _ =
    { component: opts.deckComponent (opaqueState $ DCS.initialDeck opts.deck.path id)
    , initialState: DNS.initialState
    }

  cssPos rect = do
    CSS.position CSS.absolute
    CSS.top $ CSS.px $ gridToPx rect.y
    CSS.left $ CSS.px $ gridToPx rect.x
    CSS.width $ CSS.px $ gridToPx rect.width
    CSS.height $ CSS.px $ gridToPx rect.height

  bgSize = do
    let size = foldr maxSize { width: 0.0, height: 0.0 } state.decks
        size' = maybe size (flip maxSize size ∘ snd) state.moving
    CSS.width $ CSS.px $ gridToPx $ size'.width + 1.0
    CSS.height $ CSS.px $ gridToPx $ size'.height + 1.0

evalCard ∷ Natural CC.CardEvalQuery DraftboardDSL
evalCard = case _ of
  CC.EvalCard _ _ next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.Save k →
    map (k ∘ Card.Draftboard ∘ modelFromState) H.get
  CC.Load card next → do
    case card of
      Card.Draftboard model → do
        H.modify _ { decks = model.decks }
        loadDecks
      _ → pure unit
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalBoard ∷ CardOptions → Natural Query DraftboardDSL
evalBoard opts = case _ of
  Grabbing deckId ev next → do
    case ev of
      Drag.Move _ d → do
        st ← H.get
        for_ (Map.lookup deckId (st.decks)) \rect →
        for_ st.canvas \el → do
          coords ← pageToGrid d el
          let
            cursorRect =
              { x: coords.x
              , y: coords.y
              , width: 1.0
              , height: 1.0
              }
            newRect = clampDeck rect
              { x = rect.x + (pxToGrid d.offsetX)
              , y = rect.y + (pxToGrid d.offsetY)
              }
            grouping =
              case overlapping cursorRect (Map.toList st.decks) of
                List.Cons (deckId' × _) _ | deckId ≠ deckId' → Just deckId'
                _ → Nothing
          H.modify
            $ (_moving ?~ Tuple deckId newRect)
            ∘ (_grouping .~ grouping)
      Drag.Done _ → do
        stopDragging opts
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Resizing deckId ev next → do
    case ev of
      Drag.Move _ d → do
        H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
          let
            newRect = clampDeck rect
              { width = rect.width + (pxToGrid d.offsetX)
              , height = rect.height + (pxToGrid d.offsetY)
              }
          H.modify $ _moving ?~ Tuple deckId newRect
      Drag.Done _ → do
        stopDragging opts
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  SetElement el next → do
    H.modify _ { canvas = el }
    pure next
  AddDeck e next → do
    let e' = mouseEventToPageEvent e
    H.gets _.canvas >>= traverse_ \el → do
      same ← H.fromEff (elementEq el e'.target)
      when same do
        coords ← pageToGrid { x: e'.pageX, y: e'.pageY } el
        addDeck opts DM.emptyDeck
          { x: floor $ coords.x + 1.0
          , y: floor $ coords.y
          }
        CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  LoadDeck deckId next → do
    queryDeck deckId
      $ H.action
      $ DCQ.Load opts.deck.path deckId (DL.succ opts.deck.level)
    pure next
  -- TODO: hopefully we can get rid of this later, as it's relatively expensive
  -- and will run quite frequently, as it's used to enable the `Unwrap` action.
  -- If we can add a real mechanism for handling card-specific actions on the
  -- deck backside we should be able to do something much more sensible, only
  -- building the decks' value here when we actually trigger an unwrap. -gb
  GetDecks k → do
    decks ← H.gets _.decks
    decks' ← runMaybeT $ for (Map.toList decks) \(deckId × position) -> do
      model ← MaybeT $ queryDeck deckId $ H.request DCQ.GetModel
      pure (deckId × (position × model))
    pure $ k $ maybe Map.empty Map.fromList decks'

peek ∷ ∀ a. CardOptions → H.ChildF DeckId DNQ.QueryP a → DraftboardDSL Unit
peek opts (H.ChildF deckId q) = coproduct (const (pure unit)) peekDeck q
  where
  peekDeck ∷ DCQ.Query a → DraftboardDSL Unit
  peekDeck = case _ of
    DCQ.GrabDeck ev _ → startDragging deckId ev Grabbing
    DCQ.ResizeDeck ev _ → startDragging deckId ev Resizing
    DCQ.DoAction DCQ.DeleteDeck _ → do
      deleteDeck opts deckId
      CC.raiseUpdatedP' CC.EvalModelUpdate
    DCQ.DoAction DCQ.Wrap _ → do
      wrapDeck opts deckId
      CC.raiseUpdatedP' CC.EvalModelUpdate
    DCQ.DoAction (DCQ.Unwrap decks) _ → do
      unwrapDeck opts deckId decks
      CC.raiseUpdatedP' CC.EvalModelUpdate
    DCQ.DoAction DCQ.Mirror _ → do
      mirrorDeck opts deckId
      CC.raiseUpdatedP' CC.EvalModelUpdate
    _ → pure unit

  startDragging deckId ev tag =
    H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
      H.modify $ _moving ?~ Tuple deckId rect
      void
        $ Drag.subscribe' ev
        $ right ∘ H.action ∘ tag deckId

pageToGrid
  ∷ ∀ r
  . { x ∷ Number, y ∷ Number | r }
  → HTMLElement
  → DraftboardDSL { x ∷ Number, y ∷ Number }
pageToGrid e el = do
  rect ← H.fromEff $ getOffsetClientRect el
  scroll ←
    { top: _, left: _ }
    <$> H.fromEff (scrollTop el)
    <*> H.fromEff (scrollLeft el)
  pure
    { x: pxToGrid $ e.x - rect.left + scroll.left
    , y: pxToGrid $ e.y - rect.top + scroll.top
    }

pxToGrid ∷ Number → Number
pxToGrid = (_ / Config.gridPx)

gridToPx ∷ Number → Number
gridToPx = (_ * Config.gridPx)

stopDragging ∷ CardOptions → DraftboardDSL Unit
stopDragging opts = do
  st ← H.get
  for_ st.moving \(deckId × rect) → do
    let rect' = roundDeck rect
        decks = List.filter ((deckId /= _) ∘ fst) $ Map.toList st.decks
    case st.grouping of
      Just deckId' →
        groupDecks opts deckId deckId'
      Nothing →
        when (List.null $ overlapping rect' decks) do
          H.modify \s → s { decks = Map.insert deckId rect' s.decks }
  H.modify _
    { moving = Nothing
    , grouping = Nothing
    }

maxSize
  ∷ DeckPosition
  → { width ∷ Number, height ∷ Number }
  → { width ∷ Number, height ∷ Number }
maxSize deck size =
  { width: if x > size.width then x else size.width
  , height: if y > size.height then y else size.height
  }
  where
  x = deck.x + deck.width
  y = deck.y + deck.height

clampDeck ∷ DeckPosition → DeckPosition
clampDeck rect =
  { x: if rect.x < 0.0 then 0.0 else rect.x
  , y: if rect.y < 0.0 then 0.0 else rect.y
  , width: if rect.width < 10.0 then 10.0 else rect.width
  , height: if rect.height < 10.0 then 10.0 else rect.height
  }

roundDeck ∷ DeckPosition → DeckPosition
roundDeck rect =
  { x: round rect.x
  , y: round rect.y
  , width: round rect.width
  , height: round rect.height
  }

overlapping
  ∷ DeckPosition
  → List.List (DeckId × DeckPosition)
  → List.List (DeckId × DeckPosition)
overlapping a = List.filter go
  where
  go (_ × b) =
    not $ a.x + a.width <= b.x
       || b.x + b.width <= a.x
       || a.y + a.height <= b.y
       || b.y + b.height <= a.y

accomodateDeck
  ∷ List.List (DeckId × DeckPosition)
  → { x ∷ Number, y ∷ Number }
  → DeckPosition
  → Maybe DeckPosition
accomodateDeck bs = go List.Nil
  where
  go hist c a =
    case overlapping a bs of
      List.Nil →
        Just a
      bs' | not $ List.null $ List.intersectBy (eq `on` fst) hist bs' →
        Nothing
      List.Cons b _ →
        let a' = clampDeck $ reposition c a (snd b)
            c' = { x: a'.x + (floor (a'.width / 2.0)), y: a'.y }
        in  go (List.Cons b hist) c' a'

  reposition c a b
    | c.x >= b.x && c.x < b.x + b.width = a { y = b.y - a.height }
    | c.x >= b.x + b.width = a { x = b.x + b.width }
    | c.y < b.x = a { x = b.x - a.width }
    | otherwise = a

reallyAccomodateDeck
  ∷ List.List (DeckId × DeckPosition)
  → DeckPosition
  → DeckPosition
reallyAccomodateDeck decks deckPos =
  let deckPos' = deckPos { y = deckPos.y + deckPos.height }
      coords = { x: floor (deckPos'.x + deckPos'.x / 2.0), y: deckPos'.y }
  in
      case accomodateDeck decks coords deckPos of
        Nothing → reallyAccomodateDeck decks deckPos'
        Just a → a

loadDecks ∷ DraftboardDSL Unit
loadDecks =
  H.gets (Map.keys ∘ _.decks) >>=
    traverse_ (raise' ∘ right ∘ H.action ∘ LoadDeck)

addDeck ∷ CardOptions → DM.Deck → { x ∷ Number, y ∷ Number } → DraftboardDSL Unit
addDeck opts deck coords = do
  decks ← Map.toList <$> H.gets _.decks
  let
    deckPos = clampDeck $ defaultPosition
      { x = coords.x - 10.0
      , y = coords.y
      }
  for_ (accomodateDeck decks coords deckPos) $
    addDeckAt opts deck

addDeckAt ∷ CardOptions → DM.Deck → DeckPosition → DraftboardDSL Unit
addDeckAt { deck: opts, deckId: parentId, cardId } deck deckPos = do
  let deck' = deck { parent = Just (parentId × cardId) }
  H.modify $ _inserting .~ true
  deckId ← H.fromEff freshDeckId
  putDeck opts.path deckId deck' opts.wiring.decks >>= case _ of
    Left err → do
      H.modify $ _inserting .~ false
      -- TODO: do something to notify the user saving failed
      pure unit
    Right _ → void do
      H.modify \s → s
        { decks = Map.insert deckId deckPos s.decks
        , inserting = false
        }
      queryDeck deckId
        $ H.action
        $ DCQ.Load opts.path deckId (DL.succ opts.level)

deleteDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
deleteDeck { deck } deckId = do
  res ← deleteGraph deck.path deckId
  case res of
    Left err →
      -- TODO: do something to notify the user deleting failed
      pure unit
    Right _ →
      H.modify \s → s { decks = Map.delete deckId s.decks }

wrapDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
wrapDeck { cardId, deckId: parentId, deck } oldId = do
  H.gets (Map.lookup oldId ∘ _.decks) >>= traverse_ \deckPos → do
    let
      deckPos' = deckPos { x = 1.0, y = 1.0 }
      newDeck = (wrappedDeck deckPos' oldId) { parent = Just (parentId × cardId) }
    newId ← H.fromEff freshDeckId
    putDeck deck.path newId newDeck deck.wiring.decks >>= case _ of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right _ → void do
        traverse_ (queryDeck oldId ∘ H.action)
          [ DCQ.SetParent (newId × CID.CardId 0)
          , DCQ.Save Nothing
          ]
        H.modify \s → s
          { decks
              = Map.insert newId deckPos
              $ Map.delete oldId
              $ s.decks
          }
        queryDeck newId
          $ H.action
          $ DCQ.Load deck.path newId (DL.succ deck.level)

unwrapDeck
  ∷ CardOptions
  → DeckId
  → Map.Map DeckId (DeckPosition × DM.Deck)
  → DraftboardDSL Unit
unwrapDeck { deckId, cardId, deck } oldId decks = void $ runMaybeT do
  -- sort the decks here so they are ordered by position, this ensures that if
  -- decks need to be accomodated when broken out, the decks in the top left
  -- corner will maintain their position and the others will be accomodated.
  let deckList = List.sortBy (compare `on` toCoords) $ Map.toList decks
  let coord = deckId × cardId
  let level' = DL.succ deck.level
  offset ← MaybeT $ H.gets (Map.lookup oldId ∘ _.decks)
  lift do
    H.modify \s →
      s { decks = foldl (reinsert offset) (Map.delete oldId s.decks) deckList }
    for_ deckList \(deckId × (_ × deck)) → do
      let deck' = deck { parent = Just coord }
      queryDeck deckId $ H.action $ DCQ.SetModel deckId deck' level'
      queryDeck deckId $ H.action $ DCQ.Save Nothing
  where
  reinsert offset acc (deckId × (pos × deck)) =
    Map.insert deckId (updatePos (Map.toList acc) offset pos) acc
  updatePos currentDecks offset pos =
    reallyAccomodateDeck currentDecks $
      pos
        { x = pos.x + offset.x + 1.0
        , y = pos.y + offset.y + 1.0
        }
  toCoords (_ × (pos × _)) = Tuple pos.x pos.y

mirrorDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
mirrorDeck opts oldId = do
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
        putDeck opts.deck.path newId newDeck opts.deck.wiring.decks >>= case _ of
          Left _ →
            -- TODO: do something to notify the user saving failed
            pure unit
          Right _ → do
            let modelCards'' = modelCards'.init <> map (lmap (const newId)) modelCards'.rest
            queryDeck oldId $ H.action $ DCQ.SetModelCards modelCards''
            insertNewDeck DM.emptyDeck { mirror = DCS.coordModelToCoord <$> modelCards'' }
  where
  insertNewDeck deck = do
    st ← H.get
    for_ (Map.lookup oldId st.decks) \deckPos →
      addDeckAt opts deck $
        reallyAccomodateDeck (Map.toList st.decks) deckPos

groupDecks ∷ CardOptions → DeckId → DeckId → DraftboardDSL Unit
groupDecks { cardId, deckId, deck } deckFrom deckTo = do
  st ← H.get
  for_ (Map.lookup deckFrom st.decks) \rectFrom →
  for_ (Map.lookup deckTo st.decks) \rectTo →
    queryDeck deckTo (H.request DCQ.GetModelCards) >>= case _ of
      Just [ deckId' × { cardId, model: Card.Draftboard { decks } } ] → void do
        let
          rectFrom' =
            reallyAccomodateDeck (Map.toList decks) $
              rectFrom { x = 1.0, y = 1.0 }
          decks' = Map.insert deckFrom rectFrom' decks
          card = { cardId, model: Card.Draftboard { decks: decks' } }
        unsafeUpdateCachedDraftboard deck.wiring deckId' card
        H.modify \s → s { decks = Map.delete deckFrom s.decks }
        queryDeck deckTo
          $ H.action
          $ DCQ.SetModelCards [ deckId' × card ]

      _ → do
        let
          rectTo' = rectTo { x = 1.0, y = 1.0 }
          rectFrom' = rectFrom { x = 1.0, y = rectTo.height + 2.0 }
          newDeck = DM.emptyDeck
            { parent = Just (deckId × cardId)
            , cards = pure
              { cardId: CID.CardId 0
              , model: Card.Draftboard
                { decks: Map.fromFoldable
                  [ deckTo × rectTo'
                  , deckFrom × rectFrom'
                  ]
                }
              }
            }
        newId ← H.fromEff freshDeckId
        putDeck deck.path newId newDeck deck.wiring.decks >>= case _ of
          Left err → do
            -- TODO: do something to notify the user saving failed
            pure unit
          Right _ → void do
            H.modify \s → s
              { decks
                  = Map.insert newId rectTo
                  $ Map.delete deckFrom
                  $ Map.delete deckTo
                  $ s.decks
              }
            queryDeck newId
              $ H.action
              $ DCQ.Load deck.path newId (DL.succ deck.level)

queryDeck ∷ ∀ a. DeckId → DCQ.Query a → DraftboardDSL (Maybe a)
queryDeck deckId = H.query deckId ∘ right
