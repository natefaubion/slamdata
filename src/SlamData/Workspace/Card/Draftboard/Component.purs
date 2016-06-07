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

import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)

import Data.Array as Array
import Data.Function (on)
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import CSS as CSS

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState, opaqueQuery, peekOpaqueQuery, OpaqueQuery)
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
import SlamData.Quasar.Data (save, load, delete) as Quasar
import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..), QueryP, QueryC)
import SlamData.Workspace.Card.Draftboard.Component.State (State, DeckPosition, initialState, encode, decode)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Common.EvalQuery as Ceq
import SlamData.Workspace.Card.Component as Cp
import SlamData.Workspace.Deck.Common (wrappedDeck, defaultPosition)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId(..), deckIdToString)
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Model as WS

import Utils.CSS (zIndex)
import Utils.DOM (elementEq, scrollTop, scrollLeft, getOffsetClientRect)
import Utils.Path (DirPath, FilePath)

type DraftboardDSL = H.ParentDSL State DCS.StateP QueryC DCQ.QueryP Slam DeckId

type DraftboardHTML = H.ParentHTML DCS.StateP QueryC DCQ.QueryP Slam DeckId

draftboardComponent ∷ CardOptions → Cp.CardComponent
draftboardComponent opts = Cp.makeCardComponent
  { cardType: Ct.Draftboard
  , component: H.parentComponent
      { render: render opts
      , eval: coproduct evalCard (evalBoard opts)
      , peek: Just (peek opts)
      }
  , initialState: H.parentState initialState
  , _State: Cp._DraftboardState
  , _Query: Cp.makeQueryPrism' Cp._DraftboardQuery
  }

render ∷ CardOptions → State → DraftboardHTML
render opts state =
  HH.div
    [ HP.classes [ RC.gridPattern ]
    , HC.style bgSize
    , HP.ref (right ∘ H.action ∘ SetElement)
    , HE.onMouseDown (pure ∘ Just ∘ right ∘ H.action ∘ AddDeck)
    ]
    $ map renderDeck (listToArray $ Map.toList state.decks)

  where

  renderDeck (Tuple deckId rect) =
    HH.div
      [ HP.key $ deckIdToString deckId
      , HC.style $
          case state.moving of
            Just (Tuple deckId' rect') | deckId == deckId' → zIndex 1 *> cssPos rect'
            _ → cssPos rect
      ]
      [ HH.slot deckId $ mkDeckComponent deckId ]

  mkDeckComponent id _ =
    { component: opts.deckComponent
    , initialState: opaqueState $ DCS.initialDeck
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

evalCard ∷ Natural Ceq.CardEvalQuery DraftboardDSL
evalCard (Ceq.EvalCard input k) = pure $ k { output: Nothing, messages: [] }
evalCard (Ceq.SetupCard info next) = pure next
evalCard (Ceq.NotifyRunCard next) = pure next
evalCard (Ceq.NotifyStopCard next) = pure next
evalCard (Ceq.SetCanceler canceler next) = pure next
evalCard (Ceq.SetDimensions dims next) = pure next
evalCard (Ceq.Save k) = map (k ∘ encode) H.get
evalCard (Ceq.Load json next) = do
  for_ (decode json) \model → do
    H.modify _ { decks = model.decks }
    loadDecks
  pure next

evalBoard ∷ CardOptions → Natural Query DraftboardDSL
evalBoard _ (Grabbing deckId ev next) = do
  case ev of
    Drag.Move _ d → do
      H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
        let newRect = clampDeck rect
              { x = rect.x + (pxToGrid d.offsetX)
              , y = rect.y + (pxToGrid d.offsetY)
              }
        H.modify _ { moving = Just (Tuple deckId newRect) }
    Drag.Done _ →
      stopDragging
  pure next
evalBoard _ (Resizing deckId ev next) = do
  case ev of
    Drag.Move _ d → do
      H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
        let newRect = clampDeck rect
              { width = rect.width + (pxToGrid d.offsetX)
              , height = rect.height + (pxToGrid d.offsetY)
              }
        H.modify _ { moving = Just (Tuple deckId newRect) }
    Drag.Done _ →
      stopDragging
  pure next
evalBoard _ (SetElement el next) = do
  H.modify _ { canvas = el }
  pure next
evalBoard opts (AddDeck e next) = do
  let e' = mouseEventToPageEvent e
  H.gets _.canvas >>= traverse_ \el →
    H.fromEff (elementEq el e'.target) >>= \same →
      when same do
        rect ← H.fromEff $ getOffsetClientRect el
        scroll ← { top: _, left: _ } <$> H.fromEff (scrollTop el) <*> H.fromEff (scrollLeft el)
        addDeck opts
          { x: floor $ pxToGrid $ e'.pageX - rect.left + scroll.left
          , y: floor $ pxToGrid $ e'.pageY - rect.top + scroll.top
          }
  pure next
evalBoard opts (LoadDeck deckId next) = do
  for_ opts.path \path →
    queryDeck deckId
      $ H.action
      $ DCQ.Load path deckId DCQ.Nested
  pure next

peek ∷ ∀ a. CardOptions → H.ChildF DeckId (OpaqueQuery DCQ.Query) a → DraftboardDSL Unit
peek opts (H.ChildF deckId q) = flip peekOpaqueQuery q
  case _ of
    DCQ.GrabDeck ev _ → startDragging deckId ev Grabbing
    DCQ.ResizeDeck ev _ → startDragging deckId ev Resizing
    DCQ.DoAction DCQ.DeleteDeck _ → deleteDeck opts deckId
    DCQ.DoAction DCQ.Wrap _ → wrapDeck opts deckId
    _ → pure unit

  where
  startDragging deckId ev tag =
    H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
      H.modify _ { moving = Just (Tuple deckId rect) }
      void
        $ Drag.subscribe' ev
        $ right ∘ H.action ∘ tag deckId

pxToGrid ∷ Number → Number
pxToGrid = (_ / Config.gridPx)

gridToPx ∷ Number → Number
gridToPx = (_ * Config.gridPx)

stopDragging ∷ DraftboardDSL Unit
stopDragging = do
  st ← H.get
  for_ st.moving \(Tuple deckId rect) → do
    let rect' = roundDeck rect
        decks = List.filter ((deckId /= _) ∘ fst) $ Map.toList st.decks
    when (List.null $ overlapping rect' decks) do
      H.modify \s → s { decks = Map.insert deckId rect' s.decks }
  H.modify _ { moving = Nothing }

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
  → List.List (Tuple DeckId DeckPosition)
  → List.List (Tuple DeckId DeckPosition)
overlapping a = List.filter go
  where
  go (Tuple _ b) =
    not $ a.x + a.width <= b.x
       || b.x + b.width <= a.x
       || a.y + a.height <= b.y
       || b.y + b.height <= a.y

accomodateDeck
  ∷ List.List (Tuple DeckId DeckPosition)
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

loadDecks ∷ DraftboardDSL Unit
loadDecks =
  H.gets (Map.keys ∘ _.decks) >>=
    traverse_ (raise' ∘ right ∘ H.action ∘ LoadDeck)

addDeck ∷ CardOptions → { x ∷ Number, y ∷ Number } → DraftboardDSL Unit
addDeck opts coords = do
  decks ← Map.toList <$> H.gets _.decks
  let deckPos = clampDeck $ defaultPosition
        { x = coords.x - 10.0
        , y = coords.y
        }
  for_ (accomodateDeck decks coords deckPos) \deckPos' →
  for_ opts.deckId \parentId →
  for_ opts.path \path → do
    deckId ← saveDeck path $ DM.emptyDeck { parent = Just (Tuple parentId opts.cardId) }
    case deckId of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right deckId' → void do
        H.modify \s → s { decks = Map.insert deckId' deckPos' s.decks }
        queryDeck deckId'
          $ H.action
          $ DCQ.Load path deckId' DCQ.Nested

saveDeck ∷ DirPath → DM.Deck → DraftboardDSL (Either Exn.Error DeckId)
saveDeck path model = runExceptT do
  i ← ExceptT $ map DeckId <$> WS.freshId (path </> Pathy.file "index")
  ExceptT $ Quasar.save (deckIndex path i) $ DM.encode model
  pure i

deleteDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
deleteDeck opts deckId =
  for_ opts.path \path → do
    let deleteId i = Quasar.delete $ Left $ path </> Pathy.dir (deckIdToString i)
    res ← H.fromAff $ runExceptT do
      children ← ExceptT $ deckGraph path deckId
      withExceptT Exn.message
        $ ExceptT
        $ map sequence
        $ runPar
        $ traverse (Par ∘ deleteId)
        $ Array.cons deckId children

    case res of
      Left err →
        -- TODO: do something to notify the user deleting failed
        pure unit
      Right _ →
        H.modify \s → s { decks = Map.delete deckId s.decks }

wrapDeck ∷ CardOptions → DeckId → DraftboardDSL Unit
wrapDeck opts oldId = do
  H.gets (Map.lookup oldId ∘ _.decks) >>= traverse_ \deckPos →
  for_ opts.deckId \parentId →
  for_ opts.path \path → do
    let deckPos' = deckPos { x = 1.0, y = 1.0 }
        newDeck = (wrappedDeck deckPos' oldId) { parent = Just (Tuple parentId opts.cardId) }
    deckId ← saveDeck path newDeck
    case deckId of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right newId → void do
        traverse_ (queryDeck oldId ∘ H.action)
          [ DCQ.SetParent (Tuple newId (CID.CardId 0))
          , DCQ.Save
          ]
        H.modify \s → s
          { decks
              = Map.insert newId deckPos
              $ Map.delete oldId
              $ s.decks
          }
        queryDeck newId
          $ H.action
          $ DCQ.Load path newId DCQ.Nested

deckIndex ∷ DirPath → DeckId → FilePath
deckIndex path deckId =
  path </> Pathy.dir (deckIdToString deckId) </> Pathy.file "index"

deckGraph
  ∷ DirPath
  → DeckId
  → Slam (Either String (Array DeckId))
deckGraph path deckId = runExceptT do
  json ← ExceptT $ Quasar.load $ deckIndex path deckId
  deck ← ExceptT $ pure $ DM.decode json
  boards ← ExceptT
    $ pure
    $ sequence
    $ map (decode ∘ _.state)
    $ Array.filter (\c → c.cardType == Ct.Draftboard) deck.cards

  let children = join $ map (listToArray ∘ Map.keys ∘ _.decks) boards

  ExceptT $ map (children <> _) <$> transDecks children

  where
  transDecks ids = map (map join ∘ sequence) $ runPar $ traverse (Par ∘ deckGraph path) ids

queryDeck ∷ ∀ a. DeckId → DCQ.Query a → DraftboardDSL (Maybe a)
queryDeck deckId = H.query deckId <<< opaqueQuery

-- This can be removed once Array gets fromFoldable
listToArray ∷ ∀ a. List.List a → Array a
listToArray = foldl Array.snoc []
