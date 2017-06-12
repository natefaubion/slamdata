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

module SlamData.Workspace.Routing
  ( routing
  , Routes(..)
  , mkWorkspaceHash
  , mkWorkspaceURL
  , varMapsForURL
  , getPath
  , getURLVarMaps
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Foldable as F
import Data.List as L
import Data.Map as Map
import Data.Maybe as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String.Regex as R
import Data.String.Regex.Flags as RXF
import Data.StrMap as SM

import Global (encodeURIComponent)

import Routing.Match (Match)
import Routing.Match (eitherMatch, list) as Match
import Routing.Match.Class (lit, str, param) as Match

import SlamData.Config as Config
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Deck.DeckId as DID

import Utils.Path as UP

data Routes
  = WorkspaceRoute
      UP.DirPath
      (L.List DID.DeckId)
      WA.Action
      (SM.StrMap VM.URLVarMap)

getPath ∷ Routes → UP.DirPath
getPath (WorkspaceRoute p _ _ _) = p

getURLVarMaps ∷ Routes → SM.StrMap VM.URLVarMap
getURLVarMaps (WorkspaceRoute _ _ _ vm) = vm

routing ∷ Match Routes
routing
  = WorkspaceRoute <$> workspace <*> deckIds <*> action <*> optionalVarMap

  where
  optionalVarMap ∷ Match (SM.StrMap VM.URLVarMap)
  optionalVarMap = varMap <|> pure SM.empty

  varMap ∷ Match (SM.StrMap VM.URLVarMap)
  varMap = Match.eitherMatch $ decodeVarMaps <$> Match.param "vars"

  oneSlash ∷ Match Unit
  oneSlash = Match.lit ""

  explored ∷ Match UP.FilePath
  explored = Match.eitherMatch $ mkResource <$> Match.list Match.str

  mkResource ∷ L.List String → Either String UP.FilePath
  mkResource parts =
    case L.last parts of
      Just filename | filename /= "" →
        let dirParts = unsafePartial M.fromJust (L.init parts)
            filePart = P.file filename
            path = foldr (\part acc → P.dir part </> acc) filePart dirParts
        in Right $ P.rootDir </> path
      _ → Left "Expected non-empty explore path"

  workspace ∷ Match UP.DirPath
  workspace = workspaceFromParts <$> partsAndName

  workspaceFromParts ∷ Tuple (L.List String) String → UP.DirPath
  workspaceFromParts (Tuple ps nm) =
    foldl (</>) P.rootDir (map P.dir ps) </> P.dir nm

  partsAndName ∷ Match (Tuple (L.List String) String)
  partsAndName = Tuple <$> (oneSlash *> Match.list notName) <*> name

  name ∷ Match String
  name = Match.eitherMatch $ map workspaceName Match.str

  notName ∷ Match String
  notName = Match.eitherMatch $ map pathPart Match.str

  workspaceName ∷ String → Either String String
  workspaceName input
    | checkExtension input = Right input
    | otherwise = Left input

  pathPart ∷ String → Either String String
  pathPart input
    | input == "" || checkExtension input = Left "incorrect path part"
    | otherwise = Right input

  extensionRegex ∷ R.Regex
  extensionRegex =
    unsafePartial fromRight $
      R.regex ("\\." <> Config.workspaceExtension <> "$") RXF.noFlags

  checkExtension ∷ String → Boolean
  checkExtension = R.test extensionRegex

  deckIds ∷ Match (L.List DID.DeckId)
  deckIds = L.reverse <$> Match.list (Match.eitherMatch (DID.fromString' <$> Match.str))

  action ∷ Match WA.Action
  action
      = (WA.Exploring <$> (Match.lit "exploring" *> explored))
    <|> (Match.eitherMatch $ map WA.parseAction Match.str)
    <|> pure (WA.Load ReadOnly)

-- TODO: it would be nice if `purescript-routing` had a way to render a route
-- from a matcher, so that we could do away with the following brittle functions.

mkWorkspaceURL
  ∷ UP.DirPath    -- workspace path
  → WA.Action     -- workspace action
  → String
mkWorkspaceURL path action =
  Config.workspaceUrl
    <> mkWorkspaceHash path action SM.empty

mkWorkspaceHash
  ∷ UP.DirPath -- workspace path
  → WA.Action -- workspace action
  → SM.StrMap VM.URLVarMap -- varmaps introduced by variables cards in the workspace
  → String
mkWorkspaceHash path action varMap =
  "#"
    <> UP.encodeURIPath (P.printPath path)
    <> WA.printAction action
    <> maybe "" ("/" <> _) (renderVarMapQueryString varMap)

varMapsForURL ∷ Map.Map CID.CardId VM.VarMap → SM.StrMap VM.URLVarMap
varMapsForURL =
  SM.fromFoldable
  ∘ map (bimap CID.toString VM.toURLVarMap)
  ∘ asList
  ∘ Map.toUnfoldable

decodeVarMaps ∷ String → Either String (SM.StrMap VM.URLVarMap)
decodeVarMaps = J.jsonParser >=> J.decodeJson

renderVarMapQueryString ∷ SM.StrMap VM.URLVarMap → Maybe String
renderVarMapQueryString varMaps
  | F.all SM.isEmpty varMaps = Nothing
  | otherwise =
      let json = J.encodeJson varMaps
      in Just $ "?vars=" <> encodeURIComponent (show json)
