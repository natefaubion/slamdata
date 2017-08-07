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

module SlamData.FileSystem.Dialog.Mount.Component.State where

import SlamData.Prelude

import Data.Lens (Prism', is, preview, prism')
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as String
import Quasar.Mount as QM
import Quasar.Mount.Type as QMT
import Quasar.Types (AnyPath)
import Utils.Path (DirPath)

data Input
  = New { parent ∷ DirPath }
  | Edit { path ∷ AnyPath, mount ∷ QM.MountConfig, fromRoot ∷ Boolean }
  | Root

_New ∷ Prism' Input { parent ∷ DirPath }
_New = prism' New case _ of
  New a → Just a
  _ → Nothing

_Edit ∷ Prism' Input { path ∷ AnyPath, mount ∷ QM.MountConfig, fromRoot ∷ Boolean }
_Edit = prism' Edit case _ of
  Edit a → Just a
  _ → Nothing

_Root ∷ Prism' Input Unit
_Root = prism' (const Root) case _ of
  Root → Just unit
  _ → Nothing

isNew ∷ Input → Boolean
isNew = case _ of
  New _ → true
  _ → false

matchSchemeConfig ∷ Input → QMT.MountType → Maybe QM.MountConfig
matchSchemeConfig = case _, _ of
  Edit { mount }, scheme | scheme == QM.getType mount → Just mount
  _, _ → Nothing

data Status = Idle | Saving | Unmounting

derive instance eqStatus ∷ Eq Status

type State =
  { input ∷ Input
  , name ∷ Maybe String
  , scheme ∷ Maybe QMT.MountType
  , pathValue ∷ Either (Maybe String) AnyPath
  , configValue ∷ Either (Maybe String) QM.MountConfig
  , error ∷ Maybe String
  , status ∷ Status
  }

initialState ∷ Input → State
initialState input =
  let
    maybeEditInput = preview _Edit input
  in
    { input
    , name: if is _New input then Just "" else Nothing
    , scheme: QM.getType ∘ _.mount <$> maybeEditInput
    , pathValue: note Nothing (_.path <$> maybeEditInput)
    , configValue: note Nothing (_.mount <$> maybeEditInput)
    , error: Nothing
    , status: Idle
    }

validate ∷ State → State
validate st = st { pathValue = validatePath st }

validatePath ∷ State → Either (Maybe String) AnyPath
validatePath st@{ input, name, scheme: Just scheme } =
  case input of
    Root → Right (Left P.rootDir)
    Edit { path } → Right path
    New { parent } → do
      let name' = fromMaybe "" st.name
      when (String.trim name' == "") $
        Left (Just "Please enter a mount name")
      when (String.contains (String.Pattern "/") name') $
        Left (Just "Mount names cannot contain slashes")
      case scheme of
        QMT.View → Right $ Right (parent </> P.file name')
        _ → Right $ Left (parent </> P.dir name')
validatePath _ = Left Nothing
