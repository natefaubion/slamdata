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

module SlamData.Workspace.Card.DownloadOptions.Component.State where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec ((<~<), (>~>))
import Data.Codec as C
import Data.Codec.Argonaut.Compat as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Lens (Lens', lens)
import Quasar.Data.CSV as CSV
import SlamData.Download.Model as D
import Utils.Path as PU

type State =
  { compress ∷ Boolean
  , options ∷ D.DownloadOptions
  , targetName ∷ Maybe String
  , source ∷ Maybe PU.AnyFilePath
  }

eqState ∷ State → State → Boolean
eqState s1 s2 =
  s1.compress ≡ s2.compress
    && s1.options ≡ s2.options
    && s1.source ≡ s2.source
    && s1.targetName ≡ s2.targetName

initialState ∷ State
initialState =
  { compress: false
  , options: Left CSV.defaultOptions
  , targetName: Nothing
  , source: Nothing
  }

_options ∷ ∀ a r. Lens' {options ∷ a|r} a
_options = lens (_.options) (_{options = _})

codec ∷ CA.JsonCodec State
codec = migrationCodec >~> CA.object "Download options model"
  (CA.record
    # CA.recordProp (SProxy ∷ SProxy "compress") CA.boolean
    # CA.recordProp (SProxy ∷ SProxy "options") (CA.either D.codecCSVOptions D.codecJSONOptions)
    # CA.recordProp (SProxy ∷ SProxy "targetName") (CA.maybe CA.string)
    # CA.recordProp (SProxy ∷ SProxy "source") (CA.maybe codecFilePath))
  where
    -- added in 4.something
    migrationCodec ∷ CA.JsonCodec J.Json
    migrationCodec = CAM.addDefaultField "targetName" nothingJson
      where
      nothingJson ∷ J.Json
      nothingJson = CA.encode (CA.maybe CA.json) Nothing

codecFilePath ∷ CA.JsonCodec PU.AnyFilePath
codecFilePath = C.basicCodec dec enc <~< CA.string
  where
  dec s = note (CA.UnexpectedValue (J.fromString s)) (PU.parseAnyFilePath s)
  enc = PU.printAnyFilePath
