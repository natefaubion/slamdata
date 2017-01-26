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

module SlamData.Workspace.Card.DownloadOptions.Component.State
  ( State
  , initialState
  , eqState
  , _compress
  , _options
  , _source
  , _levelOfDetails
  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Lens (Lens', lens)
import Data.Path.Pathy as Pathy
import SlamData.Download.Model as D
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Path as PU

type State =
  { compress ∷ Boolean
  , options ∷ Either D.CSVOptions D.JSONOptions
  , targetName ∷ Maybe String
  , source ∷ Maybe PU.FilePath
  , levelOfDetails ∷ LevelOfDetails
  }

eqState ∷ State → State → Boolean
eqState s1 s2 =
  s1.compress ≡ s2.compress
    && s1.options ≡ s2.options
    && s1.source ≡ s2.source
    && s1.levelOfDetails ≡ s2.levelOfDetails
    && s1.targetName ≡ s2.targetName

initialState ∷ Maybe String → State
initialState targetName =
  { compress: false
  , options: Left D.initialCSVOptions
  , targetName
  , source: Nothing
  , levelOfDetails: High
  }

_compress ∷ ∀ a r. Lens' {compress ∷ a|r} a
_compress = lens (_.compress) (_{compress = _ })

_options ∷ ∀ a r. Lens' {options ∷ a|r} a
_options = lens (_.options) (_{options = _})

_source ∷ ∀ a r. Lens' {source ∷ a|r} a
_source = lens (_.source) (_{source = _})

_levelOfDetails ∷ ∀ a r. Lens' {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

encode :: State -> Json
encode s
   = "compress" := s.compress
  ~> "options" := s.options
  ~> "targetName" := s.targetName
  ~> "source" := (Pathy.printPath <$> s.source)
  ~> jsonEmptyObject

decode :: Json → Either String State
decode = decodeJson >=> \obj → do
  compress ← obj .? "compress"
  options ← obj .? "options"
  targetName ← obj .? "targetName" <|> pure Nothing
  source ← traverse parsePath =<< obj .? "source"
  let levelOfDetails = High
  pure { compress, options, targetName, source, levelOfDetails }

parsePath ∷ String → Either String PU.FilePath
parsePath =
  maybe (Left "could not parse source file path") Right ∘ PU.parseFilePath

