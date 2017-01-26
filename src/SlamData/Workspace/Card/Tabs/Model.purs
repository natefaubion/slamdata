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

module SlamData.Workspace.Card.Tabs.Model where

import SlamData.Prelude
import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen (Gen)
import SlamData.Workspace.Deck.DeckId (DeckId)

type Model =
  { tabs ∷ Array DeckId
  }

initialModel ∷ Model
initialModel =
  { tabs: []
  }

eqModel ∷ Model → Model → Boolean
eqModel a b = a.tabs ≡ b.tabs

genModel ∷ Gen Model
genModel = do
  tabs ← arbitrary
  pure { tabs }

encode ∷ Model → Json
encode m =
  "tabs" := m.tabs
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode json = do
  obj ← decodeJson json
  tabs ← obj .? "tabs"
  pure { tabs }
