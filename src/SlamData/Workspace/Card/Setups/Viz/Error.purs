{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Viz.Error where

import SlamData.Prelude

import Data.Foldable (intercalate)
import Data.List as L
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import Utils (throwVariantError)

type Error =
  { missingProjections ∷ L.List Pr.Projection
  , vizType ∷ VT.VizType
  }

showError ∷ Error → String
showError { missingProjections, vizType } =
  "(MissingAxesError { missingProjections: "
  ⊕ (intercalate ", " (map Pr.print missingProjections))
  ⊕ ", vizType: "
  ⊕ VT.print vizType
  ⊕ "}"

throw
  ∷ ∀ v m a
  . MonadThrow (Variant (setupViz ∷ Error | v)) m
  ⇒ Error
  → m a
throw = throwVariantError (SProxy ∷ SProxy "setupViz")
