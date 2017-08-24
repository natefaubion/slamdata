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

module SlamData.LocalStorage.Keys where

import SlamData.Prelude

import Data.Date (Date)
import Data.Map (Map)
import OIDC.Crypt.Types as OIDCT
import Quasar.Advanced.Types as QAT
import SlamData.LocalStorage.Class as LS
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Eval.Card (AnyCardModel)
import SlamData.Workspace.Eval.Deck as Deck

--- Consumer card persistence

cardsLocalStorageKey ∷ Deck.Id → LS.Key (Map CID.CardId AnyCardModel)
cardsLocalStorageKey =
  LS.Key ∘ ("sd-cards-" ⊕ _) ∘ DID.toString

--- Hint dismissals

dismissedFocusDeckHintKey ∷ LS.Key Boolean
dismissedFocusDeckHintKey = LS.Key "dismissedFocusDeckHint"

dismissedFocusDeckFrameHintKey ∷ LS.Key Boolean
dismissedFocusDeckFrameHintKey = LS.Key "dismissedFocusDeckFrameHint"

dismissedAccessNextActionCardHintKey ∷ LS.Key Boolean
dismissedAccessNextActionCardHintKey = LS.Key "dismissedAccessNextActionCardHint"

dismissedAddCardHintKey ∷ LS.Key Boolean
dismissedAddCardHintKey = LS.Key "dismissedAddCardHint"

dismissedMountHintKey ∷ LS.Key Boolean
dismissedMountHintKey = LS.Key "dismissed-mount-guide"

-- Video dismissals

dismissedIntroVideoKey ∷ LS.Key Boolean
dismissedIntroVideoKey = LS.Key "dismissed-intro-video"

-- Guide dismissals

dismissedCardGuideKey ∷ LS.Key Boolean
dismissedCardGuideKey = LS.Key "dismissedCardGuide"

dismissedFlipGuideKey ∷ LS.Key Boolean
dismissedFlipGuideKey = LS.Key "dismissedFlipGuide"

-- Authentication

idTokenLocalStorageKey ∷ String -> LS.Key (Either String OIDCT.IdToken)
idTokenLocalStorageKey = LS.Key ∘ ("sd-auth-id-token-" ⊕ _)

keyStringLocalStorageKey ∷ String -> LS.Key OIDCT.KeyString
keyStringLocalStorageKey = LS.Key ∘ ("sd-auth-csrf-" ⊕ _)

nonceLocalStorageKey ∷ String -> LS.Key OIDCT.UnhashedNonce
nonceLocalStorageKey = LS.Key ∘ ("sd-auth-replay-" ⊕ _)

providerLocalStorageKey ∷ String -> LS.Key QAT.Provider
providerLocalStorageKey = LS.Key ∘ ("sd-auth-provider-" ⊕ _)

signedOutBefore ∷ LS.Key Boolean
signedOutBefore = LS.Key "sd-auth-signed-out-before"

-- Autocomplete

autoCompletePathsKey ∷ LS.Key (Array String)
autoCompletePathsKey = LS.Key "sd-autocomplete-paths"

-- AdminUI

adminUIDefaultTheme ∷ LS.Key String
adminUIDefaultTheme = LS.Key "sd-admin-ui-default-theme"

adminUIHomeDirectory ∷ LS.Key String
adminUIHomeDirectory = LS.Key "sd-admin-ui-home-directory"

-- Licensing

licenseDaysRemainingPresentedBefore ∷ LS.Key Date
licenseDaysRemainingPresentedBefore = LS.Key "sd-license-days-remaining-presented-before"
