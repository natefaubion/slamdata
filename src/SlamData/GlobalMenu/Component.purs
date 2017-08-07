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

module SlamData.GlobalMenu.Component
  ( component
  , Query(..)
  , MenuOpen(..)
  , Message(..)
  , AuthenticateOrPresentHelp(..)
  , State
  , module SlamData.GlobalMenu.Bus
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff as Eff
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref as Ref
import Control.UI.Browser as Browser
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import OIDC.Crypt as Crypt
import Quasar.Advanced.Types (ProviderR)
import SlamData.AuthenticationMode as AuthenticationMode
import SlamData.GlobalError (GlobalError)
import SlamData.GlobalError as GlobalError
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.AdminUI.Types as AdminUI
import SlamData.Monad (Slam)
import SlamData.Quasar as Api
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Auth.Authentication (AuthenticationError, toNotificationOptions, removeAuthenticationDetails)
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.Eval.Card as EvalCard
import SlamData.Workspace.Eval.Persistence as Persistence
import Utils.DOM as DOM

data AuthenticateOrPresentHelp

data Query a
  = Authenticate (Maybe ProviderR) a
  | DismissSubmenu a
  | HandleGlobalError GlobalError a
  | Init a
  | PresentAttribution a
  | SignOut a
  | StopPropagation DOM.Event (Query a)
  | ToggleMenu MenuOpen a
  | AdminUIClicked a

data MenuOpen
 = SignInMenu
 | HelpMenu

derive instance eqMenuOpen ∷ Eq MenuOpen

type State =
  { email ∷ Maybe String
  , loggedIn ∷ Boolean
  , menuOpen ∷ Maybe MenuOpen
  , providers ∷ Maybe (Array ProviderR)
  }

type ChildQuery = AdminUI.Query ⨁ Const Void
type ChildSlot = Unit ⊹ Void

data Message
  = PresentAttributionsDialog
  | OpenAdminUI

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { initialState: \_ →
      { email: Nothing
      , loggedIn: false
      , menuOpen: Nothing
      , providers: Nothing
      }
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  let
    stopProp action e =
      Just $ StopPropagation (DOM.toEvent e) $ H.action action

    container ∷ HH.ClassName
    container = HH.ClassName "menu-container"

    active ∷ HH.ClassName
    active = HH.ClassName "active"

    submenuItem =
      HP.classes $ HH.ClassName <$> [ "menu-item", "submenu-item" ]

    submenu = HH.ul [ HP.class_ $ HH.ClassName "submenu" ]

    -- wraps stiff in `li > button`
    sitem provider =
      HH.li [ submenuItem ]
        [ HH.button
            [ HP.classes $ HH.ClassName <$> [ "menu-item-button", "submenu-item-button" ]
            , HE.onClick $ stopProp $ Authenticate $ pure provider
            ]
            [ HH.text $ "Sign in with " <> provider.displayName ]
        ]

    -- wraps stuff in `li > a`
    hitem attrs children =
      HH.li [ submenuItem ]
        [ HH.a
          ([ HP.classes $ HH.ClassName <$> [ "menu-item-link", "submenu-item-link" ] ] <> attrs)
          children
        ]

    userInfo =
      flip foldMap state.email \e →
        [ HH.div
          [ HP.class_ $ HH.ClassName "user-info" ]
          [ HH.span_ [ HH.text e ] ]
        ]

    adminMenu =
      [ HH.div
          [ HP.classes [ container, HH.ClassName "sign-in-menu-container" ] ] $
          [ HH.button
              [ HP.class_ $ HH.ClassName "sign-in-menu-button "
              , HE.onClick $ stopProp $ AdminUIClicked
              ]
              [ I.settingsCog, HH.text "Settings" ]
          ]
      ]

    signInMenu =
      case state.providers, state.email of
        Just ps, _ →
          [ HH.div
            [ HP.classes $ [ container, HH.ClassName "sign-in-menu-container" ] ] $
            [ HH.button
                [ HP.classes $ [ HH.ClassName "sign-in-menu-button " ]
                  <> if state.menuOpen == Just SignInMenu then [ active ] else []
                , HE.onClick $ stopProp $ ToggleMenu SignInMenu
                ]
                [ I.lockSm, HH.text "Sign in" ]
            ] <>
              if state.menuOpen == Just SignInMenu then
                [ submenu $ sitem <$> ps ]
              else
                []
          ]
        _, Just _ →
          [ HH.div
              [ HP.classes [ container, HH.ClassName "sign-in-menu-container" ] ] $
              [ HH.button
                  [ HP.class_ $ HH.ClassName "sign-in-menu-button "
                  , HE.onClick $ stopProp $ SignOut -- Authenticate Nothing
                  ]
                  [ I.unlockSm, HH.text "Sign out" ]
              ]
          ]
        _, _ →
          []

    helpMenu =
      HH.div
        [ HP.classes $ [ container, HH.ClassName "help-menu-container" ] ] $
        [ HH.button
            [ HP.classes $ [ HH.ClassName "help-menu-button" ]
                <> if state.menuOpen == Just HelpMenu then [ active ] else []
            , HE.onClick $ stopProp $ ToggleMenu HelpMenu
            ]
            [ I.helpSm, HH.text "Help" ]
        ] <>
          if state.menuOpen == Just HelpMenu then
            [ submenu
              [ hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/users-guide.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "User guide" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/administration-guide.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "Administrator guide" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/developers-guide.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "Developer guide" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/helpful-tips.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "Helpful tips" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/sql-squared-reference.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "SQL² reference" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/slamdown-reference.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "SlamDown reference" ]
              , hitem
                  [ HP.href "http://docs.slamdata.com/en/v4.2/troubleshooting-faq.html"
                  , HP.target "_blank"
                  ]
                  [ HH.text "Troubleshooting FAQ" ]
              , HH.li [ submenuItem ]
                  [ HH.button
                      [ HP.classes $ HH.ClassName <$>
                          [ "menu-item-button", "submenu-item-button" ]
                      , HE.onClick $ stopProp $ PresentAttribution
                      ]
                      [ HH.text $ "Attribution" ]
                  ]
              ]
            ]
          else
            []

  in
    HH.div
      [ HP.classes $ [ HH.ClassName "sd-global-menu" ] ] $
      userInfo <> signInMenu <> adminMenu <> [ helpMenu ]

eval ∷ Query ~> DSL
eval = case _ of
  Authenticate providerR next → do
    H.modify _{ menuOpen = Nothing }
    authenticate providerR
    pure next
  DismissSubmenu next → do
    H.modify _{ menuOpen = Nothing }
    pure next
  HandleGlobalError error next →
    case error of
      GlobalError.Unauthorized _ → update $> next
      _ → pure next
  Init next → do
    { bus } ← Wiring.expose
    H.subscribe $ busEventSource (flip HandleGlobalError ES.Listening) bus.globalError
    update
    pure next
  PresentAttribution next → do
    H.modify _{ menuOpen = Nothing }
    H.raise PresentAttributionsDialog
    pure next
  SignOut next → do
    H.modify _{ email = Nothing }
    eval $ Authenticate Nothing next
  StopPropagation e q → do
    H.liftEff $ DOM.stopPropagation e
    eval q
  ToggleMenu which next → do
    menuOpen ← H.gets _.menuOpen
    let
      m = case menuOpen, which of
        Just HelpMenu, HelpMenu → Nothing
        Just SignInMenu, SignInMenu → Nothing
        _, _ → Just which
    H.modify _{ menuOpen = m }
    pure next
  AdminUIClicked next → do
    H.raise OpenAdminUI
    pure next

update ∷ DSL Unit
update = do
  maybeIdToken ← H.lift Auth.getIdToken
  case maybeIdToken of
    Just idToken → do
      either
        (const retrieveProvidersAndUpdateMenu)
        putEmailToMenu
        (Eff.runPure $ Exception.try $ Crypt.readPayload idToken)
    Nothing →
      retrieveProvidersAndUpdateMenu
  where
  putEmailToMenu ∷ Crypt.Payload → DSL Unit
  putEmailToMenu payload =
    H.modify _{ loggedIn = true, email = map unwrap $ Crypt.pluckEmail payload }

  retrieveProvidersAndUpdateMenu ∷ DSL Unit
  retrieveProvidersAndUpdateMenu = void do
    eProviders ← Api.retrieveAuthProviders
    case eProviders of
      Right p → H.modify _{ providers = p }
      _ → pure unit

authenticate ∷ Maybe ProviderR → DSL Unit
authenticate = maybe logOut logIn
  where
  keySuffix ∷ String
  keySuffix =
    AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider

  logOut ∷ DSL Unit
  logOut = do
    removeAuthenticationDetails keySuffix
    update

  logIn ∷ ProviderR → DSL Unit
  logIn providerR = do
    { auth } ← Wiring.expose
    idToken ← H.liftAff AVar.makeVar
    H.liftAff $ Bus.write { providerR, idToken, prompt: true, keySuffix } auth.requestToken
    either signInFailure (const $ signInSuccess) =<< (H.liftAff $ AVar.takeVar idToken)

  signInSuccess ∷ DSL Unit
  signInSuccess = do
    wiring ← Wiring.expose
    update
    traverse_ (lift ∘ Persistence.queueEvalImmediate ∘ EvalCard.toAll)
      =<< (H.liftEff $ Ref.readRef wiring.auth.retryEval)
    whenM
      (H.liftEff $ Ref.readRef wiring.auth.retrySave)
      (void $ lift $ Persistence.saveWorkspace)
    H.liftAff $ Bus.write SignInSuccess wiring.auth.signIn

  signInFailure ∷ AuthenticationError → DSL Unit
  signInFailure error = do
    { auth, bus } ← Wiring.expose
    H.liftAff do
      maybe (pure unit) (flip Bus.write bus.notify) (toNotificationOptions error)
      Bus.write SignInFailure auth.signIn

presentHelp ∷ String → DSL Unit
presentHelp = H.liftEff ∘ Browser.newTab
