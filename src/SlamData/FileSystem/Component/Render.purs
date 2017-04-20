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

module SlamData.FileSystem.Component.Render where

import SlamData.Prelude

import Data.Lens ((^.))

import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Query (action)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Common.Sort (Sort(..))
import SlamData.FileSystem.Component.CSS as CSS
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.State (State, _showHiddenFiles, _isMount, _sort)
import SlamData.Hint as Hint
import SlamData.Render.Icon as I

import Utils.DOM as DOM

sorting ∷ ∀ a. State → HTML a (Query Unit)
sorting state =
  let
    chevron ∷ { icon ∷ H.HTML a (Query Unit), label ∷ String }
    chevron = case state ^. _sort of
      Asc → { icon: I.chevronUpSm, label: "Sort files by name descending" }
      Desc → { icon: I.chevronDownSm, label: "Sort files by name ascending" }
  in
    H.div
      [ P.classes [ B.colXs4, CSS.toolbarSort ] ]
      [ H.a
        [ E.onClick \e → Just $ PreventDefault (DOM.toEvent e) $ action $ Resort ]
        [ H.text "Name"
        , H.span
          [ ARIA.label $ chevron.label
          , P.title $ chevron.label
          ]
          [ chevron.icon ]
        ]
      ]

toolbar ∷ ∀ p. State → HTML p (Query Unit)
toolbar state =
  H.div
    [ P.classes [ B.colXs5, CSS.toolbarMenu ] ]
    $ ( guard state.presentMountHint $>
        Hint.render
          Hint.RightArrow
          (H.ClassName "sd-mount-guide")
          DismissMountHint
          "To begin exploring data, please press the Mount button"
      )
    <> [ H.ul
         [ P.classes [ B.listInline, B.pullRight ] ]
         $ configure <> [ mount, workspace, folder, showHide, download, file ]
       ]
  where
  configure ∷ Array (HTML p (Query Unit))
  configure = do
    guard $ state ^. _isMount
    pure $ toolItem Configure "Configure mount" I.cog

  showHide ∷ HTML p (Query Unit)
  showHide =
    if state ^. _showHiddenFiles
    then toolItem HideHiddenFiles "Hide hidden files" I.eyeVisibleSm
    else toolItem ShowHiddenFiles "Show hidden files" I.eyeHiddenSm

  download ∷ HTML p (Query Unit)
  download = toolItem Download "Download" I.downloadSm

  mount ∷ HTML p (Query Unit)
  mount = toolItem MakeMount "Mount database" I.databaseCreateSm

  folder ∷ HTML p (Query Unit)
  folder = toolItem MakeFolder "Create folder" I.folderCreateSm

  file ∷ HTML p (Query Unit)
  file =
    H.li_
      [ H.input
          [ P.type_ P.InputFile
          , E.onChange (map (action <<< FileListChanged) <<< DOM.fromNode <<< DOM.target)
          , P.id_ "upload"
          ]
      , H.label
          [ P.for "upload"
          , P.classes
              [ H.ClassName "tool-item"
              , H.ClassName "sd-tooltip"
              , H.ClassName "sd-tooltip-r"
              ]
          , ARIA.label "Upload file"
          ]
          [ I.uploadSm ]
      ]

  workspace ∷ HTML p (Query Unit)
  workspace = toolItem MakeWorkspace "Create workspace" I.workspaceSm

toolItem ∷ ∀ p f. (Unit → f Unit) → String → HTML p (f Unit) → HTML p (f Unit)
toolItem func title icon =
  H.li_
    [ H.button
        [ ARIA.label title
        , P.classes
            [ H.ClassName "tool-item"
            , H.ClassName "sd-tooltip"
            ]
        , E.onClick (E.input_ func)
        ]
        [ icon ]
    ]
