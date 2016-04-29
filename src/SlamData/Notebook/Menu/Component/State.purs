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

module SlamData.Notebook.Menu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap

import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Component.State as HalogenMenu

import SlamData.Notebook.Component.State (NotebookShortcut())
import SlamData.Notebook.Menu.Component.Query
import SlamData.Notebook.Rename.Component as Rename

type StateP g = HalogenMenu.MenuP (Maybe Value) g

make :: (StrMap.StrMap NotebookShortcut) -> HalogenMenu.Menu (Maybe Value)
make shortcuts = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , shortcutLabel: Nothing
          , value: Just $ renameQueryToValue $ Rename.Focus unit
          }
        , { label: "Publish"
          , shortcutLabel: StrMap.lookup "NotebookPublish" shortcuts >>= _.label
          , value: StrMap.lookup "NotebookPublish" shortcuts >>= _.value >>> pure
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , shortcutLabel: StrMap.lookup "InsertQuery" shortcuts >>= _.label
          , value: StrMap.lookup "InsertQuery" shortcuts >>= _.value >>> pure
          }
        , { label: "Markdown"
          , shortcutLabel: StrMap.lookup "InsertMarkdown" shortcuts >>= _.label
          , value: StrMap.lookup "InsertMarkdown" shortcuts >>= _.value >>> pure
          }
        , { label: "Explore"
          , shortcutLabel: StrMap.lookup "InsertExplore" shortcuts >>= _.label
          , value: StrMap.lookup "InsertExplore" shortcuts >>= _.value >>> pure
          }
        , { label: "Search"
          , shortcutLabel: StrMap.lookup "InsertSearch" shortcuts >>= _.label
          , value: StrMap.lookup "InsertSearch" shortcuts >>= _.value >>> pure
          }
        , { label: "API"
          , shortcutLabel: StrMap.lookup "InsertAPI" shortcuts >>= _.label
          , value: StrMap.lookup "InsertAPI" shortcuts >>= _.value >>> pure
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Users Guide"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://docs.slamdata.com/en/latest/users-guide/"
          }
        , { label: "Tutorial"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://docs.slamdata.com/en/latest/tutorial/"
          }
        , { label: "SQL² Reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://docs.slamdata.com/en/latest/sql-squared-reference/"
          }
        , { label: "SlamDown reference"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://docs.slamdata.com/en/latest/slamdown-reference/"
          }
        , { label: "Cheatsheet"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "https://d1qmdf3vop2l07.cloudfront.net/buff-hare1.cloudvent.net/raw/images/slamdata-cheatsheet-20160322.pdf"
          }
        , { label: "Securing access to SlamData"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://docs.slamdata.com/en/latest/securing-slamdata/"
          }
        , { label: "Report a bug"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "https://slamdata.atlassian.net/secure/Dashboard.jspa"
          }
        , { label: "Request support"
          , shortcutLabel: Nothing
          , value: Just $ helpURIToValue $ HelpURI "http://slamdata.com/support-portal/submit-a-support-request/"
          }
        ]
    }
  ]
