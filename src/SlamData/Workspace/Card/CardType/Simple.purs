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

module SlamData.Workspace.Card.CardType.Simple where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_search = SProxy ∷ SProxy "search"
_markdown = SProxy ∷ SProxy "markdown"
_table = SProxy ∷ SProxy "table"
_download = SProxy ∷ SProxy "download"
_variables = SProxy ∷ SProxy "variables"
_troubleshoot = SProxy ∷ SProxy "troubleshoot"
_cache = SProxy ∷ SProxy "cache"
_open = SProxy ∷ SProxy "open"
_downloadOptions = SProxy ∷ SProxy "downloadOptions"
_draftboard = SProxy ∷ SProxy "draftboard"
_tabs = SProxy ∷ SProxy "tabs"
_structureEditor = SProxy ∷ SProxy "structureEditor"
_viz = SProxy ∷ SProxy "viz"
_setupViz = SProxy ∷ SProxy "setupViz"

viz ∷ ∀ r. Variant (viz ∷ Unit|r)
viz = inj _viz unit

search ∷ ∀ r. Variant (search ∷ Unit|r)
search = inj _search unit

markdown ∷ ∀ r. Variant (markdown ∷ Unit|r)
markdown = inj _markdown unit

table ∷ ∀ r. Variant (table ∷ Unit|r)
table = inj _table unit

download ∷ ∀ r. Variant (download ∷ Unit|r)
download = inj _download unit

variables ∷ ∀ r. Variant (variables ∷ Unit|r)
variables = inj _variables unit

troubleshoot ∷ ∀ r. Variant (troubleshoot ∷ Unit|r)
troubleshoot = inj _troubleshoot unit

cache ∷ ∀ r. Variant (cache ∷ Unit|r)
cache = inj _cache unit

open ∷ ∀ r. Variant (open ∷ Unit|r)
open = inj _open unit

downloadOptions ∷ ∀ r. Variant (downloadOptions ∷ Unit|r)
downloadOptions = inj _downloadOptions unit

draftboard ∷ ∀ r. Variant (draftboard ∷ Unit|r)
draftboard = inj _draftboard unit

tabs ∷ ∀ r. Variant (tabs ∷ Unit|r)
tabs = inj _tabs unit

structureEditor ∷ ∀ r. Variant (structureEditor ∷ Unit|r)
structureEditor = inj _structureEditor unit

setupViz ∷ ∀ r. Variant (setupViz ∷ Unit|r)
setupViz = inj _setupViz unit

type SimpleR r =
  ( search ∷ Unit
  , markdown ∷ Unit
  , table ∷ Unit
  , download ∷ Unit
  , variables ∷ Unit
  , troubleshoot ∷ Unit
  , cache ∷ Unit
  , open ∷ Unit
  , downloadOptions ∷ Unit
  , draftboard ∷ Unit
  , tabs ∷ Unit
  , structureEditor ∷ Unit
  , viz ∷ Unit
  , setupViz ∷ Unit
  | r)

type Simple r = Variant (SimpleR r)

all ∷ ∀ r. Array (Simple r)
all =
  [ search
  , markdown
  , table
  , download
  , variables
  , troubleshoot
  , cache
  , open
  , downloadOptions
  , draftboard
  , tabs
  , structureEditor
  , setupViz
  , viz
  ]

print ∷ ∀ r. (Variant r → String) → Simple r → String
print cb = cb
  # on _viz (const "viz")
  # on _search (const "search")
  # on _markdown (const "markdown")
  # on _table (const "table")
  # on _structureEditor (const "structure-editor")
  # on _download (const "download")
  # on _downloadOptions (const "download-options")
  # on _draftboard (const "draftboard")
  # on _variables (const "variables")
  # on _troubleshoot (const "troubleshoot")
  # on _cache (const "cache")
  # on _open (const "open")
  # on _tabs (const "tabs")
  # on _setupViz (const "setupViz")

parse ∷ ∀ r. String → String ⊹ Simple r
parse = case _ of
  "viz" → pure viz
  "search" → pure search
  "markdown" → pure markdown
  "table" → pure table
  "download" → pure download
  "variables" → pure variables
  "troubleshoot" → pure troubleshoot
  "cache" → pure cache
  "open" → pure open
  "download-options" → pure downloadOptions
  "draftboard" → pure draftboard
  "tabs" → pure tabs
  "structure-editor" → pure structureEditor
  "chart" → pure viz
  "form-input" → pure viz
  "geo-chart" → pure viz
  "setupViz" → pure setupViz
  ty → Left $ ty ⊕ " is unknown basic card type"

name ∷ ∀ r. (Variant r → String) → Simple r → String
name cb = cb
  # on _viz (const "Show Visualization")
  # on _search (const "Search")
  # on _markdown (const "Show Markdown")
  # on _table (const "Preview Table")
  # on _download (const "Show Download")
  # on _variables (const "Setup Variables")
  # on _troubleshoot (const "Troubleshoot")
  # on _cache (const "Cache")
  # on _open (const "Open")
  # on _downloadOptions (const "Setup Downlaod")
  # on _draftboard (const "Setup Dashboard")
  # on _tabs (const "Setup Tabs")
  # on _structureEditor (const "Structure Editor")
  # on _setupViz (const "Setup Visualization")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Simple r → I.IconHTML
icon cb = cb
  # on _viz (const $ I.IconHTML I.cardsShowChart)
  # on _search (const $ I.IconHTML I.cardsSearch)
  # on _download (const $ I.IconHTML I.cardsShowDownload)
  # on _variables (const $ I.IconHTML I.cardsSetupVariables)
  # on _troubleshoot (const $ I.IconHTML I.cardsTroubleshoot)
  # on _markdown (const $ I.IconHTML I.cardsShowMarkdown)
  # on _table (const $ I.IconHTML I.cardsTable)
  # on _cache (const $ I.IconHTML I.cardsCache)
  # on _open (const $ I.IconHTML I.cardsOpen)
  # on _downloadOptions (const $ I.IconHTML I.cardsSetupDownload)
  # on _draftboard (const $ I.IconHTML I.cardsDashboard)
  # on _tabs (const $ I.IconHTML I.cardsTabs)
  # on _structureEditor (const $ I.IconHTML I.cardsStructureEditor)
  # on _setupViz (const $ I.IconHTML I.cardsSetupChart)

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Simple r → Array H.ClassName
cardClasses cb = cb
  # on _viz (\_ → [ H.ClassName "sd-card-chart" ] )
  # on _search (\_ → [ H.ClassName "sd-card-search" ])
  # on _markdown (\_ → [ H.ClassName "sd-card-markdown" ])
  # on _table (\_ → [ H.ClassName "sd-card-table" ])
  # on _download (\_ → [ H.ClassName "sd-card-download" ])
  # on _downloadOptions (\_ → [ H.ClassName "sd-card-download-options" ])
  # on _variables (\_ → [ H.ClassName "sd-card-variables" ])
  # on _troubleshoot (\_ → [ H.ClassName "sd-card-troubleshoot" ])
  # on _cache (\_ → [ H.ClassName "sd-card-cache" ] )
  # on _open (\_ → [ H.ClassName "sd-card-open" ])
  # on _draftboard (\_ → [ H.ClassName "sd-card-draftboard" ])
  # on _structureEditor (\_ → [ H.ClassName "sd-structure-editor" ])
  # on _tabs (\_ → [ H.ClassName "sd-card-tabs" ])
  # on _setupViz (\_ → [ H.ClassName "sd-card-chart-options" ] )

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Simple r → Boolean
consumerInteractable cb = cb
  # on _viz tt
  # on _search tt
  # on _markdown tt
  # on _table tt
  # on _download tt
  # on _downloadOptions ff
  # on _variables ff
  # on _troubleshoot ff
  # on _cache ff
  # on _open ff
  # on _draftboard tt
  # on _tabs tt
  # on _structureEditor ff
  # on _setupViz tt

contractToSimple ∷ ∀ r. Contractable r (SimpleR ()) ⇒ Variant r → Maybe (Simple ())
contractToSimple = contract
