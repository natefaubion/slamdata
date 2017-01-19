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

module SlamData.Render.CSS where

import Halogen.HTML.Core (className, ClassName)

version ∷ ClassName
version = className "version"

results ∷ ClassName
results = className "results"

header ∷ ClassName
header = className "header"

logo ∷ ClassName
logo = className "logo"

navIcon ∷ ClassName
navIcon = className "nav-icon"

navLogo ∷ ClassName
navLogo = className "nav-logo"

content ∷ ClassName
content = className "content"

invisible ∷ ClassName
invisible = className "sd-invisible"

fileListField ∷ ClassName
fileListField = className "file-list-field"

fileListGroup ∷ ClassName
fileListGroup = className "file-list-group"

dialogDownload ∷ ClassName
dialogDownload = className "dialog-download"

dialogMount ∷ ClassName
dialogMount = className "dialog-mount"

renameDialogForm ∷ ClassName
renameDialogForm = className "rename-dialog-form"

mountMongoDB ∷ ClassName
mountMongoDB = className "mount-mongodb"

mountCouchbase ∷ ClassName
mountCouchbase = className "mount-couchbase"

mountMarkLogic ∷ ClassName
mountMarkLogic = className "mount-marklogic"

mountSpark ∷ ClassName
mountSpark = className "mount-spark"

mountName ∷ ClassName
mountName = className "mount-name"

mountHostList ∷ ClassName
mountHostList = className "mount-host-list"

mountUserInfo ∷ ClassName
mountUserInfo = className "mount-userinfo"

mountHost ∷ ClassName
mountHost = className "mount-host"

mountProps ∷ ClassName
mountProps = className "mount-props"

mountPath ∷ ClassName
mountPath = className "mount-path"

mountFormat ∷ ClassName
mountFormat = className "mount-format"

mountPropsScrollbox ∷ ClassName
mountPropsScrollbox = className "mount-props-scrollbox"

mountProgressSpinner ∷ Boolean → ClassName
mountProgressSpinner true = className "mount-progress-spinner"
mountProgressSpinner false = className "mount-progress-spinner-hidden"

cardInput ∷ ClassName
cardInput = className "card-input"

aceContainer ∷ ClassName
aceContainer = className "ace-container"

cardFailures ∷ ClassName
cardFailures = className "card-failures"

pageSize ∷ ClassName
pageSize = className "page-size"

collapsed ∷ ClassName
collapsed = className "collapsed"

loadingMessage ∷ ClassName
loadingMessage = className "loading-message"

embedBox ∷ ClassName
embedBox = className "embed-box"

downloadSource ∷ ClassName
downloadSource = className "download-source"

downloadTarget ∷ ClassName
downloadTarget = className "download-target"

downloadTargetBox ∷ ClassName
downloadTargetBox = className "download-target-box"

downloadCSVDelimiters ∷ ClassName
downloadCSVDelimiters = className "download-csv-delimiters"

downloadArrayMode ∷ ClassName
downloadArrayMode = className "download-array-mode"

downloadJSONOptions ∷ ClassName
downloadJSONOptions = className "download-json-options"

refreshButton ∷ ClassName
refreshButton = className "refresh-button"

shareButton ∷ ClassName
shareButton = className "share-button"

aggregation ∷ ClassName
aggregation = className "aggregation"

deleteDeckIcon ∷ ClassName
deleteDeckIcon = className "delete-deck-icon"

actionIcon ∷ ClassName
actionIcon = className "action-icon"

chartOutput ∷ ClassName
chartOutput = className "chart-output"

glyphiconInactive ∷ ClassName
glyphiconInactive = className "glyphicon-inactive"

downloadCardEditor ∷ ClassName
downloadCardEditor = className "download-card-editor"

downloadTypeSelector ∷ ClassName
downloadTypeSelector = className "download-type-selector"

downloadConfiguration ∷ ClassName
downloadConfiguration = className "download-configuration"

permissionsCheckboxes ∷ ClassName
permissionsCheckboxes = className "permissions-checkboxes"

tokenGeneratorForm ∷ ClassName
tokenGeneratorForm = className "token-generator-form"

cancelInputRunIcon ∷ ClassName
cancelInputRunIcon = className "cancel-input-run-icon"

userShareForm ∷ ClassName
userShareForm = className "user-share-form"

nextActionCard ∷ ClassName
nextActionCard = className "next-action-card"

sharePermissionsDialog ∷ ClassName
sharePermissionsDialog = className "share-permissions-dialog"

sharePermissionsCheckboxes ∷ ClassName
sharePermissionsCheckboxes = className "share-permissions-checkboxes"

sharePermissionsResourceMark ∷ ClassName
sharePermissionsResourceMark = className "share-permissions-resource-mark"

sharePermissionsContent ∷ ClassName
sharePermissionsContent = className "share-permissions-content"

sharePermissionsButtons ∷ ClassName
sharePermissionsButtons = className "share-permissions-buttons"

glyphImage ∷ ClassName
glyphImage = className "glyph-image"

saveCardButton ∷ ClassName
saveCardButton = className "save-card-button"

chartGlyph ∷ ClassName
chartGlyph = className "chart-glyph"

codeGlyph ∷ ClassName
codeGlyph = className "code-glyph"

deckBackSide ∷ ClassName
deckBackSide = className "sd-deck-backside"

openCard ∷ ClassName
openCard = className "open-card"

openCardMenu ∷ ClassName
openCardMenu = className "open-card-menu"

loading ∷ ClassName
loading = className "loading"

pending ∷ ClassName
pending = className "pending"

cardSlider ∷ ClassName
cardSlider = className "sd-card-slider"

card :: ClassName
card = className "sd-card"

cardSliding ∷ ClassName
cardSliding = className "sd-card-sliding"

cardTransitioning ∷ ClassName
cardTransitioning = className "sd-card-transitioning"

cardActive ∷ ClassName
cardActive = className "sd-card-active"

cardGripper ∷ ClassName
cardGripper = className "sd-card-gripper"

cardGripperLast ∷ ClassName
cardGripperLast = className "sd-card-gripper-last"

form ∷ ClassName
form = className "sd-form"

formButton ∷ ClassName
formButton = className "sd-form-button"
