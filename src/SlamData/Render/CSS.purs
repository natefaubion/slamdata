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

filesystem ∷ ClassName
filesystem = className "filesystem"

dashboard ∷ ClassName
dashboard = className "dashboard"

version ∷ ClassName
version = className "version"

selected ∷ ClassName
selected = className "selected"

phantom ∷ ClassName
phantom = className "phantom"

searchInput ∷ ClassName
searchInput = className "search-input"

searchClear ∷ ClassName
searchClear = className "search-clear"

searchPath ∷ ClassName
searchPath = className "search-path"

searchPathActive ∷ ClassName
searchPathActive = className "search-path-active"

searchAffix ∷ ClassName
searchAffix = className "search-affix"

searchPathBody ∷ ClassName
searchPathBody = className "search-path-body"

searchAffixEmpty ∷ ClassName
searchAffixEmpty = className "search-affix-empty"

results ∷ ClassName
results = className "results"

header ∷ ClassName
header = className "header"

headerMenu ∷ ClassName
headerMenu = className "header-menu"

logo ∷ ClassName
logo = className "logo"

navCont ∷ ClassName
navCont = className "nav-cont"

navIcon ∷ ClassName
navIcon = className "nav-icon"

navLogo ∷ ClassName
navLogo = className "nav-logo"

search ∷ ClassName
search = className "search"

content ∷ ClassName
content = className "content"

toolbarSort ∷ ClassName
toolbarSort = className "toolbar-sort"

toolbarMenu ∷ ClassName
toolbarMenu = className "toolbar-menu"

itemIcon ∷ ClassName
itemIcon = className "item-icon"

itemToolbar ∷ ClassName
itemToolbar = className "item-toolbar"

itemContent ∷ ClassName
itemContent = className "item-content"

itemHidden ∷ ClassName
itemHidden = className "item-hidden"

invisible ∷ ClassName
invisible = className "sd-invisible"

fileListField ∷ ClassName
fileListField = className "file-list-field"

fileListGroup ∷ ClassName
fileListGroup = className "file-list-group"

workspaceNav ∷ ClassName
workspaceNav = className "workspace-nav"

dialogDownload ∷ ClassName
dialogDownload = className "dialog-download"

dialogMount ∷ ClassName
dialogMount = className "dialog-mount"

renameDialogForm ∷ ClassName
renameDialogForm = className "rename-dialog-form"

mountSection ∷ ClassName
mountSection = className "mount-section"

mountMongoDB ∷ ClassName
mountMongoDB = className "mount-mongodb"

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

mountPropsScrollbox ∷ ClassName
mountPropsScrollbox = className "mount-props-scrollbox"

mountProgressSpinner ∷ Boolean -> ClassName
mountProgressSpinner true = className "mount-progress-spinner"
mountProgressSpinner false = className "mount-progress-spinner-hidden"

workspaceViewHack ∷ ClassName
workspaceViewHack = className "workspace-view-hack"

deckCard ∷ ClassName
deckCard = className "deck-card"

cardHeader ∷ ClassName
cardHeader = className "card-header"

cardIcon ∷ ClassName
cardIcon = className "card-icon"

cardName ∷ ClassName
cardName = className "card-name"

cardInput ∷ ClassName
cardInput = className "card-input"

cardOutput ∷ ClassName
cardOutput = className "card-output"

cardOutputLabel ∷ ClassName
cardOutputLabel = className "card-output-label"

cardOutputResult ∷ ClassName
cardOutputResult = className "card-output-result"

cardNextActions ∷ ClassName
cardNextActions = className "card-next-actions"

newCardMenu ∷ ClassName
newCardMenu = className "new-card-menu"

aceContainer ∷ ClassName
aceContainer = className "ace-container"

playButton ∷ ClassName
playButton = className "play-button"

stopButton ∷ ClassName
stopButton = className "stop-button"

cardControls ∷ ClassName
cardControls = className "card-controls"

statusText ∷ ClassName
statusText = className "status-text"

cardFailures ∷ ClassName
cardFailures = className "card-failures"

cardMessages ∷ ClassName
cardMessages = className "card-messages"

cardBlockedMessage ∷ ClassName
cardBlockedMessage = className "card-blocked-message"

cardEvalLine ∷ ClassName
cardEvalLine = className "card-eval-line"

exploreCardEditor ∷ ClassName
exploreCardEditor = className "explore-card-editor"

pageInput ∷ ClassName
pageInput = className "page-input"

pageSize ∷ ClassName
pageSize = className "page-size"

searchCardButton ∷ ClassName
searchCardButton = className "search-card-button"

nextCardList ∷ ClassName
nextCardList = className "next-card-list"

echartsContainer ∷ ClassName
echartsContainer = className "echarts-container"

chartConfigureForm ∷ ClassName
chartConfigureForm = className "chart-configure-form"

vizCardEditor ∷ ClassName
vizCardEditor = className "viz-card-editor"

vizChartTypeSelector ∷ ClassName
vizChartTypeSelector = className "viz-chart-type-selector"

vizChartConfiguration ∷ ClassName
vizChartConfiguration = className "viz-chart-configuration"

collapsed ∷ ClassName
collapsed = className "collapsed"

scrollbox ∷ ClassName
scrollbox = className "scrollbox"

loadingMessage ∷ ClassName
loadingMessage = className "loading-message"

withAggregation ∷ ClassName
withAggregation = className "with-aggregation"

aggregation ∷ ClassName
aggregation = className "aggregation"

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

hiddenFileInput ∷ ClassName
hiddenFileInput = className "hidden-file-input"

chartConfigureHeight ∷ ClassName
chartConfigureHeight = className "chart-configure-height"

chartConfigureWidth ∷ ClassName
chartConfigureWidth = className "chart-configure-width"


chartCategory ∷ ClassName
chartCategory = className "chart-category"

chartMeasureOne ∷ ClassName
chartMeasureOne = className "chart-measure-one"

chartMeasureTwo ∷ ClassName
chartMeasureTwo = className "chart-measure-two"

chartDimension ∷ ClassName
chartDimension = className "chart-dimension"

chartSeriesOne ∷ ClassName
chartSeriesOne = className "chart-series-one"

chartSeriesTwo ∷ ClassName
chartSeriesTwo = className "chart-series-two"

pieChartIcon ∷ ClassName
pieChartIcon = className "pie-chart-icon"

barChartIcon ∷ ClassName
barChartIcon = className "bar-chart-icon"

lineChartIcon ∷ ClassName
lineChartIcon = className "line-chart-icon"

deleteDeckIcon ∷ ClassName
deleteDeckIcon = className "delete-deck-icon"

actionIcon ∷ ClassName
actionIcon = className "action-icon"

chartEditor ∷ ClassName
chartEditor = className "chart-editor"

chartOutput ∷ ClassName
chartOutput = className "chart-output"

glyphiconInactive ∷ ClassName
glyphiconInactive = className "glyphicon-inactive"

axisLabelParam ∷ ClassName
axisLabelParam = className "axis-label-param"

chartSizeParam ∷ ClassName
chartSizeParam = className "chart-size-param"

downloadCardEditor ∷ ClassName
downloadCardEditor = className "download-card-editor"

downloadTypeSelector ∷ ClassName
downloadTypeSelector = className "download-type-selector"

downloadConfiguration ∷ ClassName
downloadConfiguration = className "download-configuration"

sqlMountVarPair ∷ ClassName
sqlMountVarPair = className "sql-mount-var-pair"

sqlMountForm ∷ ClassName
sqlMountForm = className "sql-mount-form"

sqlMountAddVarPairButton ∷ ClassName
sqlMountAddVarPairButton = className "sql-mount-add-var-pair-button"

fileAction ∷ ClassName
fileAction = className "file-action"

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

openResourceCard ∷ ClassName
openResourceCard = className "open-resource-card"

openResourceCardMenu ∷ ClassName
openResourceCardMenu = className "open-resource-card-menu"

loading ∷ ClassName
loading = className "loading"

cardSlider :: ClassName
cardSlider = className "sd-card-slider"

board :: ClassName
board = className "sd-board"

gridPattern :: ClassName
gridPattern = className "sd-grid-pattern"

deck :: ClassName
deck = className "sd-deck"

card :: ClassName
card = className "sd-card"

cardGripper :: ClassName
cardGripper = className "sd-card-gripper"

cardGripperLast :: ClassName
cardGripperLast = className "sd-card-gripper-last"

flipDeck :: ClassName
flipDeck = className "sd-flip-deck"

grabDeck :: ClassName
grabDeck = className "sd-grab-deck"

resizeDeck :: ClassName
resizeDeck = className "sd-resize-deck"

form ∷ ClassName
form = className "sd-form"

formButton ∷ ClassName
formButton = className "sd-form-button"
