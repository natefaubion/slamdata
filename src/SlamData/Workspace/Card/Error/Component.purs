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

module SlamData.Workspace.Card.Error.Component
  ( errorCardComponent
  , module SlamData.Workspace.Card.Error.Component.Query
  , module SlamData.Workspace.Card.Error.Component.State
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as A
import Data.Foldable (intercalate)
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Path.Pathy as Path
import Data.Variant (case_, on)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.QuasarAF as QA
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Card.Cache.Error as CCaE
import SlamData.Workspace.Card.CardType (AceMode(..), CardType(..), cardName)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Chart.Error as CChE
import SlamData.Workspace.Card.DownloadOptions.Error as CDOE
import SlamData.Workspace.Card.Error (CardError, cardToGlobalError)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Error.Component.Query (Query(..))
import SlamData.Workspace.Card.Error.Component.State (State, initialState)
import SlamData.Workspace.Card.Markdown.Error as CME
import SlamData.Workspace.Card.Open.Error as COE
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Error as CQE
import SlamData.Workspace.Card.Search.Error as CSE
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Error as CPTE
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Error as CFILE
import SlamData.Workspace.Card.Setups.FormInput.Static.Error as CFISE
import SlamData.Workspace.Card.Table.Error as CTE
import SlamData.Workspace.Card.Variables.Error as CVE
import Text.Parsing.Parser (parseErrorMessage)
import Utils (prettyJson)
import Utils.Path as PU

type DSL = H.ComponentDSL State Query Void Slam
type HTML = H.ComponentHTML Query

errorCardComponent ∷ H.Component HH.HTML Query CardError Void Slam
errorCardComponent =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    -- The input of this card is never modified after it is created, we only
    -- use the input for the initial state.
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render st =
  HH.div
    [ HP.class_ (H.ClassName "sd-error-container") ]
    [ prettyPrintCardError st st.error ]

errorTitle ∷ Array HTML → HTML
errorTitle title =
  HH.h1_
    [ I.warningSm
    , HH.span_ title
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    { accessType } ← Wiring.expose
    H.modify (_ { accessType = accessType })
    pure next
  ToggleExpanded b next → do
    H.modify (_ { expanded = b })
    pure next

printQErrorWithDetails ∷ QA.QError → HTML
printQErrorWithDetails = case _ of
  err → HH.text (QA.printQError err)

printQErrorDetails ∷ QA.QError → HTML
printQErrorDetails = case _ of
  QA.ErrorMessage { raw } → printQErrorRaw raw
  err → HH.text (QA.printQError err)

printQErrorRaw ∷ J.JObject → HTML
printQErrorRaw raw = HH.pre_ [ HH.text (prettyJson (J.fromObject raw)) ]

prettyPrintCardError ∷ State → CardError → HTML
prettyPrintCardError state ce =
  case cardToGlobalError ce of
    Just ge →
      HH.div_
        [ errorTitle [ HH.text (GE.print ge) ] ]
    Nothing →
      ce # (case_
        # on CE._qerror printQError
        # on CE._stringly printStringly
        # on CE._resource (resourceErrorMessage state)
        # on CE._cache (cacheErrorMessage state)
        # on CE._chart (chartErrorMessage state)
        # on CE._downloadOptions (downloadOptionsErrorMessage state)
        # on CE._formInputLabeled (formInputLabeledErrorMessage state)
        # on CE._formInputStatic (formInputStaticErrorMessage state)
        # on CE._markdown (markdownErrorMessage state)
        # on CE._open (openErrorMessage state)
        # on CE._pivotTable (pivotTableErrorMessage state)
        # on CE._query (queryErrorMessage state)
        # on CE._search (searchErrorMessage state)
        # on CE._table (tableErrorMessage state)
        # on CE._variables (variablesErrorMessage state))
  where
  printQError qError =
    HH.div_
      [ errorTitle [ HH.text "An error occurred." ]
      , printQErrorWithDetails qError
      ]
  printStringly err =
    HH.div_
      [ errorTitle [ HH.text err ] ]

collapsible ∷ String → HTML → Boolean → HTML
collapsible title content expanded =
  HH.div
    [ HP.classes [ H.ClassName "sd-collapsible-error" ] ]
    $ join
      [ pure $
          HH.button
            [ HP.class_ (H.ClassName "sd-form-button")
            , HE.onClick $ HE.input_ (ToggleExpanded (not expanded))
            ]
            [ if expanded then I.chevronDownSm else I.chevronRightSm
            , HH.span_ [ HH.text title ]
            ]
      , guard expanded $>
          HH.div
            [ HP.class_ (H.ClassName "sd-collapsible-error-content") ]
            [ content ]
      ]

resourceErrorMessage ∷ State → CE.ResourceError → HTML
resourceErrorMessage state (CE.ResourceError rs) =
  HH.div_ $
    [ errorTitle [ HH.text "An error occured when attempting to read a resource." ] ] <>
      if L.null rs
        then
          [ HH.p_ [ HH.text "There are no available resources. Go back and select one to continue." ]
          ]
        else
          [ HH.p_ [ HH.text "There are multiple available resources." ]
          , HH.ul_ $ A.fromFoldable $ rs <#> \r →
              HH.li_ [ HH.text $ PU.printAnyFilePath $ Port.filePath r ]
          ]

queryErrorMessage ∷ State → CQE.QueryError → HTML
queryErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (Ace SQLMode) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CQE.QueryCompileError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occurred during query compilation." ]
          , renderMore qErr
          ]
    CQE.QueryRetrieveResultError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occurred when retrieving the query result." ]
          , renderMore qErr
          ]
    CQE.QueryParseError pErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occured while parsing the query." ]
          , pure $ HH.pre_ [ HH.text pErr ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
  renderMore = case _ of
    QA.ErrorMessage {title, message, raw} →
      pure $ HH.div_
        $ join
          [ foldMap (\t -> pure $ HH.p_ [ HH.text t ]) title
          , pure $ HH.p_ [ HH.text message ]
          , guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorRaw raw) expanded
          ]
    err' →
      guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorDetails err') expanded

cacheErrorMessage ∷ State → CCaE.CacheError → HTML
cacheErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Cache <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CCaE.CacheInvalidFilepath fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "There is a problem in the configuration of the " <> cardName Cache <> " card." ]
          , pure $ HH.p_
              [ HH.text "The provided path "
              , HH.code_ [ HH.text fp ]
              , HH.text " is not a valid location to store the cache result."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CCaE.CacheQuasarError qe →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_ [ HH.text "The Quasar analytics engine returned an error while verifying the cache result." ]
          , guard (accessType == Editable) $> collapsible "Quasar error details" (printQErrorDetails qe) expanded
          ]
    CCaE.CacheErrorSavingFile fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_
              [ HH.text "The file "
              , HH.code_ [ HH.text (Path.printPath fp) ]
              , HH.text " could not be written to."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and provide an alternative path to fix this error." ]
          ]
    CCaE.CacheResourceNotModified →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Caching the result of a query failed." ]
          , pure $ HH.p_ [ HH.text "Caching can only be applied to queries that perform at least one transformation on an existing data set." ]
          -- TODO: only show this solution when there are no following cards?
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and delete it to fix this error." ]
          ]

markdownErrorMessage ∷ State → CME.MarkdownError → HTML
markdownErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (Ace MarkdownMode) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CME.MarkdownParseError {markdown, error} →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "Parsing the provided Markdown failed." ]
          , pure $ HH.p_ [ HH.code_ [ HH.text error ] ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CME.MarkdownSqlParseError {field, sql, error} →
      case field of
        Nothing →
          HH.div_
            $ join
              [ pure $ errorTitle [ HH.text "Parsing a SQL² query embedded in Markdown failed." ]
              , pure $ HH.p_ [ HH.text "The query:" ]
              , pure $ HH.pre_ [ HH.text sql ]
              , pure $ HH.p_ [ HH.text "Failed to parse with the following error:" ]
              , pure $ HH.pre_ [ HH.text error ]
              , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and edit the query to fix this error." ]
              ]
        Just fieldName →
          HH.div_
            $ join
              [ pure $ errorTitle [ HH.text "There was a problem populating a field based on a SQL² query." ]
              , pure $ HH.p_
                  [ HH.text "The field "
                  , HH.code_ [ HH.text fieldName ]
                  , HH.text " failed to parse with the following error:"
                  ]
              , pure $ HH.pre_ [ HH.text error ]
              , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and edit the query to fix this error." ]
              ]
    CME.MarkdownNoTextBoxResults { field, sql } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "There was a problem populating a field based on a SQL² query." ]
          , pure $ HH.p_
              [ HH.text "The field "
              , HH.code_ [ HH.text field ]
              , HH.text " has the query:"
              ]
          , pure $ HH.pre_ [ HH.text sql ]
          , pure $ HH.p_ [ HH.text "Which returned no results." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and edit the query to fix this error." ]
          ]
    CME.MarkdownInvalidTimeValue { field, time, error } →
      renderParseValueError field "time" error time
    CME.MarkdownInvalidDateValue { field, date, error } →
      renderParseValueError field "date" error date
    CME.MarkdownInvalidDateTimeValue { field, datetime, error } →
      renderParseValueError field "timestamp" error datetime
    CME.MarkdownTypeError { field, actual, expected } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "There was a problem populating a field based on a SQL² query." ]
          , pure $ HH.p_
              [ HH.text "The query for field "
              , HH.code_ [ HH.text field ]
              , HH.text " returned a value of type "
              , HH.code_ [ HH.text actual ]
              , HH.text " but a "
              , HH.span_ $ renderList [HH.text ", "] [HH.text " or "] $ map (\ty -> [ HH.code_ [ HH.text ty ] ]) expected
              , HH.text " value was expected."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and edit the query to fix this error." ]
          ]
    where
      renderParseValueError field ty error value =
        HH.div_
          $ join
            [ pure $ errorTitle [ HH.text "There was a problem populating a field based on a SQL² query." ]
            , pure $
                HH.p_
                  [ HH.text ("Parsing a " <> ty <> " value for the field ")
                  , HH.code_ [ HH.text field ]
                  , HH.text " failed with error:"
                  ]
            , pure $ HH.pre_ [ HH.text error ]
            , pure $ HH.p_ [ HH.text "When trying to parse the query result value:" ]
            , pure $ HH.pre_ [ HH.text value ]
            , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and edit the query to fix this error." ]
            ]

downloadOptionsErrorMessage ∷ State → CDOE.DownloadOptionsError → HTML
downloadOptionsErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName DownloadOptions <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CDOE.DownloadOptionsFilenameRequired →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "No filename was provided in the " <> cardName DownloadOptions <> " card." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]
    CDOE.DownloadOptionsFilenameInvalid fn →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "The filename provided in the " <> cardName DownloadOptions <> " card is invalid." ]
          , pure $ HH.p_
              [ HH.code_ [ HH.text fn ], HH.text " is not a valid filepath." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]

openErrorMessage ∷ State → COE.OpenError → HTML
openErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Open <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    COE.OpenFileNotFound fp →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "A file that was selected in the " <> cardName Open <> " card could not be found." ]
          , pure $ HH.p_
              [ HH.code_ [ HH.text fp ], HH.text " does not exist." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and make a new selection to fix this error." ]
          ]
    COE.OpenNoResourceSelected →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "No resource was selected in the " <> cardName Open <> " card." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select a file, directory or a variable to fix this error." ]
          ]
    COE.OpenNoFileSelected →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "The resource selected in the " <> cardName Open <> " card is of an invalid type" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select a file, directory or variable to fix this error." ]
          ]

tableErrorMessage ∷ State → CTE.TableError → HTML
tableErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Table <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CTE.TableMissingResourceInputError →
      HH.div_
        [ errorTitle [ HH.text $ "The " <> cardName Table <> " card requires data as an input." ] ]
    CTE.TableCountQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occured when counting the preview data." ]
          , pure $ printQErrorWithDetails qErr
          ]
    CTE.TableSampleQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occured during sampling of the preview data." ]
          , pure $ printQErrorWithDetails qErr
          ]

chartErrorMessage ∷ State → CChE.ChartError → HTML
chartErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Chart <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CChE.ChartMissingResourceInputError →
      HH.div_
        [ errorTitle [ HH.text $ "The " <> cardName Chart <> " card requires data as an input." ] ]
    CChE.ChartCountQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occured when counting the chart data." ]
          , pure $ printQErrorWithDetails qErr
          ]
    CChE.ChartSampleQuasarError qErr →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text "An error occured during sampling of the chart data." ]
          , pure $ printQErrorWithDetails qErr
          ]

formInputStaticErrorMessage ∷ State → CFISE.FormInputStaticError → HTML
formInputStaticErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (SetupFormInput FIT.Static) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CFISE.FIStaticNoAxis →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput FIT.Static) <> " card." ]
          , pure $ HH.p_
              [ HH.text "No axis was selected" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CFISE.FIStaticMissingAxis axis →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput FIT.Static) <> " card." ]
          , pure $ HH.p_
              [ HH.text "The selected axis was not present in the data." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]

formInputLabeledErrorMessage ∷ State → CFILE.FormInputLabeledError → HTML
formInputLabeledErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      let
        fit = extractType err
      in
        HH.div_
          [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (SetupFormInput fit) <> " card, please notify the author of this workspace." ]
          , collapsible "Error details" (renderDetails err) expanded
          ]
  where
  extractType = case _ of
    CFILE.FILabeledNoAxisError fit → fit
    CFILE.FILabeledEmptyResourceError fit → fit
    CFILE.FILabeledTooManyEntries { formInputType } → formInputType
    CFILE.FILabeledTooManySelected { formInputType } → formInputType
    CFILE.FILabeledNonUniqueLabelError fit _ → fit
  renderDetails = case _ of
    CFILE.FILabeledNoAxisError fit →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "No axis was selected" ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CFILE.FILabeledEmptyResourceError fit →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "The selected resource was empty." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]
    CFILE.FILabeledTooManyEntries { formInputType, maximum, entryCount } →
      let
        errorText =
          "The " <> FIT.print formInputType
          <> " form input can't take more than "
          <> show (FIT.maximumCountOfEntries formInputType)
          <> "entries, but there were: "
          <> show entryCount
          <> ". Please use 'limit' or 'group by'"
      in
        HH.div_
          $ join
            [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput formInputType) <> " card." ]
            , pure $ HH.p_
                [ HH.text errorText ]
            , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
            ]
    CFILE.FILabeledTooManySelected { formInputType, maximum, selectedCount } →
      let
        errorText =
          "The " <> FIT.print formInputType
          <> " form input can't have more than "
          <> show (FIT.maximumCountOfSelectedValues formInputType)
          <> " selected values, but there were: "
          <> show selectedCount
          <> ". Please, use another axis"
      in
        HH.div_
          $ join
            [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput formInputType) <> " card." ]
            , pure $ HH.p_
                [ HH.text errorText ]
            , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
            ]
    -- TODO: Use the label argument for a better error message
    CFILE.FILabeledNonUniqueLabelError fit label →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occured when setting up the " <> cardName (SetupFormInput fit) <> " card." ]
          , pure $ HH.p_
              [ HH.text "Labels must be unique. Please, use other axis." ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select an axis to fix this error." ]
          ]

searchErrorMessage ∷ State → CSE.SearchError → HTML
searchErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Search <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CSE.SearchQueryParseError { query, error } →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "Failed to parse the query when setting up the " <> cardName Search <> " card." ]
          , pure $ HH.p_
              [ HH.text "Failed with the following parse error: "
              , HH.code_ [HH.text (parseErrorMessage error)]
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and correct your query to fix this error." ]
          ]
    CSE.SearchQueryCompilationError error →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "Failed to compile the query when setting up the " <> cardName Search <> " card." ]
          , pure $ HH.p_
              [ HH.text "Failed with the following compilation error: "
              , printQErrorWithDetails error
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and correct your query to fix this error." ]
          ]

pivotTableErrorMessage ∷ State → CPTE.PivotTableError → HTML
pivotTableErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName (ChartOptions PivotTable) <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails = case _ of
    CPTE.PivotTableNoColumnSelectedError →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "Encountered an error when setting up the " <> cardName (ChartOptions PivotTable) <> " card." ]
          , pure $ HH.p_
              [ HH.text "No column was selected to be displayed."
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card and select a column to fix this error." ]
          ]
    CPTE.PivotTableQuasarError error →
      HH.div_
        $ join
          [ pure $ errorTitle [ HH.text $ "An error occurred in the " <> cardName (ChartOptions PivotTable) <> " card." ]
          , pure $ HH.p_
              [ HH.text "The Quasar Analytics engine returned the following error:"
              , printQErrorWithDetails error
              ]
          , guard (accessType == Editable) $> HH.p_ [ HH.text "Go back to the previous card to fix this error." ]
          ]

variablesErrorMessage ∷ State → CVE.VariablesError → HTML
variablesErrorMessage { accessType, expanded } err =
  case accessType of
    Editable → renderDetails err
    ReadOnly →
      HH.div_
        [ HH.p_ [ HH.text $ "A problem occurred in the " <> cardName Variables <> " card, please notify the author of this workspace." ]
        , collapsible "Error details" (renderDetails err) expanded
        ]
  where
  renderDetails (CVE.VariablesError nel)
    | L.null (NEL.tail nel) =
        HH.div_
          [ errorTitle [ HH.text $ "There was a problem in the configuration of the " <> cardName Variables <> " card." ]
          , renderError (NEL.head nel)
          ]
    | otherwise =
        HH.div_
          [ errorTitle [ HH.text $ "There were multiple problems in the configuration of the " <> cardName Variables <> " card." ]
          , HH.ul_ $ map (\msg → HH.li_ [ renderError msg ]) (A.fromFoldable nel)
          ]
  renderError = case _ of
    CVE.DefaultValueError fieldName err' →
      HH.div_
        [ HH.p_
            [ HH.text "The default value for the variable "
            , HH.code_ [ HH.text (show (unwrap fieldName)) ]
            , HH.text " failed to parse:"
            ]
        , HH.pre_ [ HH.text (unwrap err') ]
        ]
    CVE.URLValueError fieldName err' →
      HH.div_
        [ HH.p_
            [ HH.text "The URL-specified value for the variable "
            , HH.code_ [ HH.text (show (unwrap fieldName)) ]
            , HH.text " failed to parse:"
            ]
        , HH.pre_ [ HH.text (unwrap err') ]
        ]
    CVE.DuplicateVariableError fieldName →
      HH.p_
        [ HH.text "A variable named "
        , HH.code_ [ HH.text (show (unwrap fieldName)) ]
        , HH.text " was specified more than once."
        ]
    CVE.InvalidVariableNameError fieldName →
      HH.p_
        [ HH.text "Invalid variable name "
        , HH.code_ [ HH.text (show (unwrap fieldName)) ]
        , HH.text "."
        ]

-- | Renders a list of things with a separator and a final connective.
-- | For example, a list of strings with the separator `","` and the
-- | connective `" or "``: `a, b, c, or d`. The separator is dropped when there
-- | are only two entries; `a or b`.
renderList ∷ ∀ m. Monoid m ⇒ m → m → NEL.NonEmptyList m → m
renderList sep connective xs = case NEL.init xs of
  L.Nil → NEL.head xs
  x : L.Nil → x <> connective <> NEL.last xs
  init → intercalate sep init <> sep <> connective <> NEL.last xs
