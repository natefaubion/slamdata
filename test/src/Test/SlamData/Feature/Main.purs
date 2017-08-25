module Test.SlamData.Feature.Main where

import SlamData.Prelude

import Control.Monad.Aff (Aff, runAff, apathize, attempt)
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar, AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Reattempt (reattempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Ref (REF, newRef, readRef)
import Control.Monad.Reader.Trans (runReaderT)
import DOM (DOM)
import Data.Int as Int
import Data.Posix.Signal (Signal(SIGTERM))
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff (mkdir, unlink)
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable, Duplex, pipe, onClose)
import Quasar.Spawn.Util.Starter (starter)
import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build)
import Selenium.Monad (setWindowSize)
import Selenium.Types (SELENIUM)
import Test.Feature.Log as Log
import Test.Feature.Monad (FeatureEffects)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Config (Config)
import Test.SlamData.Feature.Effects (SlamFeatureEffects)
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (Connector(Marklogic, Couchbase), SlamFeature, getConnector)
import Test.SlamData.Feature.Test.CacheCard as Cache
import Test.SlamData.Feature.Test.File as File
--import Test.SlamData.Feature.Test.FlexibleVisualation as FlexibleVisualization
import Test.SlamData.Feature.Test.FlipDeck as FlipDeck
import Test.SlamData.Feature.Test.ImportExport as ImportExport
import Test.SlamData.Feature.Test.Markdown as Markdown
import Test.SlamData.Feature.Test.Search as Search
import Text.Chalky (green, yellow, gray, red)

foreign import getConfig ∷ ∀ e. Eff (fs ∷ FS|e) Config
foreign import createReadStream
  ∷ ∀ e. String → Aff e (Readable () e)
foreign import createWriteStream
  ∷ ∀ e. String → Aff e (Duplex e)
foreign import stack
  ∷ Error → String

type Effects =
  SlamFeatureEffects (FeatureEffects
    ( ref ∷ REF
    , console ∷ Ec.CONSOLE
    , dom ∷ DOM
    , selenium ∷ SELENIUM
    ))

setupScenario
  ∷ SlamFeature Unit
    → String
    → KnownIssues
    → SlamFeature Unit
    → SlamFeature Unit
setupScenario after scenarioName knownIssues implementation = do
  connector <- getConnector
  scenario
    { epic: "Setup"
    , before: (pure unit)
    , after: after
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

tests ∷ SlamFeature Unit
tests = do
  setupScenario (pure unit) "Launch SlamData" noIssues do
    Interact.launchSlamData
    Log.successMsg "** Ok, launched SlamData **"

  setupScenario (pure unit) "Mount test database" noIssues do
    connector ← getConnector
    Interact.skipGuide
    Interact.mountTestDatabase
    case connector of
      Couchbase → Interact.setupCouchbase
      Marklogic → Interact.setupMarklogic
      _ → pure unit
    Log.successMsg "** Ok, mounted test database **"

  setupScenario
    (Interact.deleteFileInTestFolder "Untitled Workspace.slam")
    "Skip guides and dismiss hints"
    noIssues
    do
      Interact.browseTestFolder
      Interact.createWorkspace
      Interact.skipGuide
      Interact.dismissHint
      Interact.insertOpenCardInLastDeck
      Interact.dismissHint
      Interact.flipDeck
      Interact.skipGuide
      Log.successMsg "** Ok, skipped guides and dismiss hints **"

  File.test
  ImportExport.tests
  Search.test
  Markdown.test
  -- Broken in PR #2112 - ECharts default theme color
  --FlexibleVisualization.test
  Cache.test
  FlipDeck.test

runTests ∷ Config → Aff Effects Unit
runTests config = do
  driver ← build do
    withCapabilities $ browserCapabilities Chrome

  let
    defaultTimeout = Milliseconds $ Int.toNumber config.selenium.waitTime
    readerInp = { config, defaultTimeout, driver }

  res ← attempt $ flip runReaderT readerInp do
    setWindowSize { height: 800, width: 1024 }
    tests

  apathize $ quit driver
  either throwError (const $ pure unit) res

copyFile
  ∷ ∀ e
  . String
  → String
  → Aff (fs ∷ FS, avar ∷ AVAR, exception ∷ EXCEPTION|e) Unit
copyFile source tgt = do
  apathize $ unlink to
  readFrom ← createReadStream from
  writeTo ← createWriteStream to
  knot ← makeVar

  liftEff
    $ onClose writeTo
    $ void
    $ runAff
        (const $ pure unit)
        (const $ pure unit)
        (putVar knot unit)

  _ ← liftEff $ readFrom `pipe` writeTo
  takeVar knot
  where
  from = resolve [source] ""
  to = resolve [tgt] ""

procStartMaxTimeout ∷ Milliseconds
procStartMaxTimeout = Milliseconds 60000.0

startProc
  ∷ String
  → String
  → Array String
  → (Either String String → Maybe (Either String Unit))
  → Aff Effects CP.ChildProcess
startProc name command args check
  = reattempt procStartMaxTimeout
  $ starter name check
  $ liftEff
  $ CP.spawn command args CP.defaultSpawnOptions

cleanMkDir ∷ String → Aff Effects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

main ∷ Eff Effects Unit
main = do
  procs ← newRef []

  Process.onExit \_ → readRef procs >>= traverse_ (CP.kill SIGTERM)

  rawConfig ← getConfig

  void $ runAff errHandler (const $ Process.exit 0) do
    log $ gray "Creating data folder for MongoDB"
    cleanMkDir "tmp/data"
    log $ gray "Emptying test folder"
    cleanMkDir "tmp/test"
    cleanMkDir "tmp/test/image"
    cleanMkDir "tmp/test/downloads"

    log $ yellow "Starting tests"
    testResults
      ← attempt
        $ runTests rawConfig
            { download = rawConfig.download
              { folder = resolve [ rawConfig.download.folder ] "" }
              , upload = rawConfig.upload
                  { filePaths =
                       map (\x → resolve [ x ] "") rawConfig.upload.filePaths }
              }
    case testResults of
      Left e →  throwError e
      Right _ → log $ green "OK, tests are passed"

  where
  errHandler e = do
    Ec.log $ red $ message e
    traverse_ (Ec.log ∘ red) $ S.split (S.Pattern "\n") $ stack e
    Process.exit 1
