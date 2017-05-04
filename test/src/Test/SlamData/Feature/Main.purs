module Test.SlamData.Feature.Main where

import SlamData.Prelude

import Control.Monad.Aff (Aff, launchAff, runAff, apathize, attempt)
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar, AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Reattempt (reattempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Ref (REF, newRef, modifyRef, readRef)
import Control.Monad.Reader.Trans (runReaderT)

import Data.Array as Arr
import Data.Int as Int
import Data.Posix.Signal (Signal(SIGTERM))
import Data.String as S
import Data.Time.Duration (Milliseconds(..))

import DOM (DOM)

import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff (mkdir, unlink)
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable, Duplex, pipe, onClose)

import Quasar.Spawn.Util.Process (spawnQuasar, spawnQuasarInit)
import Quasar.Spawn.Util.Starter (expectStdOut, starter)

import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build)
import Selenium.Monad (setWindowSize)
import Selenium.Types (SELENIUM)

import Test.Feature.Log as Log
import Test.Feature.Monad (FeatureEffects)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Config (Config)
import Test.SlamData.Feature.Effects (SlamFeatureEffects)
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Test.File as File
import Test.SlamData.Feature.Test.FlexibleVisualation as FlexibleVisualization
import Test.SlamData.Feature.Test.Markdown as Markdown
import Test.SlamData.Feature.Test.Search as Search
import Test.SlamData.Feature.Test.CacheCard as Cache
import Test.SlamData.Feature.Test.FlipDeck as FlipDeck
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


tests ∷ SlamFeature Unit
tests = do
  let setupScenario = scenario "Setup" (pure unit)
  setupScenario (pure unit) "Launch SlamData" [] do
    Interact.launchSlamData
    Log.successMsg "Ok, launched SlamData"

  setupScenario (pure unit) "Mount test database" [] do
    Interact.skipGuide
    Interact.mountTestDatabase
    Log.successMsg "Ok, mounted test database"

  setupScenario
    (Interact.deleteFileInTestFolder "Untitled Workspace.slam")
    "Skip guides and dismiss hints"
    []
    do
      Interact.browseTestFolder
      Interact.createWorkspace
      Interact.skipGuide
      Interact.dismissHint
      Interact.insertOpenCardInLastDeck
      Interact.dismissHint
      Interact.flipDeck
      Interact.skipGuide
      Log.successMsg "Ok, skipped guides and dismiss hints"

  File.test
  Search.test
  Markdown.test
  FlexibleVisualization.test
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

mongoArgs ∷ Config → Array String
mongoArgs config =
  [ "--port", show config.mongodb.port
  , "--dbpath", "tmp/data"
  ]

mongoConnectionString ∷ Config → String
mongoConnectionString config =
  "mongodb://"
  ⊕ config.mongodb.host
  ⊕ ":" ⊕ show config.mongodb.port
  ⊕ "/" ⊕ config.database.name

cleanMkDir ∷ String → Aff Effects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

restoreDatabase ∷ Config → Aff Effects Unit
restoreDatabase rawConfig = do
  var ← makeVar
  liftEff $ CP.exec
    rawConfig.restoreCmd
    CP.defaultExecOptions
    (void <<< launchAff <<< putVar var)
  res ← takeVar var
  traverse_ throwError res.error


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
    copyFile
      "test/quasar-config.json"
      "tmp/test/quasar-config.json"

    spawnQuasarInit
      (resolve ["tmp", rawConfig.quasar.config] "")
      (resolve [rawConfig.quasar.jar] "")

    mongo ←
      startProc
        "MongoDB"
        "mongod"
        (mongoArgs rawConfig)
        (expectStdOut "waiting for connections on port")
    liftEff $ modifyRef procs (Arr.cons mongo)

    quasar ←
      spawnQuasar
        (resolve ["tmp", rawConfig.quasar.config] "")
        (resolve [rawConfig.quasar.jar] "")
        ("-C " <> resolve ["public"] "")
    liftEff $ modifyRef procs (Arr.cons quasar)

    log $ gray "Restoring database"
    restoreDatabase rawConfig
    log $ gray "Database restored"

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
