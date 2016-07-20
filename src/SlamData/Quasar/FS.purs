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

module SlamData.Quasar.FS
  ( children
  , transitiveChildrenProducer
  , getNewName
  , move
  , listing
  , delete
  , messageIfFileNotFound
  , dirNotAccessible
  , fileNotAccessible
  ) where

import SlamData.Prelude

import Control.Coroutine as CR
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class as MR
import Control.Monad.Free.Trans as FT

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens ((.~), (^.), (^?), _Left)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as Set
import Data.String as S

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (lowerQError)
import Quasar.FS as QFS
import Quasar.FS.Resource as QR
import Quasar.Types (AnyPath, DirPath, FilePath)

import SlamData.Quasar.Aff (QEff, runQuasarF)
import SlamData.Config as Config
import SlamData.FileSystem.Resource as R

import Utils.AffableProducer as AP
import Utils.Completions (memoizeCompletionStrs)

children
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ DirPath
  → m (Either Exn.Error (Array R.Resource))
children dir = runExceptT do
  cs ← ExceptT $ listing dir
  let result = (R._root .~ dir) <$> cs
  lift $ fromAff $ memoizeCompletionStrs dir result
  pure result

-- | Produces a stream of the transitive children of a path
transitiveChildrenProducer
  ∷ ∀ eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ DirPath
  → CR.Producer (Array R.Resource) m Unit
transitiveChildrenProducer dirPath = do
  AP.produce \emit → do
    activeRequests ← Ref.newRef $ Set.singleton $ P.printPath dirPath
    Aff.runAff Exn.throwException (const (pure unit)) $ go emit activeRequests dirPath
  where
  go emit activeRequests start = do
    let strPath = P.printPath start
    eitherChildren ← children start
    liftEff $ Ref.modifyRef activeRequests $ Set.delete strPath
    for_ eitherChildren \items → do
      liftEff $ emit $ Left items
      let parents = Arr.mapMaybe (either Just (const Nothing) ∘ R.getPath) items
      for_ parents $ \p →
        liftEff $ Ref.modifyRef activeRequests $ Set.insert $ P.printPath p
      for_ parents $ go emit activeRequests
    remainingRequests ← liftEff $ Ref.readRef activeRequests
    when (Set.isEmpty remainingRequests) $
      liftEff ∘ emit $ Right unit

listing
  ∷ ∀ eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ DirPath
  → m (Either Exn.Error (Array R.Resource))
listing p =
  bimap lowerQError (map toResource) <$> runQuasarF (QF.dirMetadata p)
  where
  toResource ∷ QFS.Resource → R.Resource
  toResource res = case res of
    QFS.File path → R.File path
    QFS.Directory path →
      let workspaceName
            = S.stripSuffix ("." <> Config.workspaceExtension)
            ∘ P.runDirName =<< P.dirName path
      in case workspaceName of
        Just name → R.Workspace (p </> P.dir name)
        Nothing → R.Directory path
    QFS.Mount (QFS.MongoDB path) → R.Mount (R.Database path)
    QFS.Mount (QFS.View path) → R.Mount (R.View path)

-- | Generates a new resource name based on a directory path and a name for the
-- | resource. If the name already exists in the path a number is appended to
-- | the end of the name.
getNewName
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ DirPath
  → String
  → m (Either Exn.Error String)
getNewName parent name = do
  result ← runQuasarF (QF.dirMetadata parent)
  pure case result of
    Left (QF.Error err) → Left err
    Right items | exists name items → Right (getNewName' items 1)
    _ → Right name
  where

  getNewName' ∷ Array QFS.Resource → Int → String
  getNewName' items i =
    let arr = S.split "." name
    in fromMaybe "" do
      body ← Arr.head arr
      suffixes ← Arr.tail arr
      let newName = S.joinWith "." $ Arr.cons (body <> " " <> show i) suffixes
      pure if exists newName items
           then getNewName' items (i + one)
           else newName

  exists ∷ String → Array QFS.Resource → Boolean
  exists name = F.any ((_ == name) ∘ printName ∘ QR.getName)

  printName ∷ Either (Maybe P.DirName) P.FileName → String
  printName = either (fromMaybe "" ∘ map P.runDirName) P.runFileName

-- | Will return `Just` in case the resource was successfully moved, and
-- | `Nothing` in case no resource existed at the requested source path.
move
  ∷ ∀ eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ R.Resource
  → AnyPath
  → m (Either Exn.Error (Maybe AnyPath))
move src tgt = do
  let srcPath = R.getPath src
  runExceptT ∘ traverse cleanViewMounts $ srcPath ^? _Left
  result ←
    runQuasarF case src of
      R.Mount _ → QF.moveMount srcPath tgt
      _ → QF.moveData srcPath tgt
  pure
    case result of
      Right _ → Right (Just tgt)
      Left QF.NotFound → Right Nothing
      Left (QF.Error err) → Left err
      Left QF.Forbidden → Right Nothing

delete
  ∷ ∀ eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ R.Resource
  → m (Either Exn.Error (Maybe R.Resource))
delete resource =
  runExceptT $
    if R.isMount resource || alreadyInTrash resource
    then
      forceDelete resource $> Nothing
    else
      moveToTrash resource `catchError` \(err ∷ Exn.Error) →
        forceDelete resource $> Nothing

  where
  msg ∷ String
  msg = "cannot delete"

  moveToTrash
    ∷ R.Resource
    → ExceptT Exn.Error m (Maybe R.Resource)
  moveToTrash res = do
    let
      d = (res ^. R._root) </> P.dir Config.trashFolder
      path = (res # R._root .~ d) ^. R._path
    name ← ExceptT $ getNewName d (res ^. R._name)
    ExceptT $ move res (path # R._nameAnyPath .~ name)
    pure ∘ Just $ R.Directory d

  alreadyInTrash ∷ R.Resource → Boolean
  alreadyInTrash res =
    case res ^. R._path of
      Left path → alreadyInTrash' path
      Right _ → alreadyInTrash' (res ^. R._root)

  alreadyInTrash' ∷ DirPath → Boolean
  alreadyInTrash' d =
    if d == P.rootDir
    then false
    else maybe false go $ P.peel d

    where
    go ∷ Tuple DirPath (Either P.DirName P.FileName) → Boolean
    go (Tuple d name) =
      case name of
        Right _ → false
        Left n →
          if n == P.DirName Config.trashFolder
          then true
          else alreadyInTrash' d

forceDelete
  ∷ ∀ eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ R.Resource
  → ExceptT Exn.Error m Unit
forceDelete res =
  case res of
    R.Mount _ →
      ExceptT ∘ runQuasarF $ lmap lowerQError <$>
        QF.deleteMount (R.getPath res)
    _ → do
      let path = R.getPath res
      traverse cleanViewMounts $ path ^? _Left
      ExceptT ∘ runQuasarF $ lmap lowerQError <$>
        QF.deleteData path

cleanViewMounts
  ∷ ∀ eff m
  . (Monad m, MR.MonadRec m, Affable (QEff eff) m)
  ⇒ DirPath
  → ExceptT Exn.Error m Unit
cleanViewMounts path =
  CR.runProcess (producer CR.$$ consumer)

  where
  producer ∷ CR.Producer (Array R.Resource) (ExceptT Exn.Error m) Unit
  producer =
    FT.hoistFreeT lift $
      transitiveChildrenProducer path

  consumer ∷ CR.Consumer (Array R.Resource) (ExceptT Exn.Error m) Unit
  consumer =
    CR.consumer \fs → do
      traverse_ deleteViewMount fs
      pure Nothing

  deleteViewMount
    ∷ R.Resource
    → ExceptT Exn.Error m Unit
  deleteViewMount =
    case _ of
      R.Mount (R.View vp) →
        ExceptT ∘ runQuasarF $
          lmap lowerQError <$>
            QF.deleteMount (Right vp)
      _ → pure unit

messageIfFileNotFound
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → String
  → m (Either Exn.Error (Maybe String))
messageIfFileNotFound path defaultMsg =
  handleResult <$> runQuasarF (QF.fileMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Either Exn.Error (Maybe String)
  handleResult (Left (QF.Error e)) = Left e
  handleResult (Left QF.NotFound) = Right (Just defaultMsg)
  handleResult (Left QF.Forbidden) = Right (Just defaultMsg)
  handleResult (Right _) = Right Nothing

dirNotAccessible
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ DirPath
  → m (Maybe String)
dirNotAccessible path =
  handleResult <$> runQuasarF (QF.dirMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Maybe String
  handleResult (Left (QF.Error e)) = Just $ Exn.message e
  handleResult (Left QF.NotFound) = Just "Target directory does not exist."
  handleResult (Left QF.Forbidden) = Just "Your browser isn't currently permitted to access that directory."
  handleResult (Right _) = Nothing

fileNotAccessible
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Maybe String)
fileNotAccessible path =
  handleResult <$> runQuasarF (QF.fileMetadata path)
  where
  handleResult ∷ ∀ a. Either QF.QError a → Maybe String
  handleResult (Left (QF.Error e)) = Just $ Exn.message e
  handleResult (Left QF.NotFound) = Just "Target file does not exist."
  handleResult (Left QF.Forbidden) = Just "Your browser isn't currently permitted to access that file."
  handleResult (Right _) = Nothing
