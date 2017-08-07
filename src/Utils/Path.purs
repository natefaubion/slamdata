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

module Utils.Path
  ( module Utils.Path
  , module Quasar.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (intersect, null, (:))
import Data.Bitraversable (bitraverse)
import Data.Char as Ch
import Data.Either (Either(..), either, fromRight)
import Data.List as L
import Data.Maybe (Maybe(..), maybe, fromMaybe, fromJust)
import Data.Path.Pathy (Sandboxed, Unsandboxed, Abs, Path, File, Rel, Dir, DirName(..), FileName(..), peel, rootDir, (</>), file, canonicalize, printPath, parseAbsDir, parseAbsFile, dir, relativeTo, renameDir)
import Data.Path.Pathy as P
import Data.String as S
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Global as Global
import Partial.Unsafe (unsafePartial)
import Quasar.Types (AnyPath, DirPath, FilePath)
import SlamData.Config as Config
import Text.SlamSearch.Parser.Tokens (keyChars)

type AnyFilePath = Either FilePath RelFilePath

type AnyDirPath = Either DirPath RelDirPath

type RelFilePath = P.RelFile P.Sandboxed

type RelDirPath = P.RelDir P.Sandboxed

infixl 6 renameDirExt as <./>

newtype Extension = Extension String

renameDirExt ∷ forall a s. Path a Dir s → String → Path a Dir s
renameDirExt p ext = renameDir (changeDirExt $ const ext) p

addDirExt ∷ forall a s. String → Path a Dir s → Path a Dir s
addDirExt ext = renameDir \(DirName d) → DirName $ dropExt (Extension ext) d <> "." <> ext

rootify ∷ DirPath → Path Rel Dir Sandboxed
rootify p = fromMaybe (dir "/") $ relativeTo p rootDir

rootifyFile ∷ FilePath → Path Rel File Sandboxed
rootifyFile p = fromMaybe (file "") $ relativeTo p rootDir

sandbox ∷ ∀ b. Path Abs b Unsandboxed → Maybe (Path Abs b Sandboxed)
sandbox = map (rootDir </> _) <<< P.sandbox rootDir

-- | Set by default to support cells without
-- | `pathToWorkspace` field. After first save `pathToWorkspace` is
-- | setted to correct workspace path.
phantomWorkspacePath ∷ DirPath
phantomWorkspacePath = rootDir </> dir "phantom" <./> Config.workspaceExtension

parseAnyPath ∷ String → Maybe AnyPath
parseAnyPath s
  = Right <$> parseFilePath s
  <|> Left <$> parseDirPath s

parseFilePath ∷ String → Maybe FilePath
parseFilePath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsFile s)

parseRelFilePath ∷ String → Maybe RelFilePath
parseRelFilePath = P.sandbox P.currentDir <=< P.parseRelFile

parseAnyFilePath ∷ String → Maybe AnyFilePath
parseAnyFilePath s = (Left <$> parseFilePath s) <|> (Right <$> parseRelFilePath s)

parseDirPath ∷ String → Maybe DirPath
parseDirPath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsDir s)

absOrRel ∷ ∀ a b c d r. (∀ e. P.Path e c d → r) → Either (P.Path a c d) (P.Path b c d) → r
absOrRel f = either f f

anyParentDir ∷ ∀ a. Either (P.Path Abs a Sandboxed) (P.Path Rel a Sandboxed) → Maybe AnyDirPath
anyParentDir = bitraverse P.parentDir P.parentDir

anyToAbs ∷ ∀ a b. Path Abs Dir b → Either (Path Abs a b) (Path Rel a b) → Path Abs a b
anyToAbs p = either id (p </> _)

changeDirExt ∷ (String → String) → DirName → DirName
changeDirExt f (DirName name) =
  DirName ((if ext == "" then name else n) <> "." <> f ext)
  where
  mbIdx = S.lastIndexOf (S.Pattern ".") name
  n = maybe name (\idx → S.take idx name) mbIdx
  ext = maybe "" (\idx → S.drop (idx + 1) name) mbIdx

dropDirExt ∷ DirName → DirName
dropDirExt (DirName d) =
  DirName $ maybe d (\idx → S.take idx d) $ S.lastIndexOf (S.Pattern ".") d

takeDirExt ∷ DirName → String
takeDirExt (DirName d) =
  maybe "" (\idx → S.drop (idx + 1) d) $ S.lastIndexOf (S.Pattern ".") d

dropExt ∷ Extension → String → String
dropExt (Extension ext) name =
  fromMaybe name do
    idx ← S.lastIndexOf (S.Pattern ".") name
    if S.drop (idx + 1) name == ext
      then pure $ S.take idx name
      else Nothing

dropWorkspaceExt ∷ String → String
dropWorkspaceExt = dropExt (Extension Config.workspaceExtension)

decodeURIPath ∷ String → String
decodeURIPath uri =
  Global.decodeURIComponent $
    Rgx.replace (unsafePartial fromRight $ Rgx.regex "\\+" RXF.global) " " uri

encodeURIPath ∷ String → String
encodeURIPath path =
  S.joinWith "/" $
  S.joinWith "+" <$>
  (Global.encodeURIComponent <$> _) <$>
  S.split (S.Pattern " ") <$>
  S.split (S.Pattern "/") path

hidePath ∷ String → String → String
hidePath path input =
  S.trim $
  S.replace (S.Pattern $ "+path:\"" <> path <> "\"") (S.Replacement "") $
  S.replace (S.Pattern $ "+path:" <> path) (S.Replacement "") input

renderPath ∷ AnyPath → String
renderPath ap =
  if null $ intersect (S.toCharArray rendered) ((Ch.fromCharCode 32):keyChars)
  then rendered
  else "\"" <> rendered <> "\""
  where
  rendered = either renderDir renderFile ap
  renderDir path =
    printPath $ canonicalize $ maybe rootDir (rootDir </> _) (P.sandbox rootDir path)

  renderFile path =
    printPath $ canonicalize $
    maybe (rootDir </> file "") (rootDir </> _) (P.sandbox rootDir path)

printAnyFilePath ∷ AnyFilePath → String
printAnyFilePath = either printPath printPath

anyFileName ∷ AnyFilePath → P.FileName
anyFileName = either P.fileName P.fileName

getNameStr ∷ AnyPath → String
getNameStr ap = either getNameStr' getNameStr' ap
  where
  getNameStr' ∷ forall b a s. Path a b s → String
  getNameStr' p = maybe "" (snd >>> nameOfFileOrDir) $ peel p

nameOfFileOrDir ∷ Either DirName FileName → String
nameOfFileOrDir (Left (DirName name)) = name
nameOfFileOrDir (Right (FileName name)) = name

getDir ∷ AnyPath → DirPath
getDir ap = either getDir' getDir' ap
  where
  getDir' ∷ forall b. Path Abs b Sandboxed → DirPath
  getDir' = maybe rootDir fst <<< peel

rootFile ∷ FilePath
rootFile = rootDir </> file ""

tmpDir ∷ RelDirPath
tmpDir = P.dir ".tmp"

parentDir ∷ P.RelDir Unsandboxed
parentDir = P.parentDir' P.currentDir

peelFile ∷ ∀ a s. P.Path a P.File s → Maybe (Tuple (P.Path a P.Dir s) P.FileName)
peelFile path = unsafePartial (map fromRight <$> peel path)

absToRelative ∷ ∀ a s. P.Path P.Abs a s → P.Path P.Rel a s
absToRelative p = unsafePartial $ fromJust $ P.relativeTo p P.rootDir

toPathList ∷ AnyPath → L.List AnyPath
toPathList res
  | Left P.rootDir == res = L.singleton (Left rootDir)
  | otherwise =
    (unfoldr \r → Tuple r <$> either go go r) res `L.snoc` Left rootDir
    where
    go ∷ ∀ b. Path Abs b Sandboxed → Maybe AnyPath
    go = map (Left <<< fst) <<< peel

dirPathAsFilePath ∷ DirPath -> Maybe FilePath
dirPathAsFilePath = P.peel >>> case _ of
  Nothing → Nothing
  Just (Tuple parentDir' peeled) → case peeled of
    Right _ → Nothing
    Left dirName → Just $ parentDir' </> (P.file $ P.runDirName dirName)
