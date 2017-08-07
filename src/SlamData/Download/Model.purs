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

module SlamData.Download.Model
  ( module SlamData.Download.Model
  , module Exports
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec ((>~>))
import Data.Codec as C
import Data.Codec.Argonaut.Compat as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Lens (Lens')
import Data.Lens.Record as LR
import Data.MediaType (MediaType(..))
import Data.Newtype (over)
import Data.Path.Pathy as P
import Data.String as Str
import Global as Global
import Network.HTTP.RequestHeader (RequestHeader(..))
import Quasar.Data.CSV as CSV
import Quasar.Data.Json (PrecisionMode(..)) as Exports
import Quasar.Data.Json as JSON
import Quasar.Data.MediaTypes as QMT
import Quasar.Paths as QP
import SlamData.FileSystem.Resource as R
import SlamData.Quasar as Q

type DownloadModel r =
  { targetName ∷ String
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  | r
  }

type DownloadOptions = Either CSV.Options JSON.Options

initialOptions ∷ R.Resource → Either CSV.Options JSON.Options
initialOptions res
  | R.isWorkspace res = Right initialJSONOptions
  | otherwise = Left CSV.defaultOptions

renderURL ∷ ∀ r. Array RequestHeader → DownloadModel (resource ∷ R.Resource | r) → String
renderURL authHeaders opts =
  let
    headers =
      "?request-headers="
        <> (Global.encodeURIComponent
             $ show
             $ Q.reqHeadersToJSON
             $ append authHeaders
             $ toHeaders opts (shouldCompress opts)
             $ Just (opts.targetName <> ext))
    ext = extension (shouldCompress opts) opts.options
  in
    Q.encodeURI (P.printPath QP.data_ <> R.resourcePath opts.resource) <> headers

codecCSVOptions ∷ CA.JsonCodec CSV.Options
codecCSVOptions =
  migrationCodec >~> (_Newtype $ CA.object "CSV Options" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "columnDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "rowDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "quoteChar") CA.string
    # CA.recordProp (SProxy ∷ SProxy "escapeChar") CA.string)
  where
    -- Added in 4.2.5
    migrationCodec = CAM.renameField "colDelimiter" "columnDelimiter"

_colDelimiter ∷ Lens' CSV.Options String
_colDelimiter = _Newtype ∘ LR.prop (SProxy ∷ SProxy "columnDelimiter")

_rowDelimiter ∷ Lens' CSV.Options String
_rowDelimiter = _Newtype ∘ LR.prop (SProxy ∷ SProxy "rowDelimiter")

_quoteChar ∷ Lens' CSV.Options String
_quoteChar = _Newtype ∘ LR.prop (SProxy ∷ SProxy "quoteChar")

_escapeChar ∷ Lens' CSV.Options String
_escapeChar = _Newtype ∘ LR.prop (SProxy ∷ SProxy "escapeChar")

codecJSONOptions ∷ CA.JsonCodec JSON.Options
codecJSONOptions =
  migrationCodec >~> (_Newtype $ CA.object "JSON Options" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "encoding") codecEncodingStyle
    # CA.recordProp (SProxy ∷ SProxy "precision") codecPrecisionMode)
  where
    -- Added in 4.2.5
    migrationCodec = CAM.renameField "multivalues" "encoding"

initialJSONOptions ∷ JSON.Options
initialJSONOptions =
  JSON.Options
    { encoding: JSON.Array
    , precision: JSON.Readable
    }

_encoding ∷ Lens' JSON.Options JSON.EncodingStyle
_encoding = _Newtype ∘ LR.prop (SProxy ∷ SProxy "encoding")

_precision ∷ Lens' JSON.Options JSON.PrecisionMode
_precision = _Newtype ∘ LR.prop (SProxy ∷ SProxy "precision")

-- TODO-codec: replace with generic-based codec?
codecEncodingStyle ∷ CA.JsonCodec JSON.EncodingStyle
codecEncodingStyle = C.basicCodec dec enc
  where
  dec j = case J.toString j of
    Just "ArrayWrapped" → Right JSON.Array
    Just "LineDelimited" → Right JSON.LineDelimited
    _ → Left (CA.UnexpectedValue j)
  enc = J.fromString ∘ case _ of
   JSON.Array → "ArrayWrapped"
   JSON.LineDelimited → "LineDelimited"

-- TODO-codec: replace with generic-based codec?
codecPrecisionMode ∷ CA.JsonCodec JSON.PrecisionMode
codecPrecisionMode = C.basicCodec dec enc
  where
  dec j = case J.toString j of
    Just "Readable" → Right JSON.Readable
    Just "Precise" → Right JSON.Precise
    _ → Left (CA.UnexpectedValue j)
  enc = J.fromString ∘ case _ of
    JSON.Readable → "Readable"
    JSON.Precise → "Precise"

alwaysCompress ∷ R.Resource → Boolean
alwaysCompress = not R.isFile

shouldCompress ∷ ∀ r. DownloadModel (resource ∷ R.Resource | r) → Boolean
shouldCompress r = alwaysCompress r.resource || r.compress

extension ∷ Boolean → Either CSV.Options JSON.Options → String
extension compress options
  | compress = ".zip"
  | otherwise = case options of
      Right (JSON.Options { encoding: JSON.LineDelimited }) →  ".ldjson"
      Right _ → ".json"
      Left _ → ".csv"

validFilename ∷ String → Either String String
validFilename s =
  if not Str.null s && isJust (Str.indexOf (Str.Pattern "/") s)
    then Left s
    else Right s

toHeaders
  ∷ ∀ r
  . DownloadModel r
  → Boolean
  → Maybe String
  → Array RequestHeader
toHeaders r compress filename =
  [ RequestHeader "Accept-Encoding" "gzip"
  , Accept $ attachify filename (compressify (mimeType r.options))
  ]
  where
    mimeType ∷ Either CSV.Options JSON.Options → MediaType
    mimeType = either CSV.toMediaType JSON.toMediaType

    compressify ∷ MediaType → MediaType
    compressify = if compress then QMT.zipped else id

    attachify ∷ Maybe String → MediaType → MediaType
    attachify name =
      let name' = maybe "" (\n → "; filename*=UTF-8''" <> Global.encodeURIComponent n) name
      in over MediaType (\mt → mt <> ";disposition=\"attachment" <> name' <> "\"")

data OutputType = CSV | JSON

derive instance eqOutputType ∷ Eq OutputType
