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

module SlamData.Download.Model where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), (.?), jsonEmptyObject)
import Data.Lens (Lens', lens)
import Data.MediaType (MediaType(..))
import Data.String as Str
import Data.String.Regex as Rx
import Data.String.Regex.Flags as RXF

import Global as Global

import Network.HTTP.RequestHeader (RequestHeader(..))

type DownloadOptions = Either CSVOptions JSONOptions

type CSVOptionsRec =
  { colDelimiter :: String
  , rowDelimiter :: String
  , quoteChar :: String
  , escapeChar :: String
  , arrays :: ArrayMode
  }
newtype CSVOptions = CSVOptions CSVOptionsRec

derive instance eqCSVOptions :: Eq CSVOptions

instance encodeJsonCSVOptions :: EncodeJson CSVOptions where
  encodeJson (CSVOptions r) =
       "colDelimiter" := r.colDelimiter
    ~> "rowDelimiter" := r.rowDelimiter
    ~> "quoteChar" := r.quoteChar
    ~> "escapeChar" := r.escapeChar
    ~> "arrays" := r.arrays
    ~> jsonEmptyObject

instance decodeJsonCSVOptions :: DecodeJson CSVOptions where
  decodeJson json = do
    obj <- decodeJson json
    r <- { colDelimiter: _
         , rowDelimiter: _
         , quoteChar: _
         , escapeChar: _
         , arrays: _
         }
         <$> (obj .? "colDelimiter")
         <*> (obj .? "rowDelimiter")
         <*> (obj .? "quoteChar")
         <*> (obj .? "escapeChar")
         <*> (obj .? "arrays")
    pure $ CSVOptions r

initialCSVOptions :: CSVOptions
initialCSVOptions =
  CSVOptions { colDelimiter: ","
             , rowDelimiter: "\\n"
             , quoteChar: "\""
             , escapeChar: "\""
             , arrays: Flatten
             }

_CSVOptions :: Lens' CSVOptions CSVOptionsRec
_CSVOptions = lens (\(CSVOptions obj) -> obj) (const CSVOptions)

_colDelimiter :: Lens' CSVOptions String
_colDelimiter = _CSVOptions <<< lens _.colDelimiter (_ { colDelimiter = _ })

_rowDelimiter :: Lens' CSVOptions String
_rowDelimiter = _CSVOptions <<< lens _.rowDelimiter (_ { rowDelimiter = _ })

_quoteChar :: Lens' CSVOptions String
_quoteChar = _CSVOptions <<< lens _.quoteChar (_ { quoteChar = _ })

_escapeChar :: Lens' CSVOptions String
_escapeChar = _CSVOptions <<< lens _.escapeChar (_ { escapeChar = _ })

_arrays :: Lens' CSVOptions ArrayMode
_arrays = _CSVOptions <<< lens _.arrays (_ { arrays = _ })

type JSONOptionsRec =
  { multivalues :: MultiValueMode
  , precision :: PrecisionMode
  }
newtype JSONOptions = JSONOptions JSONOptionsRec

derive instance eqJsonOptions :: Eq JSONOptions

instance encodeJsonJSONOptions :: EncodeJson JSONOptions where
  encodeJson (JSONOptions r) =
       "multivalues" := r.multivalues
    ~> "precision" := r.precision
    ~> jsonEmptyObject

instance decodeJsonJSONOptions :: DecodeJson JSONOptions where
  decodeJson json = do
    obj <- decodeJson json
    r <- { multivalues: _, precision: _ }
         <$> (obj .? "multivalues")
         <*> (obj .? "precision")
    pure $ JSONOptions r

initialJSONOptions :: JSONOptions
initialJSONOptions = JSONOptions
  { multivalues: ArrayWrapped
  , precision: Readable
  }

_JSONOptions :: Lens' JSONOptions JSONOptionsRec
_JSONOptions = lens (\(JSONOptions obj) -> obj) (const JSONOptions)

_multivalues :: Lens' JSONOptions MultiValueMode
_multivalues = _JSONOptions <<< lens _.multivalues (_ { multivalues = _ })

_precision :: Lens' JSONOptions PrecisionMode
_precision = _JSONOptions <<< lens _.precision (_ { precision = _ })

data ArrayMode = Flatten | Separate String

derive instance eqArrayMode :: Eq ArrayMode

instance encodeJsonArrayMode :: EncodeJson ArrayMode where
  encodeJson Flatten =
    "ty" := "flatten"
    ~> "val" := ""
    ~> jsonEmptyObject
  encodeJson (Separate str) =
    "ty" := "separate"
    ~> "val" := str
    ~> jsonEmptyObject

instance decodeJsonArrayMode :: DecodeJson ArrayMode where
  decodeJson json = do
    obj <- decodeJson json
    ty <- obj .? "ty"
    val <- obj .? "val"
    case ty of
      "flatten" -> Right Flatten
      "separate" -> Right $ Separate val
      _ -> Left "Incorrect ArrayMode encoding"

separateValue :: ArrayMode -> Maybe String
separateValue (Separate s) = Just s
separateValue _ = Nothing

data MultiValueMode = ArrayWrapped | LineDelimited
data PrecisionMode = Readable | Precise

derive instance eqMultiValueMode :: Eq MultiValueMode

derive instance eqPrecisionMode :: Eq PrecisionMode

instance encodeJsonMultivalueMode :: EncodeJson MultiValueMode where
  encodeJson ArrayWrapped = encodeJson "ArrayWrapped"
  encodeJson LineDelimited = encodeJson "LineDelimited"

instance encodeJsonPrecision :: EncodeJson PrecisionMode where
  encodeJson Readable = encodeJson "Readable"
  encodeJson Precise = encodeJson "Precise"

instance decodeJsonMultiValueMode :: DecodeJson MultiValueMode where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "ArrayWrapped" -> pure ArrayWrapped
      "LineDelimited" -> pure LineDelimited
      _ -> Left "Incorrect MultiValueMode"

instance decodeJsonPrecision :: DecodeJson PrecisionMode where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "Readable" -> pure Readable
      "Precise" -> pure Precise
      _ -> Left "Incorrect Precision"

extension :: Boolean -> Either CSVOptions JSONOptions -> String
extension compress options
  | compress = ".zip"
  | otherwise = case options of
      Right (JSONOptions { multivalues: LineDelimited }) →  ".ldjson"
      Right _ → ".json"
      Left _ → ".csv"

validFilename ∷ String → Either String String
validFilename s =
  if isJust (Str.indexOf (Str.Pattern "/") s)
    then Left s
    else Right s

toHeaders
  :: forall r
   . { compress :: Boolean
     , options :: Either CSVOptions JSONOptions
     | r
     }
  -> Maybe String
  -> Array RequestHeader
toHeaders r filename =
  [ RequestHeader "Accept-Encoding" "gzip"
  , Accept $ MediaType $ mimeType r.options <> ";disposition=\"attachment" <> encFilename <> "\""
  ]
  where
  encFilename :: String
  encFilename = case filename of
    Nothing -> ""
    Just fn -> "; filename*=UTF-8''" <> Global.encodeURIComponent fn

  mimeType :: Either CSVOptions JSONOptions -> String
  mimeType (Left (CSVOptions opts)) =
    "text/csv"
    <> ";columnDelimiter=" <> esc opts.colDelimiter
    <> (if opts.rowDelimiter == "\\n"
        then ""
        else ";rowDelimiter=" <> esc opts.rowDelimiter)
    <> ";quoteChar=" <> esc opts.quoteChar
    <> ";escapeChar=" <> esc opts.escapeChar
  mimeType (Right (JSONOptions opts)) =
    let suffix = if opts.precision == Precise then ";mode=precise" else ""
        subtype = if opts.multivalues == ArrayWrapped then "json" else "ldjson"
    in "application/" <> subtype <> suffix
  esc :: String -> String
  esc s =
    (\a -> "\"" <> a <> "\"")
    $ Rx.replace (grx "\"") "\\\""
    $ Str.replace (Str.Pattern "\\t") (Str.Replacement "\t")
    $ Str.replace (Str.Pattern "\\r") (Str.Replacement "\r")
    s
    where
    grx :: String -> Rx.Regex
    grx pat = unsafePartial fromRight $ Rx.regex pat RXF.global

data OutputType = CSV | JSON
