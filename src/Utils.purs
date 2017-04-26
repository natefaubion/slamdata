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

module Utils where

import SlamData.Prelude

import Control.Monad.Except (Except)

import Data.Argonaut as J
import Data.Array as Array
import Data.Formatter.Number as FN
import Data.Int as Int
import Data.String as S
import Global (readFloat, isNaN, isFinite)
import SqlSquared.Signature.Ident (printIdent)

stringToNumber ∷ String → Maybe Number
stringToNumber s =
  if isNaN n || not isFinite n
    then Nothing
    else Just n
  where
  n = readFloat s

stringToBoolean ∷ String → Maybe Boolean
stringToBoolean "true" = Just true
stringToBoolean "false" = Just false
stringToBoolean _ = Nothing

stringToInt ∷ String → Maybe Int
stringToInt = map Int.floor ∘ stringToNumber

singletonValue' ∷ ∀ a m. (Foldable m) ⇒ m a → Either Int (Maybe a)
singletonValue' =
  foldr f initial
  where
  f x (Right (Just _)) = Left 1
  f x (Right Nothing) = Right $ Just x
  f x (Left i) = Left $ i + 1
  initial = Right Nothing

singletonValue ∷ ∀ a m n. Applicative m ⇒ Foldable n ⇒ m a → (Int → m a) → n a → m a
singletonValue noElements tooManyElements =
  either tooManyElements (maybe noElements pure) <<< singletonValue'

passover ∷ ∀ a b m. (Applicative m) ⇒ (a → m b) → a → m a
passover f x =
  f x *> pure x

replicate ∷ ∀ m. Monoid m ⇒ Int → m → m
replicate n m = go n mempty
  where
  go i acc | i <= 0 = acc
  go i acc = go (i - 1) (acc <> m)

chunksOf ∷ ∀ a. Int → Array a → Array (Array a)
chunksOf = go []
  where
  go ∷ Array (Array a) → Int → Array a → Array (Array a)
  go acc n [] = acc
  go acc n as =
    let ch = Array.take n as
    in if Array.length ch ≡ n
        then go (Array.snoc acc ch) n (Array.drop n as)
        else Array.snoc acc ch

hush ∷ ∀ a b. Either a b → Maybe b
hush = either (\_ → Nothing) (Just)

hush' ∷ ∀ a b. Except a b → Maybe b
hush' = hush ∘ runExcept

rightBool ∷ ∀ a. Either a Boolean → Boolean
rightBool = either (const false) id

parenthesize ∷ String → String
parenthesize s = "(" <> s <> ")"

removeLastCharIfPeriod ∷ String → String
removeLastCharIfPeriod s = fromMaybe s $ S.stripSuffix (S.Pattern ".") s

endSentence ∷ String → String
endSentence s = removeLastCharIfPeriod s <> "."

lowercaseFirstChar ∷ String → String
lowercaseFirstChar s = S.toLower (S.take 1 s) <> S.drop 1 s

words ∷ String → Array String
words = S.split $ S.Pattern " "

showPrettyNumber ∷ Number → String
showPrettyNumber n =
  let s = show n
  in fromMaybe s (S.stripSuffix (S.Pattern ".0") s)

showFormattedNumber ∷ Number → String
showFormattedNumber = FN.format
  { comma: true
  , before: 0
  , after: 0
  , abbreviations: false
  , sign: false
  }

showPrettyJCursor ∷ J.JCursor → String
showPrettyJCursor = go ""
  where
  go = case _, _ of
    "", J.JCursorTop  → "*"
    "", J.JField k js → go (printIdent k) js
    "", J.JIndex i js → go ("*" <> printIndex i) js
    pr, J.JCursorTop  → pr
    pr, J.JField k js → go (pr <> "." <> printIdent k) js
    pr, J.JIndex i js → go (pr <> printIndex i) js

  printIndex ∷ Int → String
  printIndex i =
    "[" <> show i <> "]"

foreign import prettyJson ∷ J.Json → String

foreign import isFirefox ∷ Boolean
