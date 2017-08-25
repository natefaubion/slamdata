module Test.Utils where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)

import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe(..), maybe)

import Node.Path (normalize)
import Node.Process (PROCESS, cwd)

ifFalse ∷ ∀ m. Applicative m ⇒ m Unit → Boolean → m Unit
ifFalse f boolean =
  if boolean then pure unit else f

ifTrue ∷ ∀ m. Applicative m ⇒ m Unit → Boolean → m Unit
ifTrue f boolean =
  if boolean then f else pure unit

passover ∷ ∀ a b m. Applicative m ⇒ (a → m b) → a → m a
passover f x =
  f x *> pure x

orIfItFails ∷ ∀ a b m. Alt m ⇒ (a → m b) → (a → m b) → a → m b
orIfItFails f g x =
  f x <|> g x

isEmpty ∷ ∀ a m. Foldable m ⇒ m a → Boolean
isEmpty =
  foldr (\_ _ → false) true

singletonValue' ∷ ∀ a m. Foldable m ⇒ m a → Either Int (Maybe a)
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

throwIfEmpty
  ∷ ∀ a f m eff
  . Foldable f
  ⇒ MonadEff (exception ∷ EXCEPTION | eff) m
  ⇒ String
  → f a
  → m Unit
throwIfEmpty message xs
  | isEmpty xs = liftEff $ throw message
  | otherwise = pure unit

throwIfNotEmpty
  ∷ ∀ a f m eff
  . Foldable f
  ⇒ MonadEff (exception ∷ EXCEPTION | eff) m
  ⇒ String
  → f a
  → m Unit
throwIfNotEmpty message xs
  | isEmpty xs = pure unit
  | otherwise = liftEff $ throw message

appendToCwd
  ∷ ∀ eff m
  . MonadEff (process ∷ PROCESS | eff) m
  ⇒ String
  → m String
appendToCwd s = liftEff $ normalize <<< (\i -> i <> "/" <> s) <$> cwd
