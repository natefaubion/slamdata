module SlamData.Workspace.Card.Eval.Monad where

import SlamData.Prelude hiding (throwError)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, resume)
import Control.Parallel.Class (runParallel, parallel)

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.Set (Set)
import Data.Set as Set

import Quasar.Advanced.QuasarAF as QA
import Quasar.Error (QError)

import SlamData.Effects (SlamDataEffects)
import SlamData.Monad.Par (ParF(..), Par, mkPar, unPar)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Quasar.Error (msgToQError)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Eval.Machine (Machine, Step)
import SlamData.Workspace.Card.Eval.Transition (CardEvalInput, Eval, tagEval)
import SlamData.Workspace.Card.Port (Port, tagPort)
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))
import SlamData.Workspace.Deck.DeckId as DID
import Utils.Path (DirPath, FilePath)

type CardEval = CardEvalM SlamDataEffects

type EvalMachineInput =
  CardEvalInput × Eval

type EvalMachine =
  Machine CardEval EvalMachineInput Port

type EvalMachineStep =
  Step CardEval EvalMachineInput Port

data CardEvalF eff a
  = Aff (Aff eff a)
  | Par (Par (CardEvalM eff) a)
  | Quasar (QA.QuasarAFC a)
  | Tell (Set AdditionalSource) a
  | Throw QError

instance functorCardEvalF ∷ Functor (CardEvalF eff) where
  map f (Aff aff)   = Aff (map f aff)
  map f (Par par)   = Par (map f par)
  map f (Quasar q)  = Quasar (map f q)
  map f (Tell as a) = Tell as (f a)
  map f (Throw err) = Throw err

newtype CardEvalM eff a = CardEvalM (Free (CardEvalF eff) a)

unCardEvalM ∷ ∀ eff. CardEvalM eff ~> Free (CardEvalF eff)
unCardEvalM (CardEvalM a) = a

instance functorCardEvalM ∷ Functor (CardEvalM eff) where
  map f (CardEvalM a) = CardEvalM (map f a)

instance applyCardEvalM ∷ Apply (CardEvalM eff) where
  apply (CardEvalM a) (CardEvalM b) = CardEvalM (a <*> b)

instance applicativeCardEvalM ∷ Applicative (CardEvalM eff) where
  pure = CardEvalM ∘ pure

instance bindCardEvalM ∷ Bind (CardEvalM eff) where
  bind (CardEvalM a) f = CardEvalM (a >>= unCardEvalM ∘ f)

instance monadCardEvalM ∷ Monad (CardEvalM eff)

instance monadEffCardEvalM ∷ MonadEff eff (CardEvalM eff) where
  liftEff = CardEvalM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffCardEvalM ∷ MonadAff eff (CardEvalM eff) where
  liftAff = CardEvalM ∘ liftF ∘ Aff

instance affableCardEvalM ∷ Affable eff (CardEvalM eff) where
  fromAff = CardEvalM ∘ liftF ∘ Aff

instance monadParCardEvalM ∷ MonadPar (CardEvalM eff) where
  par f a b = CardEvalM $ liftF $ Par $ mkPar $ ParF f a b

instance quasarDSLCardEvalM ∷ QuasarDSL (CardEvalM eff) where
  liftQuasar = CardEvalM ∘ liftF ∘ Quasar

throwError ∷ ∀ eff a. QError → CardEvalM eff a
throwError = CardEvalM ∘ liftF ∘ Throw

throw ∷ ∀ eff a. String → CardEvalM eff a
throw = CardEvalM ∘ liftF ∘ Throw ∘ msgToQError

tell ∷ ∀ eff. Set AdditionalSource → CardEvalM eff Unit
tell as = CardEvalM (liftF (Tell as unit))

addSource ∷ ∀ eff. FilePath → CardEvalM eff Unit
addSource fp = tell (Set.singleton (Source fp))

addCache ∷ ∀ eff. FilePath → CardEvalM eff Unit
addCache fp = tell (Set.singleton (Cache fp))

addSources ∷ ∀ eff f. Foldable f ⇒ f FilePath → CardEvalM eff Unit
addSources fps = tell (foldMap (Set.singleton ∘ Source) fps)

addCaches ∷ ∀ eff f. Foldable f ⇒ f FilePath → CardEvalM eff Unit
addCaches fps = tell (foldMap (Set.singleton ∘ Cache) fps)

additionalSources ∷ ∀ eff f. Foldable f ⇒ f AdditionalSource → CardEvalM eff Unit
additionalSources = tell ∘ foldMap Set.singleton

temporaryOutputResource
  ∷ ∀ r
  . { path ∷ DirPath, cardCoord ∷ DID.DeckId × CID.CardId | r }
  → FilePath
temporaryOutputResource { path, cardCoord: deckId × cardId } =
  path
    </> Path.dir ".tmp"
    </> Path.dir (DID.deckIdToString deckId)
    </> Path.file ("out" ⊕ CID.cardIdToString cardId)

liftQ ∷ ∀ eff a. CardEvalM eff (Either QError a) → CardEvalM eff a
liftQ = flip bind (either throwError pure)

unexpectedInput ∷ ∀ eff a. CardEvalInput × Eval → CardEvalM eff a
unexpectedInput ({ input } × eval) =
  throw $ "Card received unexpected input type; " <> tagEval eval <> " | " <> tagPort input

runCardEvalM
  ∷ ∀ eff m a
  . (Functor m, Monad m, MonadPar m, QuasarDSL m, Affable eff m)
  ⇒ CardEvalM eff a
  → m (Either QError a × Set AdditionalSource)
runCardEvalM (CardEvalM ce) = go Set.empty ce
  where
    go as ce = case resume ce of
      Left ctr →
        case ctr of
          Aff aff →
            fromAff aff >>= go as
          Par par →
            flip unPar par \(ParF f fx fy) → do
              { x: ex × as1, y: ey × as2 } ←
                runParallel $
                  { x: _, y: _ }
                    <$> parallel (runCardEvalM fx)
                    <*> parallel (runCardEvalM fy)
              case ex, ey of
                Left err, _ →
                  pure (Left err × as)
                _, Left err →
                  pure (Left err × as)
                Right x, Right y →
                  go (as <> as1 <> as2) (f x y)
          Quasar q →
            liftQuasar q >>= go as
          Tell as' n →
            go (as <> as') n
          Throw err →
            pure (Left err × as)
      Right a →
        pure (Right a × as)
