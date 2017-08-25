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

module SlamData.Workspace.Card.Setups.DimensionMap.DSL where

import SlamData.Prelude

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, modify, execState, gets)
import Data.Lens (view, Lens', lens, (%~), (^.))
import Data.List as L
import Data.Lens.Iso (Iso', iso, withIso)
import Data.Lens.Record (prop)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import Unsafe.Coerce (unsafeCoerce)

type AxesLens' s r = Lens' (Ax.AxisTypeAnnotated s r) s
data AxesLens

packAxesLens ∷ ∀ s r. AxesLens' s r → AxesLens
packAxesLens = unsafeCoerce

unpackAxesLens ∷ ∀ s r. AxesLens → AxesLens' s r
unpackAxesLens = unsafeCoerce

data DimensionFF f next
  = MandatoryField Pr.Projection (f → next)
  | OptionalField Pr.Projection (f → next)
  | Source f AxesLens (f → next)
  | Depends { filter ∷ f, source ∷ f } (f → next)
  | ActiveGuard { guard ∷ f, source ∷ f } (f → next)

type Field =
  { projection ∷ Pr.Projection
  , axesLenses ∷ L.List AxesLens
  , guards ∷ L.List Pr.Projection
  , filters ∷ L.List Pr.Projection
  , optional ∷ Boolean
  }
type DimensionF = DimensionFF Field
type DimensionMF f = Free (DimensionFF f)
type DimensionM = DimensionMF Field

_withZero ∷ ∀ a z. Semiring z ⇒ Iso' a (z × a)
_withZero = iso (zero × _)  snd

_projection ∷ ∀ r a. Lens' { projection ∷ a| r} a
_projection = lens _.projection _{ projection = _ }

_axesLenses ∷ ∀ r a. Lens' { axesLenses ∷ a|r } a
_axesLenses = lens _.axesLenses _{ axesLenses = _ }

_guards ∷ ∀ r a. Lens' { guards ∷ a|r } a
_guards = lens _.guards _{ guards = _ }

_filters ∷ ∀ r a. Lens' { filters ∷ a | r } a
_filters = lens _.filters _{ filters = _ }

-- `Iso'` is used because it's more powerful constraint than
-- simple function pair
imapField
  ∷ ∀ f ff
  . Iso' f ff
  → DimensionFF f
  ~> DimensionFF ff
imapField iso d = withIso iso \fn bn → case d of
  MandatoryField prj cb →
    MandatoryField prj $ cb ∘ bn
  OptionalField prj cb →
    OptionalField prj $ cb ∘ bn
  Source f a cb →
    Source (fn f) a $ cb ∘ bn
  Depends { filter, source } cb →
    Depends {filter: fn filter, source: fn source } $ cb ∘ bn
  ActiveGuard { guard, source } cb →
    ActiveGuard { guard: fn guard, source: fn source } $ cb ∘ bn

indexify ∷ ∀ f z. Semiring z ⇒ DimensionFF f ~> DimensionFF (z × f)
indexify = imapField _withZero

type AxesRequirements =
  Ax.AxisTypeAnnotated Int ( total ∷ Int, nonMeasure ∷ Int )

mapRequirements
  ∷ ∀ a
  . (Int → a)
  → AxesRequirements
  → Ax.AxisTypeAnnotated a ( total ∷ a, nonMeasure ∷ a )
mapRequirements f { value, category, date, time, datetime, total, nonMeasure } =
  { value: f value
  , category: f category
  , date: f date
  , time: f time
  , datetime: f datetime
  , total: f total
  , nonMeasure: f nonMeasure
  }

applyRequirements
  ∷ ∀ a
  . (Int → Int → a)
  → AxesRequirements
  → AxesRequirements
  → Ax.AxisTypeAnnotated a ( total ∷ a, nonMeasure ∷ a )
applyRequirements f r1 r2 =
  { value: f r1.value r2.value
  , category: f r1.category r2.category
  , date: f r1.date r2.date
  , time: f r1.time r2.time
  , datetime: f r1.datetime r2.datetime
  , total: f r1.total r2.total
  , nonMeasure: f r1.nonMeasure r2.nonMeasure
  }

eqRequirements ∷ AxesRequirements → AxesRequirements → Boolean
eqRequirements r1 r2 =
  r1.value ≡ r2.value
    ∧ r1.category ≡ r2.category
    ∧ r1.date ≡ r2.date
    ∧ r1.time ≡ r2.time
    ∧ r1.datetime ≡ r2.datetime
    ∧ r1.total ≡ r2.total
    ∧ r1.nonMeasure ≡ r2.nonMeasure

noneRequirements ∷ AxesRequirements
noneRequirements =
  { value: -1
  , category: -1
  , date: -1
  , time: -1
  , datetime: -1
  , total: -1
  , nonMeasure: -1
  }

zeroRequirements ∷ AxesRequirements
zeroRequirements = mapRequirements (const 0) noneRequirements

requirement ∷ L.List AxesLens → AxesRequirements
requirement lenses =
  { value: oneIf valueReq
  , category: oneIf categoryReq
  , date: oneIf dateReq
  , time: oneIf timeReq
  , datetime: oneIf datetimeReq
  , total: oneIf totalReq
  , nonMeasure: oneIf nonMeasureReq
  }
  where
  foldFn ∷ AxesRequirements → AxesLens → AxesRequirements
  foldFn acc lens =
    acc # unpackAxesLens lens %~ add one

  appliedReqs ∷ AxesRequirements
  appliedReqs =
    foldl foldFn zeroRequirements lenses

  totalReq ∷ AxesRequirements
  totalReq = { value: 1, category: 1, date: 1, datetime: 1, time: 1, total: 0, nonMeasure: 0 }

  nonMeasureReq ∷ AxesRequirements
  nonMeasureReq = totalReq { value = 0, total = 0, nonMeasure = 0 }

  valueReq ∷ AxesRequirements
  valueReq = zeroRequirements{ value = 1 }

  categoryReq ∷ AxesRequirements
  categoryReq = zeroRequirements { category = 1 }

  dateReq ∷ AxesRequirements
  dateReq = zeroRequirements { date = 1 }

  timeReq ∷ AxesRequirements
  timeReq = zeroRequirements { time = 1 }

  datetimeReq ∷ AxesRequirements
  datetimeReq = zeroRequirements { datetime = 1 }

  oneIf ∷ AxesRequirements → Int
  oneIf x
    | eqRequirements appliedReqs x = 1
    | otherwise = 0


type Package s r =
  { allFields ∷ Pr.DimMap → Ax.AxisTypeAnnotated s r → L.List Pr.Projection
  , cursorMap ∷ Pr.DimMap → Ax.AxisTypeAnnotated s r → Pr.ProjectionMap s
  , requiredFields ∷ L.List Pr.Projection
  , axesRequirements ∷ AxesRequirements
  }

mkField ∷ Pr.Projection → Field
mkField projection =
  { projection
  , axesLenses: L.Nil
  , guards: L.Nil
  , filters: L.Nil
  , optional: false
  }

optionalField ∷ Pr.Projection → Field
optionalField = _{ optional = true } ∘ mkField

fieldF ∷ DimensionFF (Int × Field) ~> State (L.List Field)
fieldF = case _ of
  MandatoryField projection c → do
    len ← gets L.length
    let fld = mkField projection
    modify $ L.Cons fld
    pure $ c $ len × fld
  OptionalField projection c → do
    len ← gets L.length
    let fld = optionalField projection
    modify $ L.Cons fld
    pure $ c $ len × fld
  Source fld a c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst fld)
            (_axesLenses %~ L.Cons a)
            st
    pure $ c fld
  Depends { source, filter } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (_filters %~ L.Cons (snd filter ^. _projection))
            st
    pure $ c source
  ActiveGuard { source, guard } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (_guards %~ L.Cons (snd guard ^. _projection))
            st
    pure $ c source

field ∷ Pr.Projection → DimensionM Field
field p = liftF $ MandatoryField p id

optional ∷ Pr.Projection → DimensionM Field
optional p = liftF $ OptionalField p id

addSource ∷ ∀ r s. AxesLens' s r → Field → DimensionM Field
addSource prj fld = liftF $ Source fld (packAxesLens prj) id

addAll ∷ Field → DimensionM Field
addAll fld = do
  f1 ← addSource (prop (SProxy ∷ SProxy "time")) fld
  f2 ← addSource (prop (SProxy ∷ SProxy "date")) f1
  f3 ← addSource (prop (SProxy ∷ SProxy "datetime")) f2
  f4 ← addSource (prop (SProxy ∷ SProxy "value")) f3
  addSource (prop (SProxy ∷ SProxy "category")) f4


isFilteredBy ∷ Field → Field → DimensionM Field
isFilteredBy filter source = liftF $ Depends { filter, source } id

isActiveWhen ∷ Field → Field → DimensionM Field
isActiveWhen guard source = liftF $ ActiveGuard { source, guard } id

interpret ∷ ∀ a s r. Monoid s ⇒ Pr.AxesComposer s → DimensionM a → Package s r
interpret axc cmds =
  { allFields
  , cursorMap
  , requiredFields: map _.projection requiredFields
  , axesRequirements
  }
  where
  fields ∷ L.List Field
  fields =
    flip execState L.Nil
    $ foldFree fieldF
    $ hoistFree indexify cmds

  requiredFields ∷ L.List Field
  requiredFields = L.filter (not ∘ _.optional) fields

  allFields ∷ Pr.DimMap → Ax.AxisTypeAnnotated s r → L.List Pr.Projection
  allFields =
    const $ const $ L.reverse $ map (view _projection) fields

  rawRequirements ∷ AxesRequirements
  rawRequirements =
    foldl axesReqFoldFn zeroRequirements $ requiredFields

  axesRequirements ∷ AxesRequirements
  axesRequirements =
    rawRequirements
    { total =
        rawRequirements.category
        + rawRequirements.value
        + rawRequirements.date
        + rawRequirements.datetime
        + rawRequirements.time
        + rawRequirements.total
    , nonMeasure =
        rawRequirements.category
        + rawRequirements.date
        + rawRequirements.datetime
        + rawRequirements.time
        + rawRequirements.nonMeasure
    }

  axesReqFoldFn ∷ AxesRequirements → Field → AxesRequirements
  axesReqFoldFn req f =
    applyRequirements add req $ requirement f.axesLenses

  cursorMap
    ∷ Pr.DimMap
    → Ax.AxisTypeAnnotated s r
    → Pr.ProjectionMap s
  cursorMap dimMap axes =
    let
      foldfn acc f =
        let
          ss = foldMap (\p → axes ^. unpackAxesLens p) $ f ^. _axesLenses
          filtered = foldl (\sm prj → axc.filter prj dimMap sm) ss $ L.reverse $ f ^. _filters
          guarded = foldl (\sm prj → axc.guard prj dimMap sm) filtered $ L.reverse $ f ^. _guards
        in Pr.insert (f ^. _projection) guarded acc
    in
      foldl foldfn Pr.empty fields
