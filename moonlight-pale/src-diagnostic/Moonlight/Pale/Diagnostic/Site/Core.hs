-- | Diagnostic severities and the accumulating 'Diagnosed' writer.
module Moonlight.Pale.Diagnostic.Site.Core
  ( DiagnosticSeverity (..),
    filterBySeverity,
    exactSeverity,
    partitionBySeverity,
    Diagnosed (..),
    diagnosed,
    pureDiagnosed,
    emitDiagnostic,
    emitDiagnostics,
    mapDiagnostics,
    filterDiagnostics,
    diagnosedValue,
    diagnosedDiagnostics,
    runDiagnosed,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prelude
  ( Applicative (pure, (<*>)),
    Bool,
    Bounded,
    Enum,
    Eq ((==)),
    Functor (fmap),
    Monad ((>>=)),
    Monoid (mempty),
    Ord ((>=)),
    Read,
    Semigroup ((<>)),
    Show,
    filter,
    reverse,
    (.),
  )

type DiagnosticSeverity :: Type
data DiagnosticSeverity
  = DiagInfo
  | DiagWarning
  | DiagError
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)

filterBySeverity :: (d -> DiagnosticSeverity) -> DiagnosticSeverity -> [d] -> [d]
filterBySeverity extract threshold =
  filter (\d -> extract d >= threshold)

exactSeverity :: (d -> DiagnosticSeverity) -> DiagnosticSeverity -> [d] -> [d]
exactSeverity extract target =
  filter (\d -> extract d == target)

partitionBySeverity :: (d -> DiagnosticSeverity) -> [d] -> Map DiagnosticSeverity [d]
partitionBySeverity extract =
  fmap reverse . Map.fromListWith (<>) . fmap (\d -> (extract d, [d]))

type Diagnosed :: Type -> Type -> Type
newtype Diagnosed d a = Diagnosed {unDiagnosed :: ([d], a)}
  deriving stock (Eq, Show)

instance Functor (Diagnosed d) where
  fmap f (Diagnosed (ds, a)) = Diagnosed (ds, f a)

instance Applicative (Diagnosed d) where
  pure a = Diagnosed ([], a)
  Diagnosed (ds1, f) <*> Diagnosed (ds2, a) = Diagnosed (ds1 <> ds2, f a)

instance Monad (Diagnosed d) where
  Diagnosed (ds1, a) >>= f =
    let Diagnosed (ds2, b) = f a
     in Diagnosed (ds1 <> ds2, b)

diagnosed :: a -> [d] -> Diagnosed d a
diagnosed a ds = Diagnosed (ds, a)

pureDiagnosed :: a -> Diagnosed d a
pureDiagnosed = pure

emitDiagnostic :: d -> Diagnosed d ()
emitDiagnostic d = Diagnosed ([d], ())

emitDiagnostics :: [d] -> Diagnosed d ()
emitDiagnostics ds = Diagnosed (ds, ())

mapDiagnostics :: (d -> e) -> Diagnosed d a -> Diagnosed e a
mapDiagnostics f (Diagnosed (ds, a)) = Diagnosed (fmap f ds, a)

filterDiagnostics :: (d -> Bool) -> Diagnosed d a -> Diagnosed d a
filterDiagnostics p (Diagnosed (ds, a)) = Diagnosed (filter p ds, a)

diagnosedValue :: Diagnosed d a -> a
diagnosedValue (Diagnosed (_, a)) = a

diagnosedDiagnostics :: Diagnosed d a -> [d]
diagnosedDiagnostics (Diagnosed (ds, _)) = ds

runDiagnosed :: Diagnosed d a -> (a, [d])
runDiagnosed (Diagnosed (ds, a)) = (a, ds)

instance Semigroup a => Semigroup (Diagnosed d a) where
  Diagnosed (ds1, a1) <> Diagnosed (ds2, a2) = Diagnosed (ds1 <> ds2, a1 <> a2)

instance Monoid a => Monoid (Diagnosed d a) where
  mempty = Diagnosed ([], mempty)
