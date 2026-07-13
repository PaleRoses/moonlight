{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeFamilies #-}

module Moonlight.Differential.Fact.Local
  ( Carrier,
    carrierContexts,
    carrierEmpty,
    carrierIntersection,
    carrierIntersects,
    carrierSubsetOf,
    LocalityShape (..),
    LocalAddress,
    laProp,
    laSupport,
    laCarrier,
    mkLocalAddress,
    LocalFact,
    lfAddress,
    lfBoundary,
    lfEvidence,
    mkLocalFact,
    BoundarySummary,
    BoundaryView,
    Overlap,
    Obstruction,
    Compatibility,
    LocalFactOverlap (..),
    LocalFactObstruction (..),
    LocalFactCompatibility (..),
    FactAntichain,
    emptyFactAntichain,
    insertAntichain,
    antichainFromFacts,
    mergeAntichains,
    membersAntichain,
    lookupByKey,
    exportBoundary,
    overlapBetween,
    restrictBoundary,
    compatibleOnOverlap,
    overlapFacts,
    compatibleFacts,
    closure,
    minimizeSupport,
    subsumes,
    dominates,
  )
where

import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeLookupError
  )
import Moonlight.Core
  ( BoundaryOps (..),
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    normalizeSupport,
    supportReachableLatticeContexts
  )

type Carrier :: Type -> Type
newtype Carrier context = Carrier
  { carrierContexts :: Set context
  }
  deriving stock (Eq, Ord, Show)

type LocalityShape :: Type
data LocalityShape
  = MinimalClosed

type LocalAddress :: Type -> Type -> LocalityShape -> Type
data LocalAddress context proposition (shape :: LocalityShape) = LocalAddress
  { laProp :: PropositionKey proposition,
    laSupport :: SupportBasis context,
    laCarrier :: Carrier context
  }
  deriving stock (Eq, Show)

type LocalFact :: Type -> Type -> Type -> Type -> Type
data LocalFact context proposition evidence boundary = LocalFact
  { lfAddress :: LocalAddress context proposition 'MinimalClosed,
    lfBoundary :: boundary,
    lfEvidence :: evidence
  }
  deriving stock (Eq, Show)

type BoundarySummary :: Type -> Type
newtype BoundarySummary boundary = BoundarySummary
  { unBoundarySummary :: boundary
  }
  deriving stock (Eq, Show)

type BoundaryView :: Type -> Type
newtype BoundaryView boundary = BoundaryView
  { unBoundaryView :: boundary
  }
  deriving stock (Eq, Show)

type Overlap :: Type -> Type
newtype Overlap overlap = Overlap
  { unOverlap :: overlap
  }
  deriving stock (Eq, Show)

type Obstruction :: Type -> Type
newtype Obstruction boundary = Obstruction
  { unObstruction :: boundary
  }
  deriving stock (Eq, Show)

type Compatibility :: Type -> Type
newtype Compatibility boundary = Compatibility
  { unCompatibility :: boundary
  }
  deriving stock (Eq, Show)

type LocalFactOverlap :: Type -> Type -> Type
data LocalFactOverlap context overlap = LocalFactOverlap
  { lfoCarrier :: !(Carrier context),
    lfoBoundaryOverlap :: !overlap
  }
  deriving stock (Eq, Show)

type LocalFactObstruction :: Type -> Type -> Type
data LocalFactObstruction context boundary = LocalFactObstruction
  { lfoObstructedCarrier :: !(Carrier context),
    lfoBoundaryObstruction :: !(Obstruction boundary)
  }
  deriving stock (Eq, Show)

type LocalFactCompatibility :: Type -> Type -> Type
data LocalFactCompatibility context boundary
  = LocalFactNoCarrierOverlap !(Carrier context)
  | LocalFactCompatibleOnCarrier !(Carrier context) !(Compatibility boundary)
  deriving stock (Eq, Show)

type FactAntichain :: Type -> Type -> Type -> Type -> Type
newtype FactAntichain context proposition evidence boundary = FactAntichain
  (Map (PropositionKey proposition) [LocalFact context proposition evidence boundary])
  deriving stock (Eq, Show)

carrierEmpty :: Carrier context -> Bool
carrierEmpty =
  Set.null . carrierContexts

carrierIntersection ::
  Ord context =>
  Carrier context ->
  Carrier context ->
  Carrier context
carrierIntersection leftCarrier rightCarrier =
  Carrier
    ( Set.intersection
        (carrierContexts leftCarrier)
        (carrierContexts rightCarrier)
    )

carrierIntersects ::
  Ord context =>
  Carrier context ->
  Carrier context ->
  Bool
carrierIntersects leftCarrier rightCarrier =
  not (carrierEmpty (carrierIntersection leftCarrier rightCarrier))

carrierSubsetOf ::
  Ord context =>
  Carrier context ->
  Carrier context ->
  Bool
carrierSubsetOf leftCarrier rightCarrier =
  Set.isSubsetOf
    (carrierContexts leftCarrier)
    (carrierContexts rightCarrier)

mkLocalAddress ::
  Ord context =>
  ContextLattice context ->
  PropositionKey proposition ->
  SupportBasis context ->
  Either (ContextLatticeLookupError context) (LocalAddress context proposition 'MinimalClosed)
mkLocalAddress latticeValue propositionKey supportValue = do
  minimalSupport <- minimizeSupport latticeValue supportValue
  supportCarrier <- closure latticeValue minimalSupport
  pure
    LocalAddress
      { laProp = propositionKey,
        laSupport = minimalSupport,
        laCarrier = supportCarrier
      }

mkLocalFact ::
  LocalAddress context proposition 'MinimalClosed ->
  boundary ->
  evidence ->
  LocalFact context proposition evidence boundary
mkLocalFact address boundary evidence =
  LocalFact
    { lfAddress = address,
      lfBoundary = boundary,
      lfEvidence = evidence
    }

minimizeSupport :: Ord context => ContextLattice context -> SupportBasis context -> Either (ContextLatticeLookupError context) (SupportBasis context)
minimizeSupport =
  normalizeSupport

closure :: Ord context => ContextLattice context -> SupportBasis context -> Either (ContextLatticeLookupError context) (Carrier context)
closure latticeValue =
  fmap (Carrier . Set.fromList)
    . supportReachableLatticeContexts latticeValue

exportBoundary :: LocalFact context proposition evidence boundary -> BoundarySummary boundary
exportBoundary =
  BoundarySummary . lfBoundary

overlapBetween ::
  BoundaryOps boundary =>
  BoundarySummary boundary ->
  BoundarySummary boundary ->
  Overlap (BoundaryOverlap boundary)
overlapBetween (BoundarySummary leftBoundary) (BoundarySummary rightBoundary) =
  Overlap (overlapBetweenBoundary leftBoundary rightBoundary)

restrictBoundary ::
  BoundaryOps boundary =>
  Overlap (BoundaryOverlap boundary) ->
  BoundarySummary boundary ->
  BoundaryView boundary
restrictBoundary (Overlap overlapValue) (BoundarySummary boundaryValue) =
  BoundaryView (restrictBoundaryRaw overlapValue boundaryValue)

compatibleOnOverlap ::
  BoundaryOps boundary =>
  BoundaryView boundary ->
  BoundaryView boundary ->
  Either (Obstruction boundary) (Compatibility boundary)
compatibleOnOverlap (BoundaryView leftBoundary) (BoundaryView rightBoundary) =
  either
    (Left . Obstruction)
    (Right . Compatibility)
    (compatibleBoundaryRaw leftBoundary rightBoundary)

overlapFacts ::
  (Ord context, BoundaryOps boundary) =>
  LocalFact context proposition evidence boundary ->
  LocalFact context proposition evidence boundary ->
  LocalFactOverlap context (BoundaryOverlap boundary)
overlapFacts leftFact rightFact =
  let leftSummary = exportBoundary leftFact
      rightSummary = exportBoundary rightFact
      Overlap boundaryOverlap =
        overlapBetween leftSummary rightSummary
   in LocalFactOverlap
        { lfoCarrier =
            carrierIntersection
              (laCarrier (lfAddress leftFact))
              (laCarrier (lfAddress rightFact)),
          lfoBoundaryOverlap = boundaryOverlap
        }

compatibleFacts ::
  (Ord context, BoundaryOps boundary) =>
  LocalFact context proposition evidence boundary ->
  LocalFact context proposition evidence boundary ->
  Either
    (LocalFactObstruction context boundary)
    (LocalFactCompatibility context boundary)
compatibleFacts leftFact rightFact =
  let factOverlap = overlapFacts leftFact rightFact
      carrierOverlap = lfoCarrier factOverlap
   in if carrierEmpty carrierOverlap
        then Right (LocalFactNoCarrierOverlap carrierOverlap)
        else
          let overlapValue =
                Overlap (lfoBoundaryOverlap factOverlap)
              leftView =
                restrictBoundary overlapValue (exportBoundary leftFact)
              rightView =
                restrictBoundary overlapValue (exportBoundary rightFact)
           in case compatibleOnOverlap leftView rightView of
                Left obstruction ->
                  Left
                    LocalFactObstruction
                      { lfoObstructedCarrier = carrierOverlap,
                        lfoBoundaryObstruction = obstruction
                      }
                Right compatibility ->
                  Right (LocalFactCompatibleOnCarrier carrierOverlap compatibility)

subsumes ::
  BoundaryOps boundary =>
  BoundarySummary boundary ->
  BoundarySummary boundary ->
  Bool
subsumes (BoundarySummary leftBoundary) (BoundarySummary rightBoundary) =
  subsumesBoundaryRaw leftBoundary rightBoundary

dominates ::
  (Ord context, Eq proposition, BoundaryOps boundary) =>
  LocalFact context proposition evidence boundary ->
  LocalFact context proposition evidence boundary ->
  Bool
dominates leftFact rightFact =
  laProp (lfAddress leftFact) == laProp (lfAddress rightFact)
    && carrierSubsetOf (laCarrier (lfAddress leftFact)) (laCarrier (lfAddress rightFact))
    && subsumes (exportBoundary leftFact) (exportBoundary rightFact)

emptyFactAntichain :: FactAntichain context proposition evidence boundary
emptyFactAntichain =
  FactAntichain Map.empty

insertAntichain ::
  (Ord context, Ord proposition, BoundaryOps boundary) =>
  LocalFact context proposition evidence boundary ->
  FactAntichain context proposition evidence boundary ->
  FactAntichain context proposition evidence boundary
insertAntichain localFactValue (FactAntichain buckets) =
  FactAntichain
    (Map.alter (Just . insertAtKey . fromMaybe []) (laProp (lfAddress localFactValue)) buckets)
  where
    insertAtKey existingFacts =
      if any (`dominates` localFactValue) existingFacts
        then existingFacts
        else
          localFactValue :
            filter
              (\existingFact -> not (dominates localFactValue existingFact))
              existingFacts

antichainFromFacts ::
  (Ord context, Ord proposition, BoundaryOps boundary) =>
  [LocalFact context proposition evidence boundary] ->
  FactAntichain context proposition evidence boundary
antichainFromFacts =
  List.foldl' (flip insertAntichain) emptyFactAntichain

mergeAntichains ::
  (Ord context, Ord proposition, BoundaryOps boundary) =>
  FactAntichain context proposition evidence boundary ->
  FactAntichain context proposition evidence boundary ->
  FactAntichain context proposition evidence boundary
mergeAntichains leftFacts rightFacts =
  List.foldl'
    (flip insertAntichain)
    leftFacts
    (membersAntichain rightFacts)

membersAntichain :: FactAntichain context proposition evidence boundary -> [LocalFact context proposition evidence boundary]
membersAntichain (FactAntichain buckets) =
  Map.foldr (<>) [] buckets

lookupByKey ::
  Ord proposition =>
  PropositionKey proposition ->
  FactAntichain context proposition evidence boundary ->
  [LocalFact context proposition evidence boundary]
lookupByKey propositionKey (FactAntichain buckets) =
  Map.findWithDefault [] propositionKey buckets
