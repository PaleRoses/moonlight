{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeError (..),
    FactorShapeManifest (..),
    FactorShapeNodeManifest (..),
    compileFactorShapeManifest,
    lookupFactorShapeManifestNode,
    factorShapeManifestNodes,
    factorShapeFromManifestBoundary,
    factorShapeForBoundary,
    canonicalBoundaryShape,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( note,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    BoundaryShape (..),
    boundaryShape,
    runtimeBoundarySensitiveSlots,
    runtimeBoundarySlotKeys,
  )
import Moonlight.Flow.Plan.Shape.Boundary.Canonical
  ( mkCanonicalBoundaryShape,
  )
import Moonlight.Flow.Plan.Shape.Build
  ( mkCanonBagShape,
    mkCanonSeparator,
    mkFactorShape,
    mkPlanShape,
  )
import Moonlight.Flow.Plan.Shape.Encode
  ( fragmentPayloadWords,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Shape
  ( CanonAtom (..),
    CanonAtomMultiset,
    CanonBagShape (..),
    CanonSeparator (..),
    CanonicalBoundaryShape,
    CanonicalFragment (..),
    CanonicalizationResult (..),
    insertCanonAtom,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( CanonSlot (..),
    FragmentPayload (..),
    PlanShape (..),
    PlanStage (..),
  )

data FactorShapeError
  = FactorShapeMissingBag !BagId
  | FactorShapeMissingAtom {-# UNPACK #-} !Int
  | FactorShapeMissingSlot {-# UNPACK #-} !Int
  | FactorShapeMissingBoundarySlot {-# UNPACK #-} !Int
  | FactorShapeMissingSeparator !BagId !BagId
  | FactorShapeMissingManifestNode !FactorNode
  deriving stock (Eq, Ord, Show, Read)

data FactorShapeManifest = FactorShapeManifest
  { fsmNodes :: !(Map FactorNode FactorShapeNodeManifest)
  }
  deriving stock (Eq, Show, Read)

data FactorShapeNodeManifest = FactorShapeNodeManifest
  { fsnmNode :: !FactorNode,
    fsnmOutputSchema :: ![SlotId],
    fsnmCanonSchema :: ![CanonSlot],
    fsnmFragment :: !CanonicalFragment,
    fsnmAtoms :: !CanonAtomMultiset,
    fsnmSourceSchema :: ![CanonSlot]
  }
  deriving stock (Eq, Ord, Show, Read)

compileFactorShapeManifest ::
  CanonicalizationResult ->
  DecompPlan ->
  Either FactorShapeError FactorShapeManifest
compileFactorShapeManifest planShape decomp =
  FactorShapeManifest . Map.fromList
    <$> traverse nodeManifest (factorNodesOfDecomp decomp)
  where
    nodeManifest node = do
      manifest <- compileFactorShapeNodeManifest planShape decomp node
      pure (node, manifest)
{-# INLINE compileFactorShapeManifest #-}

compileFactorShapeNodeManifest ::
  CanonicalizationResult ->
  DecompPlan ->
  FactorNode ->
  Either FactorShapeError FactorShapeNodeManifest
compileFactorShapeNodeManifest planShape decomp node = do
  outputSchema <- factorNodeOutputSchema decomp node
  canonSchema <- traverse (lookupCanonSlotId planShape) outputSchema
  fragment <- canonicalFragmentForNode planShape decomp node
  representedAtoms <- representedAtomMultiset planShape decomp node
  sourceSchema <- sourceSchemaOfAtoms representedAtoms
  pure
    FactorShapeNodeManifest
      { fsnmNode = node,
        fsnmOutputSchema = outputSchema,
        fsnmCanonSchema = canonSchema,
        fsnmFragment = fragment,
        fsnmAtoms = representedAtoms,
        fsnmSourceSchema = sourceSchema
      }
{-# INLINE compileFactorShapeNodeManifest #-}

lookupFactorShapeManifestNode ::
  FactorNode ->
  FactorShapeManifest ->
  Maybe FactorShapeNodeManifest
lookupFactorShapeManifestNode node =
  Map.lookup node . fsmNodes
{-# INLINE lookupFactorShapeManifestNode #-}

factorShapeManifestNodes ::
  FactorShapeManifest ->
  [(FactorNode, FactorShapeNodeManifest)]
factorShapeManifestNodes =
  Map.toAscList . fsmNodes
{-# INLINE factorShapeManifestNodes #-}

factorShapeFromManifestBoundary ::
  CanonicalizationResult ->
  FactorShapeNodeManifest ->
  RuntimeBoundary ->
  Either FactorShapeError (PlanShape 'FactorShape)
factorShapeFromManifestBoundary planShape manifest boundary = do
  outputSchema <-
    traverse (lookupCanonSlotId planShape) (bsSchema (boundaryShape boundary))
  canonicalShape <-
    canonicalBoundaryShape planShape boundary
  let maybeSeparator =
        case fsnmFragment manifest of
          CanonSeparatorFragment separator ->
            Just separator
          _ ->
            Nothing
  pure
    ( mkFactorShape
        (crPlan planShape)
        (fragmentShapeFromCanonical (psDigest (crPlan planShape)) (fsnmFragment manifest))
        (fsnmAtoms manifest)
        (factorShapeVisibleSourceSchema manifest outputSchema)
        outputSchema
        maybeSeparator
        canonicalShape
        (crResidual planShape)
    )
{-# INLINE factorShapeFromManifestBoundary #-}

factorShapeVisibleSourceSchema ::
  FactorShapeNodeManifest ->
  [CanonSlot] ->
  [CanonSlot]
factorShapeVisibleSourceSchema manifest outputSchema =
  Set.toAscList (Set.fromList (fsnmSourceSchema manifest <> outputSchema))
{-# INLINE factorShapeVisibleSourceSchema #-}

fragmentShapeFromCanonical ::
  StableDigest128 ->
  CanonicalFragment ->
  PlanShape 'Fragment
fragmentShapeFromCanonical rootDigest fragment =
  mkPlanShape fragmentPayloadWords $
    case fragment of
      CanonRootFragment ->
        RootFragmentPayload rootDigest
      CanonBagFragment bag ->
        BagFragmentPayload (cbgDigest bag)
      CanonSeparatorFragment separator ->
        SeparatorFragmentPayload
          (csepDigest separator)
          (cbgDigest (csepChild separator))
          (cbgDigest (csepParent separator))
{-# INLINE fragmentShapeFromCanonical #-}

factorNodesOfDecomp ::
  DecompPlan ->
  [FactorNode]
factorNodesOfDecomp decomp =
  FactorNodeRoot
    : [ FactorNodeBag (BagId bagKey)
      | bagKey <- IntMap.keys (dpBags decomp)
      ]
      <> [ FactorNodeBagBelief (BagId bagKey)
         | bagKey <- IntMap.keys (dpBags decomp)
         ]
      <> [ FactorNodeSeparator child parent
         | ((child, parent), _separator) <- Map.toAscList (dpSeparator decomp)
         ]
{-# INLINE factorNodesOfDecomp #-}

factorNodeOutputSchema ::
  DecompPlan ->
  FactorNode ->
  Either FactorShapeError [SlotId]
factorNodeOutputSchema decomp node =
  case node of
    FactorNodeRoot ->
      Right []
    FactorNodeBag bagId ->
      dbSlots <$> lookupBag bagId decomp
    FactorNodeBagBelief bagId ->
      dbSlots <$> lookupBag bagId decomp
    FactorNodeSeparator child parent ->
      case Map.lookup (child, parent) (dpSeparator decomp) of
        Nothing ->
          Left (FactorShapeMissingSeparator child parent)
        Just separator ->
          Right separator
{-# INLINE factorNodeOutputSchema #-}

factorShapeForBoundary ::
  CanonicalizationResult ->
  DecompPlan ->
  FactorNode ->
  RuntimeBoundary ->
  Either FactorShapeError (PlanShape 'FactorShape)
factorShapeForBoundary planShape decomp node boundary = do
  manifest <- compileFactorShapeManifest planShape decomp
  nodeManifest <-
    note
      (FactorShapeMissingManifestNode node)
      (lookupFactorShapeManifestNode node manifest)
  factorShapeFromManifestBoundary planShape nodeManifest boundary
{-# INLINE factorShapeForBoundary #-}

canonicalFragmentForNode ::
  CanonicalizationResult ->
  DecompPlan ->
  FactorNode ->
  Either FactorShapeError CanonicalFragment
canonicalFragmentForNode planShape decomp node =
  case node of
    FactorNodeRoot ->
      pure CanonRootFragment
    FactorNodeBag bag ->
      CanonBagFragment <$> canonicalBagShape planShape decomp bag
    FactorNodeBagBelief bag ->
      CanonBagFragment <$> canonicalBagShape planShape decomp bag
    FactorNodeSeparator child parent -> do
      childBag <- canonicalBagShape planShape decomp child
      parentBag <- canonicalBagShape planShape decomp parent
      separatorSlots <-
        traverse (lookupCanonSlotId planShape)
          =<< note
            (FactorShapeMissingSeparator child parent)
            (Map.lookup (child, parent) (dpSeparator decomp))
      pure
        ( CanonSeparatorFragment
            (mkCanonSeparator childBag parentBag separatorSlots)
        )
{-# INLINE canonicalFragmentForNode #-}

canonicalBagShape ::
  CanonicalizationResult ->
  DecompPlan ->
  BagId ->
  Either FactorShapeError CanonBagShape
canonicalBagShape planShape decomp bagId = do
  bag <- lookupBag bagId decomp
  slots <- traverse (lookupCanonSlotId planShape) (dbSlots bag)
  atoms <- atomMultisetForRawKeys planShape (dbAtoms bag)
  pure (mkCanonBagShape slots atoms)
{-# INLINE canonicalBagShape #-}

representedAtomMultiset ::
  CanonicalizationResult ->
  DecompPlan ->
  FactorNode ->
  Either FactorShapeError CanonAtomMultiset
representedAtomMultiset planShape decomp node =
  case node of
    FactorNodeRoot ->
      atomMultisetForRawKeys
        planShape
        (allDecompAtomKeys decomp)
    FactorNodeBag bagId -> do
      bag <- lookupBag bagId decomp
      atomMultisetForRawKeys planShape (dbAtoms bag)
    FactorNodeBagBelief bagId -> do
      bag <- lookupBag bagId decomp
      atomMultisetForRawKeys planShape (dbAtoms bag)
    FactorNodeSeparator child _parent -> do
      rawAtomKeys <- subtreeAtomKeys decomp child
      atomMultisetForRawKeys planShape rawAtomKeys
{-# INLINE representedAtomMultiset #-}

lookupBag ::
  BagId ->
  DecompPlan ->
  Either FactorShapeError DecompBag
lookupBag bagId@(BagId bagKey) decomp =
  note (FactorShapeMissingBag bagId) $
    IntMap.lookup bagKey (dpBags decomp)
{-# INLINE lookupBag #-}

subtreeAtomKeys ::
  DecompPlan ->
  BagId ->
  Either FactorShapeError IntSet
subtreeAtomKeys decomp =
  go IntSet.empty
  where
    go seen bagId@(BagId bagKey)
      | IntSet.member bagKey seen =
          Right IntSet.empty
      | otherwise = do
          bag <- lookupBag bagId decomp
          let seen1 =
                IntSet.insert bagKey seen
              children =
                IntMap.findWithDefault [] bagKey (dpChildren decomp)
          childAtoms <-
            IntSet.unions <$> traverse (go seen1) children
          pure (IntSet.union (dbAtoms bag) childAtoms)
{-# INLINE subtreeAtomKeys #-}

allDecompAtomKeys :: DecompPlan -> IntSet
allDecompAtomKeys =
  foldMap dbAtoms . dpBags
{-# INLINE allDecompAtomKeys #-}

atomMultisetForRawKeys ::
  CanonicalizationResult ->
  IntSet ->
  Either FactorShapeError CanonAtomMultiset
atomMultisetForRawKeys planShape =
  foldM insertRawAtom Map.empty . IntSet.toAscList
  where
    insertRawAtom atoms rawAtomKey = do
      atomValue <- lookupCanonAtom planShape rawAtomKey
      pure (insertCanonAtom atomValue atoms)
{-# INLINE atomMultisetForRawKeys #-}

lookupCanonAtom ::
  CanonicalizationResult ->
  Int ->
  Either FactorShapeError CanonAtom
lookupCanonAtom planShape rawAtomKey =
  note (FactorShapeMissingAtom rawAtomKey) $
    IntMap.lookup rawAtomKey (crAtomShapes planShape)
{-# INLINE lookupCanonAtom #-}

lookupCanonSlotId ::
  CanonicalizationResult ->
  SlotId ->
  Either FactorShapeError CanonSlot
lookupCanonSlotId planShape slot =
  lookupCanonSlotKey planShape (slotIdKey slot)
{-# INLINE lookupCanonSlotId #-}

lookupCanonSlotKey ::
  CanonicalizationResult ->
  Int ->
  Either FactorShapeError CanonSlot
lookupCanonSlotKey planShape rawSlotKey =
  note (FactorShapeMissingSlot rawSlotKey) $
    IntMap.lookup rawSlotKey (crSlotMap planShape)
{-# INLINE lookupCanonSlotKey #-}

sourceSchemaOfAtoms ::
  CanonAtomMultiset ->
  Either FactorShapeError [CanonSlot]
sourceSchemaOfAtoms atoms =
  pure
    . Set.toAscList
    . Set.fromList
    . foldMap caColumnsPositive
    $ Map.toAscList atoms
  where
    caColumnsPositive :: (CanonAtom, Int) -> [CanonSlot]
    caColumnsPositive (atomValue, multiplicity)
      | multiplicity <= 0 =
          []
      | otherwise =
          Moonlight.Flow.Plan.Shape.caColumns atomValue
{-# INLINE sourceSchemaOfAtoms #-}

canonicalBoundaryShape ::
  CanonicalizationResult ->
  RuntimeBoundary ->
  Either FactorShapeError CanonicalBoundaryShape
canonicalBoundaryShape planShape boundary = do
  schema <-
    traverse (lookupCanonSlotId planShape) (bsSchema (boundaryShape boundary))
  sensitiveSlots <-
    remapSlotKeySet planShape (runtimeBoundarySensitiveSlots boundary)
  slotKeys <-
    remapSlotKeyMap planShape (runtimeBoundarySlotKeys boundary)
  pure (mkCanonicalBoundaryShape schema sensitiveSlots slotKeys)
{-# INLINE canonicalBoundaryShape #-}

remapSlotKeySet ::
  CanonicalizationResult ->
  IntSet ->
  Either FactorShapeError (Set CanonSlot)
remapSlotKeySet planShape =
  fmap Set.fromList
    . traverse (lookupCanonSlotKey planShape)
    . IntSet.toAscList
{-# INLINE remapSlotKeySet #-}

remapSlotKeyMap ::
  CanonicalizationResult ->
  IntMap IntSet ->
  Either FactorShapeError (Map CanonSlot (Set Int))
remapSlotKeyMap planShape =
  fmap Map.fromList . traverse remapSlotKeys . IntMap.toAscList
  where
    remapSlotKeys (rawSlotKey, representativeKeys) = do
      canonSlot <- lookupCanonSlotKey planShape rawSlotKey
      pure
        ( canonSlot,
          Set.fromAscList (IntSet.toAscList representativeKeys)
        )
{-# INLINE remapSlotKeyMap #-}
