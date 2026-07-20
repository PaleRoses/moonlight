{-# LANGUAGE PatternSynonyms #-}

module Moonlight.EGraph.Saturation.Cohomological.Backend.Modality.Internal.Proof
  ( ProofReachability,
    proofTupleSupportedWithReachability,
    supportsProofConstraints,
    rootClassFromTuple,
    classIdFromLabel,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Moonlight.Rewrite.ProofContext (ProofReachability, proofClassesReachableFrom)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphAnchor,
  )
import Moonlight.Sheaf.Obstruction
  ( Anchor (..),
    ExactLabelCode (..),
  )

proofTupleSupportedWithReachability ::
  (ClassId -> ClassId) ->
  ProofReachability ->
  ClassId ->
  [EGraphAnchor] ->
  [ExactLabelCode] ->
  Bool
proofTupleSupportedWithReachability canonicalize proofReachability fallbackRootClass anchorValues tupleValue =
  let rootClass =
        fromMaybe fallbackRootClass (rootClassFromTuple anchorValues tupleValue)
      canonicalRootKey = canonicalClassKey canonicalize rootClass
   in tupleValue
        & traverse classIdFromLabel
        & maybe
          False
          (all (classReachableFromRoot canonicalRootKey))
  where
    classReachableFromRoot canonicalRootKey classId =
      IntSet.member
        (canonicalClassKey canonicalize classId)
        (proofClassesReachableFrom (ClassId canonicalRootKey) proofReachability)

supportsProofConstraints :: Maybe ProofReachability -> MatchingRequest owner c SheafCapabilityAtom f runtime -> Bool
supportsProofConstraints maybeProofReachability request =
  isJust maybeProofReachability
    && case GenericMatching.qrPurpose request of
      GenericMatching.FactRulePurpose {} -> False
      _ -> True

rootClassFromTuple ::
  [EGraphAnchor] ->
  [ExactLabelCode] ->
  Maybe ClassId
rootClassFromTuple anchorValues tupleValue =
  zip anchorValues tupleValue
    & List.find (\(anchorValue, _) -> anchorValue == RootAnchor)
    >>= (classIdFromLabel . snd)

classIdFromLabel :: ExactLabelCode -> Maybe ClassId
classIdFromLabel labelCode =
  case labelCode of
    ClassLabelCode classKey -> Just (ClassId classKey)
    FiniteLabelCode {} -> Nothing
    TupleLabelCode {} -> Nothing

canonicalClassKey :: (ClassId -> ClassId) -> ClassId -> Int
canonicalClassKey canonicalize =
  classIdKey . canonicalize
