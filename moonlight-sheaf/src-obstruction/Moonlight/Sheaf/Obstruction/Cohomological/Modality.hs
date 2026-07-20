{-# LANGUAGE PatternSynonyms #-}

module Moonlight.Sheaf.Obstruction.Cohomological.Modality
  ( LoweringSite (..),
    LoweringGap (..),
    data MissingReferenceGap,
    data UnsupportedAnchorGap,
    ModalityContribution (..),
    CapabilityLabelAlgebra (..),
    emptyCapabilityLabelAlgebra,
    discreteCapabilityLabelAlgebra,
    CapabilityRow,
    emptyCapabilityRow,
    capabilityRowFromList,
    capabilityRowMembers,
    finiteCapabilityRowAlgebra,
    mapCapabilityLabelAlgebra,
    CapabilitySupport (..),
    CapabilityEnvironment (..),
    emptyCapabilityEnvironment,
    TypedCapabilitySupport (..),
    TypedCapabilityEnvironment (..),
    emptyTypedCapabilityEnvironment,
    lowerCapabilityEnvironment,
    lowerCapabilityEnvironmentWithGaps,
    capabilityModality,
    typedCapabilityModality,
    obstructionModality,
    obstructionModalityWithReification,
    ObstructionModality (..),
    ModalityRegistry,
    emptyModalityRegistry,
    registerModality,
    modalityRegistryFromList,
    modalityRegistryKeys,
    modalityRegistryProjection,
    modalityRegistryProjectionConflicts,
    modalityRegistryReification,
    evaluateModalities,
    evaluateModalityRegistry,
  )
where

import Data.Bits (setBit, zeroBits)
import Data.Containers.ListUtils (nubOrd)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Either (fromLeft, lefts, rights)
import Data.Function ((&))
import Data.GADT.Compare (GCompare)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Moonlight.Sheaf.Obstruction.Cohomological.Core.Environment
  ( IndexedEnvironment,
    lookupEnvironmentBinding,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Projection
  ( RelationProjectionMode (..),
    RelationProjectionConflict,
    RelationProjectionPolicy,
    SectionCoordinate,
    SectionProjection,
    combineRelationProjectionPolicies,
    relationProjectionPolicyFor,
    sectionCoordinateProjection,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionReification,
    emptySectionReification,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( ConstraintId (..),
    ExactConstraint (..),
    data CapabilityConstraint,
    RelationFlavor (CapabilityFlavor),
    ExactLabelCode (..),
  )

type LoweringSite :: Type -> Type -> Type
data LoweringSite anchor ref
  = MissingReferences ![ref]
  | UnsupportedAnchor !anchor
  deriving stock (Eq, Ord, Show, Read)

type LoweringGap :: Type -> Type -> Type
data LoweringGap anchor ref = LoweringGap
  { lgFlavor :: !RelationFlavor,
    lgConstraintId :: !ConstraintId,
    lgSite :: !(LoweringSite anchor ref)
  }
  deriving stock (Eq, Ord, Show, Read)

pattern MissingReferenceGap :: RelationFlavor -> ConstraintId -> [ref] -> LoweringGap anchor ref
pattern MissingReferenceGap relationFlavor constraintId refs =
  LoweringGap relationFlavor constraintId (MissingReferences refs)

pattern UnsupportedAnchorGap :: RelationFlavor -> ConstraintId -> anchor -> LoweringGap anchor ref
pattern UnsupportedAnchorGap relationFlavor constraintId anchorValue =
  LoweringGap relationFlavor constraintId (UnsupportedAnchor anchorValue)

type ModalityContribution :: Type -> Type -> Type
data ModalityContribution anchor ref = ModalityContribution
  { mcExactConstraints :: ![ExactConstraint anchor],
    mcLoweringGaps :: ![LoweringGap anchor ref]
  }
  deriving stock (Eq, Show, Read)

instance Semigroup (ModalityContribution anchor ref) where
  leftContribution <> rightContribution =
    ModalityContribution
      { mcExactConstraints =
          mcExactConstraints leftContribution <> mcExactConstraints rightContribution,
        mcLoweringGaps =
          mcLoweringGaps leftContribution <> mcLoweringGaps rightContribution
      }

instance Monoid (ModalityContribution anchor ref) where
  mempty =
    ModalityContribution
      { mcExactConstraints = [],
        mcLoweringGaps = []
      }

type ObstructionModality :: Type -> Type -> Type -> Type -> Type
data ObstructionModality anchor result ref value = ObstructionModality
  { modalityProjectionPolicy :: !RelationProjectionPolicy,
    modalityReification :: value -> SectionReification (SectionCoordinate anchor) result,
    runObstructionModality ::
      ConstraintId ->
      value ->
      (ConstraintId, ModalityContribution anchor ref)
  }

obstructionModality ::
  RelationProjectionPolicy ->
  (ConstraintId -> value -> (ConstraintId, ModalityContribution anchor ref)) ->
  ObstructionModality anchor result ref value
obstructionModality relationProjectionPolicy runModality =
  ObstructionModality
    { modalityProjectionPolicy = relationProjectionPolicy,
      modalityReification = const emptySectionReification,
      runObstructionModality = runModality
    }

obstructionModalityWithReification ::
  RelationProjectionPolicy ->
  (value -> SectionReification (SectionCoordinate anchor) result) ->
  (ConstraintId -> value -> (ConstraintId, ModalityContribution anchor ref)) ->
  ObstructionModality anchor result ref value
obstructionModalityWithReification =
  ObstructionModality

type CapabilityLabelAlgebra :: Type -> Type
data CapabilityLabelAlgebra label = CapabilityLabelAlgebra
  { claUniverse :: ![label],
    claAdmissible :: label -> Bool,
    claCombine :: label -> label -> Maybe label,
    claRefines :: label -> label -> Bool,
    claEncode :: label -> ExactLabelCode
  }

emptyCapabilityLabelAlgebra :: CapabilityLabelAlgebra label
emptyCapabilityLabelAlgebra =
  CapabilityLabelAlgebra
    { claUniverse = [],
      claAdmissible = const False,
      claCombine = \_ _ -> Nothing,
      claRefines = \_ _ -> False,
      claEncode = const (FiniteLabelCode 0)
    }

type CapabilityRow :: Type -> Type
newtype CapabilityRow capability = CapabilityRow
  { unCapabilityRow :: Set.Set capability
  }
  deriving stock (Eq, Ord, Show, Read)

emptyCapabilityRow :: CapabilityRow capability
emptyCapabilityRow =
  CapabilityRow Set.empty

capabilityRowFromList :: Ord capability => [capability] -> CapabilityRow capability
capabilityRowFromList =
  CapabilityRow . Set.fromList

capabilityRowMembers :: CapabilityRow capability -> [capability]
capabilityRowMembers =
  Set.toAscList . unCapabilityRow

mapCapabilityLabelAlgebra ::
  (outer -> inner) ->
  (inner -> outer) ->
  CapabilityLabelAlgebra inner ->
  CapabilityLabelAlgebra outer
mapCapabilityLabelAlgebra toInner fromInner innerAlgebra =
  CapabilityLabelAlgebra
    { claUniverse = fmap fromInner (claUniverse innerAlgebra),
      claAdmissible =
        claAdmissible innerAlgebra
          . toInner,
      claCombine =
        \leftValue rightValue ->
          fmap fromInner
            ( claCombine
                innerAlgebra
                (toInner leftValue)
                (toInner rightValue)
            ),
      claRefines =
        \leftValue rightValue ->
          claRefines
            innerAlgebra
            (toInner leftValue)
            (toInner rightValue),
      claEncode =
        claEncode innerAlgebra
          . toInner
    }

discreteCapabilityLabelAlgebra ::
  (label -> ExactLabelCode) ->
  CapabilityLabelAlgebra label
discreteCapabilityLabelAlgebra encodeLabel =
  CapabilityLabelAlgebra
    { claUniverse = [],
      claAdmissible = const True,
      claCombine =
        \leftLabel rightLabel ->
          if encodeLabel leftLabel == encodeLabel rightLabel
            then Just leftLabel
            else Nothing,
      claRefines =
        \leftLabel rightLabel ->
          encodeLabel leftLabel == encodeLabel rightLabel,
      claEncode = encodeLabel
    }

finiteCapabilityRowAlgebra ::
  Ord capability =>
  [capability] ->
  CapabilityLabelAlgebra (CapabilityRow capability)
finiteCapabilityRowAlgebra capabilityUniverse =
  let canonicalUniverse =
        capabilityUniverse
          & Set.fromList
          & Set.toAscList
   in CapabilityLabelAlgebra
        { claUniverse = [],
          claAdmissible =
            \capabilityRow ->
              rowWithinUniverse canonicalUniverse capabilityRow,
          claCombine =
            \leftRow rightRow ->
              let combinedRow = combineCapabilityRows leftRow rightRow
               in if rowWithinUniverse canonicalUniverse combinedRow
                    then Just combinedRow
                    else Nothing,
          claRefines = refineCapabilityRows,
          claEncode =
            FiniteLabelCode
              . encodeCapabilityRow canonicalUniverse
        }

type CapabilitySupport :: Type -> Type
data CapabilitySupport anchor = CapabilitySupport
  { csAnchors :: ![anchor],
    csSupportedCapabilities :: ![[ExactLabelCode]]
  }
  deriving stock (Eq, Show, Read)

type CapabilityEnvironment :: Type -> Type
newtype CapabilityEnvironment anchor = CapabilityEnvironment
  { ceSupports :: [CapabilitySupport anchor]
  }
  deriving stock (Show, Read)
  deriving newtype (Eq)

emptyCapabilityEnvironment :: CapabilityEnvironment anchor
emptyCapabilityEnvironment =
  CapabilityEnvironment []

type TypedCapabilitySupport :: Type -> Type -> Type
data TypedCapabilitySupport label anchor = TypedCapabilitySupport
  { tcsAnchors :: ![anchor],
    tcsSupportedCapabilities :: ![[label]]
  }
  deriving stock (Eq, Show, Read)

type TypedCapabilityEnvironment :: Type -> Type -> Type
data TypedCapabilityEnvironment label anchor = TypedCapabilityEnvironment
  { tceLabelAlgebra :: !(CapabilityLabelAlgebra label),
    tceSupports :: [TypedCapabilitySupport label anchor]
  }

emptyTypedCapabilityEnvironment ::
  CapabilityLabelAlgebra label ->
  TypedCapabilityEnvironment label anchor
emptyTypedCapabilityEnvironment labelAlgebra =
  TypedCapabilityEnvironment labelAlgebra []

lowerCapabilityEnvironment ::
  Ord anchor =>
  TypedCapabilityEnvironment label anchor ->
  CapabilityEnvironment anchor
lowerCapabilityEnvironment environment =
  fst (lowerCapabilityEnvironmentWithGaps environment)

lowerCapabilityEnvironmentWithGaps ::
  Ord anchor =>
  TypedCapabilityEnvironment label anchor ->
  (CapabilityEnvironment anchor, [anchor])
lowerCapabilityEnvironmentWithGaps environment =
  let loweredSupports =
        fmap
          (lowerCapabilitySupportWithGaps (tceLabelAlgebra environment))
          (tceSupports environment)
   in ( CapabilityEnvironment (fmap fst loweredSupports),
        foldMap snd loweredSupports
      )

capabilityModality :: ObstructionModality anchor result ref (CapabilityEnvironment anchor)
capabilityModality =
  obstructionModality
    (relationProjectionPolicyFor CapabilityFlavor RelationalProjection)
    (\startingId environment ->
       let capabilityConstraints =
             zipWith
               (\offset capabilitySupport ->
                  CapabilityConstraint
                    (ConstraintId (unConstraintId startingId + offset))
                    (csAnchors capabilitySupport)
                    (csSupportedCapabilities capabilitySupport)
               )
               [0 :: Int ..]
               (ceSupports environment)
        in ( ConstraintId (unConstraintId startingId + length capabilityConstraints),
             ModalityContribution
               { mcExactConstraints = capabilityConstraints,
                 mcLoweringGaps = []
               }
           )
    )

typedCapabilityModality ::
  Ord anchor =>
  ObstructionModality anchor result ref (TypedCapabilityEnvironment label anchor)
typedCapabilityModality =
  obstructionModality
    (relationProjectionPolicyFor CapabilityFlavor RelationalProjection)
    (\startingId environment ->
       let loweredSupports =
             tceSupports environment
               & zip [0 :: Int ..]
               & fmap
                 (\(offset, typedSupport) ->
                    let constraintId =
                          ConstraintId (unConstraintId startingId + offset)
                        (loweredSupport, failedAnchors) =
                          lowerCapabilitySupportWithGaps
                            (tceLabelAlgebra environment)
                            typedSupport
                     in ( loweredSupport,
                          fmap
                            (UnsupportedAnchorGap CapabilityFlavor constraintId)
                            failedAnchors
                        )
                 )
           loweredEnvironment =
             CapabilityEnvironment (fmap fst loweredSupports)
           (nextConstraintId, contribution) =
             runObstructionModality
               capabilityModality
               startingId
               loweredEnvironment
        in ( nextConstraintId,
             contribution
               { mcLoweringGaps =
                   mcLoweringGaps contribution
                     <> foldMap snd loweredSupports
               }
           )
    )

type ModalityRegistry :: (Type -> Type) -> Type -> Type -> Type -> Type
type ModalityRegistry key anchor result ref =
  DMap key (ObstructionModality anchor result ref)

emptyModalityRegistry :: ModalityRegistry key anchor result ref
emptyModalityRegistry =
  DMap.empty

registerModality ::
  GCompare key =>
  key value ->
  ObstructionModality anchor result ref value ->
  ModalityRegistry key anchor result ref ->
  ModalityRegistry key anchor result ref
registerModality =
  DMap.insert

modalityRegistryFromList ::
  GCompare key =>
  [DSum key (ObstructionModality anchor result ref)] ->
  ModalityRegistry key anchor result ref
modalityRegistryFromList =
  DMap.fromList

modalityRegistryKeys ::
  ModalityRegistry key anchor result ref ->
  DMap key Proxy
modalityRegistryKeys =
  DMap.map (const Proxy)

modalityRegistryProjectionConflicts ::
  ModalityRegistry key anchor result ref ->
  [RelationProjectionConflict]
modalityRegistryProjectionConflicts =
  fromLeft [] . modalityRegistryProjection

modalityRegistryProjection ::
  ModalityRegistry key anchor result ref ->
  Either [RelationProjectionConflict] (SectionProjection anchor (SectionCoordinate anchor))
modalityRegistryProjection registry =
  sectionCoordinateProjection
    <$> combineRelationProjectionPolicies
      ( fmap
          (\(_ :=> modality) -> modalityProjectionPolicy modality)
          (DMap.toAscList registry)
      )

modalityRegistryReification ::
  GCompare key =>
  IndexedEnvironment key ->
  ModalityRegistry key anchor result ref ->
  SectionReification (SectionCoordinate anchor) result
modalityRegistryReification environment =
  foldMap
    (\(modalityKey :=> modality) ->
       maybe
         emptySectionReification
         (modalityReification modality)
         (lookupEnvironmentBinding modalityKey environment)
    )
    . DMap.toAscList

evaluateModalities ::
  ConstraintId ->
  value ->
  [ObstructionModality anchor result ref value] ->
  ModalityContribution anchor ref
evaluateModalities initialConstraintId environment =
  snd
    . List.foldl'
      (\(nextConstraintId, contribution) modality ->
         let (nextConstraintId', contribution') =
               runObstructionModality modality nextConstraintId environment
          in (nextConstraintId', contribution <> contribution')
      )
      (initialConstraintId, mempty)

evaluateModalityRegistry ::
  GCompare key =>
  ConstraintId ->
  IndexedEnvironment key ->
  ModalityRegistry key anchor result ref ->
  ModalityContribution anchor ref
evaluateModalityRegistry initialConstraintId environment =
  snd
    . List.foldl'
      (\(nextConstraintId, contribution) (modalityKey :=> modality) ->
         maybe
           (nextConstraintId, contribution)
           (\payload ->
              let (nextConstraintId', contribution') =
                    runObstructionModality modality nextConstraintId payload
               in (nextConstraintId', contribution <> contribution')
           )
           (lookupEnvironmentBinding modalityKey environment)
      )
      (initialConstraintId, mempty)
    . DMap.toAscList

lowerCapabilitySupportWithGaps ::
  Ord anchor =>
  CapabilityLabelAlgebra label ->
  TypedCapabilitySupport label anchor ->
  (CapabilitySupport anchor, [anchor])
lowerCapabilitySupportWithGaps algebra typedSupport =
  let canonicalAnchors =
        nubOrd (tcsAnchors typedSupport)
      loweredTuples =
        fmap
          (lowerCapabilityTupleWithGaps algebra (tcsAnchors typedSupport) canonicalAnchors)
          (tcsSupportedCapabilities typedSupport)
   in ( CapabilitySupport
          { csAnchors = canonicalAnchors,
            csSupportedCapabilities =
              rights loweredTuples
          },
        concat (lefts loweredTuples)
      )

lowerCapabilityTupleWithGaps ::
  Ord anchor =>
  CapabilityLabelAlgebra label ->
  [anchor] ->
  [anchor] ->
  [label] ->
  Either [anchor] [ExactLabelCode]
lowerCapabilityTupleWithGaps algebra anchors canonicalAnchors labels
  | length anchors /= length labels =
      Left canonicalAnchors
  | otherwise =
      combineCapabilityAssignmentsWithGaps algebra anchors labels
        >>= \assignmentMap ->
          traverse
            (\anchorValue ->
               maybe
                 (Left [anchorValue])
                 (Right . claEncode algebra)
                 (Map.lookup anchorValue assignmentMap)
            )
            canonicalAnchors

combineCapabilityAssignmentsWithGaps ::
  Ord anchor =>
  CapabilityLabelAlgebra label ->
  [anchor] ->
  [label] ->
  Either [anchor] (Map anchor label)
combineCapabilityAssignmentsWithGaps algebra anchors =
  List.foldl'
    (\eitherAssignments (anchorValue, labelValue) ->
       eitherAssignments
         >>= \assignments ->
           canonicalCapabilityLabel algebra labelValue
             & maybe
               (Left [anchorValue])
               (\canonicalLabel ->
                  case Map.lookup anchorValue assignments of
                    Nothing ->
                      Right (Map.insert anchorValue canonicalLabel assignments)
                    Just existingLabel ->
                      claCombine algebra existingLabel canonicalLabel
                        & maybe
                          (Left [anchorValue])
                          (\combinedLabel -> Right (Map.insert anchorValue combinedLabel assignments))
               )
    )
    (Right Map.empty)
    . List.zip anchors

canonicalCapabilityLabel ::
  CapabilityLabelAlgebra label ->
  label ->
  Maybe label
canonicalCapabilityLabel algebra labelValue =
  List.foldr
    (\candidateLabel maybeRepresentative ->
       if claRefines algebra labelValue candidateLabel
         && claRefines algebra candidateLabel labelValue
         then Just candidateLabel
         else maybeRepresentative
    )
    (if claAdmissible algebra labelValue then Just labelValue else Nothing)
    (claUniverse algebra)

rowWithinUniverse ::
  Ord capability =>
  [capability] ->
  CapabilityRow capability ->
  Bool
rowWithinUniverse capabilityUniverse (CapabilityRow capabilitySet) =
  capabilitySet `Set.isSubsetOf` Set.fromList capabilityUniverse

combineCapabilityRows ::
  Ord capability =>
  CapabilityRow capability ->
  CapabilityRow capability ->
  CapabilityRow capability
combineCapabilityRows (CapabilityRow leftRow) (CapabilityRow rightRow) =
  CapabilityRow (Set.union leftRow rightRow)

refineCapabilityRows ::
  Ord capability =>
  CapabilityRow capability ->
  CapabilityRow capability ->
  Bool
refineCapabilityRows (CapabilityRow leftRow) (CapabilityRow rightRow) =
  Set.isSubsetOf rightRow leftRow

encodeCapabilityRow ::
  Ord capability =>
  [capability] ->
  CapabilityRow capability ->
  Integer
encodeCapabilityRow capabilityUniverse (CapabilityRow capabilitySet) =
  capabilityUniverse
    & List.zip [0 :: Int ..]
    & List.foldl'
      (\encodedValue (bitIndex, capabilityValue) ->
         if Set.member capabilityValue capabilitySet
           then setBit encodedValue bitIndex
           else encodedValue
      )
      zeroBits
