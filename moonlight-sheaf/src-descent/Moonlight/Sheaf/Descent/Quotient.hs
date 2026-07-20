{-# LANGUAGE RoleAnnotations #-}

-- | Quotient descent: the generic kernel over cover meet-sections with typed
-- obstructions.
module Moonlight.Sheaf.Descent.Quotient
  ( DescentKernel (..),
    QuotientDescentObstruction (..),
    PreparedCoverPlan,
    foldPreparedCoverPlan,
    DescentReport (..),
    CoverMeetSections,
    descentAt,
    descentAtWithPreparedCoverPlan,
    fullDescentCheck,
    preparedCoverPlanAt,
    coverMeetSectionsIn,
    coverMeetSectionsFromContexts,
    coverMeetSectionAt,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (DenseKey)
import Moonlight.Sheaf.Descent.Core
  ( DescentReport (..),
    collectDescentReport,
  )
import Moonlight.Sheaf.Descent.Kernel qualified as CoverKernel
import Moonlight.Sheaf.Context.Section
  ( ContextClassSection (..),
    restrictClassIdWith,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
    preparedContextMeet,
    preparedContextUpperCovers,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict,
    acceptedUnit,
    decidedSearchVerdict,
    rejectedFromList,
    rejectedOne,
  )
import Moonlight.FiniteLattice
  ( ContextLatticeLookupError,
  )

type DescentKernel :: Type -> Type -> Type -> Type
data DescentKernel owner c rep = DescentKernel
  { dkSite :: !(PreparedContextSite owner c),
    dkMaterializedContexts :: ![c],
    dkClassSectionAt :: c -> ContextClassSection rep
  }

type role DescentKernel nominal nominal representational

type QuotientDescentObstruction :: Type -> Type -> Type
data QuotientDescentObstruction c rep
  = QuotientDescentObstruction
      !c
      ![c]
      ![IntMap rep]
  | DescentCoverLookupObstruction
      !c
      !(ContextLatticeLookupError c)
  | DescentClassSectionLookupObstruction
      !c
      !(PreparedContextSupportError c)
  | DescentMeetLookupObstruction
      !c
      ![c]
      !c
      !c
      !(ContextLatticeLookupError c)
  | DescentSupportLookupObstruction
      !c
      ![c]
      !rep
      !(PreparedContextSupportError c)
  | DescentJoinLookupObstruction
      !c
      !c
      !(ContextLatticeLookupError c)
  | DescentVacuousCoverObstruction
      !c
      ![c]
      !(NonEmpty Int)
  | DescentMonotonicityObstruction
      !c
      !c
      !rep
      ![rep]
      ![Int]
  deriving stock (Eq, Show)

type RepSet :: Type -> Type
type RepSet rep = Set rep

type CoverMeetSections :: Type -> Type
type CoverMeetSections classes = IntMap (IntMap classes)

type PreparedCoverPlan :: Type -> Type -> Type -> Type
data PreparedCoverPlan owner c rep
  = PreparedCoverLookupFailed !(ContextLatticeLookupError c)
  | PreparedCoverTrivial
  | PreparedCoverMeetLookupFailed ![c] ![QuotientDescentObstruction c rep]
  | PreparedCoverActive ![c] !(CoverMeetSections c)
  deriving stock (Eq, Show)

type role PreparedCoverPlan nominal nominal representational

foldPreparedCoverPlan ::
  (ContextLatticeLookupError c -> result) ->
  result ->
  ([c] -> [QuotientDescentObstruction c rep] -> result) ->
  ([c] -> CoverMeetSections c -> result) ->
  PreparedCoverPlan owner c rep ->
  result
foldPreparedCoverPlan onLookupFailed onTrivial onMeetLookupFailed onActive coverPlan =
  case coverPlan of
    PreparedCoverLookupFailed lookupError -> onLookupFailed lookupError
    PreparedCoverTrivial -> onTrivial
    PreparedCoverMeetLookupFailed coverContexts obstructions ->
      onMeetLookupFailed coverContexts obstructions
    PreparedCoverActive coverContexts meetSections ->
      onActive coverContexts meetSections
{-# INLINE foldPreparedCoverPlan #-}

repsOf :: Ord rep => ContextClassSection rep -> RepSet rep
repsOf =
  Set.fromList . IntMap.elems . ccsEntries

descentAt ::
  (Ord c, DenseKey rep) =>
  CoverKernel.CoverSearchBudget ->
  DescentKernel owner c rep ->
  c ->
  SearchVerdict (CoverKernel.CoverSearchRefusal Int) (QuotientDescentObstruction c rep)
descentAt budget kernel contextValue =
  descentAtWithPreparedCoverPlan
    budget
    kernel
    contextValue
    (preparedCoverPlanAt (dkSite kernel) contextValue)

descentAtWithPreparedCoverPlan ::
  (DenseKey rep) =>
  CoverKernel.CoverSearchBudget ->
  DescentKernel owner c rep ->
  c ->
  PreparedCoverPlan owner c rep ->
  SearchVerdict (CoverKernel.CoverSearchRefusal Int) (QuotientDescentObstruction c rep)
descentAtWithPreparedCoverPlan budget kernel contextValue coverPlan =
  case coverPlan of
    PreparedCoverLookupFailed lookupError ->
      decidedSearchVerdict (rejectedOne (DescentCoverLookupObstruction contextValue lookupError))
    PreparedCoverTrivial ->
      decidedSearchVerdict acceptedUnit
    PreparedCoverMeetLookupFailed _coverContexts lookupObstructions ->
      decidedSearchVerdict (rejectedFromList lookupObstructions)
    PreparedCoverActive activeCoverContexts meetContexts ->
      let meetSections =
            coverMeetSectionsFromContexts
              meetContexts
              (dkClassSectionAt kernel)
       in CoverKernel.descentAtCover
            budget
            (quotientCoverKernel kernel contextValue activeCoverContexts meetSections)
            contextValue

fullDescentCheck ::
  (Ord c, DenseKey rep) =>
  CoverKernel.CoverSearchBudget ->
  DescentKernel owner c rep ->
  DescentReport c (CoverKernel.CoverSearchRefusal Int) (QuotientDescentObstruction c rep)
fullDescentCheck budget kernel =
  collectDescentReport
    (dkMaterializedContexts kernel)
    (const True)
    (descentAt budget kernel)

quotientCoverKernel :: DenseKey rep => DescentKernel owner c rep -> c -> [c] -> CoverMeetSections (ContextClassSection rep) -> CoverKernel.CoverDescentKernel c Int rep (QuotientDescentObstruction c rep)
quotientCoverKernel kernel parentContext activeCoverContexts meetSections =
  CoverKernel.CoverDescentKernel
    { CoverKernel.cdkMaterializedContexts = dkMaterializedContexts kernel,
      CoverKernel.cdkCoverOf = const activeCoverContexts,
      CoverKernel.cdkCoordinates =
        const CoverKernel.coverCoordinateRange,
      CoverKernel.cdkDomainAt =
        \_parentContext coverElements coordinate ->
          maybe
            []
            (Set.toAscList . repsOf . dkClassSectionAt kernel)
            (CoverKernel.coverContextAt coordinate coverElements),
      CoverKernel.cdkCompatible =
        \_parentContext _coverElements leftCoordinate leftRepresentative rightCoordinate rightRepresentative ->
          CoverKernel.coverPreparedMeetCompatible
            (coverMeetSectionAt meetSections)
            restrictWithClassSection
            leftCoordinate
            leftRepresentative
            rightCoordinate
            rightRepresentative,
      CoverKernel.cdkTupleObstructed =
        \_parentContext _coverElements assignment ->
          not (Set.member assignment parentImageSet),
      CoverKernel.cdkObstructions =
        \parentContext coverElements obstructedAssignments ->
          CoverKernel.obstructionWhenAssignmentsPresent
            ( \assignments ->
                QuotientDescentObstruction
                  parentContext
                  coverElements
                  (CoverKernel.intAssignmentsToIntMaps assignments)
            )
            obstructedAssignments,
      CoverKernel.cdkVacuousObstruction =
        DescentVacuousCoverObstruction
    }
  where
    parentImageSet =
      parentRepresentativeImageSet kernel parentContext activeCoverContexts

    restrictWithClassSection ::
      DenseKey classId =>
      ContextClassSection classId ->
      classId ->
      classId
    restrictWithClassSection targetSection =
      restrictClassIdWith (ccsEntries targetSection)

parentRepresentativeImageSet :: DenseKey rep => DescentKernel owner c rep -> c -> [c] -> Set (Map.Map Int rep)
parentRepresentativeImageSet kernel parentContext coverElements =
  Set.fromList
    [ Map.fromList
        [ (coordinate, restrictWithClassSection coverClasses representative)
        | (coordinate, coverContext) <- zip [0 :: Int ..] coverElements,
          let coverClasses = dkClassSectionAt kernel coverContext
        ]
    | representative <- Set.toAscList (repsOf (dkClassSectionAt kernel parentContext))
    ]
  where
    restrictWithClassSection ::
      DenseKey classId =>
      ContextClassSection classId ->
      classId ->
      classId
    restrictWithClassSection targetSection =
      restrictClassIdWith (ccsEntries targetSection)

preparedCoverPlanAt ::
  Ord c =>
  PreparedContextSite owner c ->
  c ->
  PreparedCoverPlan owner c rep
preparedCoverPlanAt site parentContext =
  case preparedContextUpperCovers site parentContext of
    Left lookupError ->
      PreparedCoverLookupFailed lookupError
    Right coverContexts ->
      maybe
        PreparedCoverTrivial
        ( \activeCoverContexts ->
            case coverMeetSectionsIn site parentContext activeCoverContexts id of
              Right meetContexts ->
                PreparedCoverActive activeCoverContexts meetContexts
              Left lookupObstructions ->
                PreparedCoverMeetLookupFailed activeCoverContexts lookupObstructions
        )
        (CoverKernel.nontrivialCover coverContexts)

coverMeetSectionsIn ::
  Ord c =>
  PreparedContextSite owner c ->
  c ->
  [c] ->
  (c -> classes) ->
  Either [QuotientDescentObstruction c rep] (CoverMeetSections classes)
coverMeetSectionsIn site parentContext coverContexts classSectionAt =
  IntMap.fromAscList
    <$> traverse meetSectionRow (zip [0 :: Int ..] coverContexts)
  where
    meetSectionRow (leftCoordinate, leftContext) =
      fmap
        ((,) leftCoordinate . IntMap.fromAscList)
        (traverse (meetSectionCell leftContext) (zip [0 :: Int ..] coverContexts))

    meetSectionCell leftContext (rightCoordinate, rightContext) =
      first
        (\lookupError -> [DescentMeetLookupObstruction parentContext coverContexts leftContext rightContext lookupError])
        (fmap ((,) rightCoordinate . classSectionAt) (preparedContextMeet site leftContext rightContext))

coverMeetSectionsFromContexts ::
  CoverMeetSections c ->
  (c -> classes) ->
  CoverMeetSections classes
coverMeetSectionsFromContexts meetContexts classSectionAt =
  fmap (fmap classSectionAt) meetContexts

coverMeetSectionAt :: CoverMeetSections classes -> Int -> Int -> Maybe classes
coverMeetSectionAt meetSections leftCoordinate rightCoordinate =
  IntMap.lookup leftCoordinate meetSections
    >>= IntMap.lookup rightCoordinate
