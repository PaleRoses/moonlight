{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Descent.Context.Support
  ( supportFiberObstructions,
    supportFiberObstructionsWithPreparedCoverPlan,
  )
where

import Data.Void (Void)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Moonlight.Core (DenseKey (decodeDenseKey, encodeDenseKey))
import Moonlight.Sheaf.Context.Algebra
  ( ContextAlgebraSite,
    ContextSiteOwner,
    classesFor,
  )
import Moonlight.Sheaf.Context.Core
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
    decidedSearchVerdict,
    rejectedFromList,
    rejectedOne,
  )
import Moonlight.Sheaf.Descent.Quotient
  ( QuotientDescentObstruction (..),
    PreparedCoverPlan,
    foldPreparedCoverPlan,
    preparedCoverPlanAt,
  )

-- | Descent at a parent context is a theorem when sections are monotone
-- along the lattice order (see docs/ELECTION-FREE-DESCENT.md); the check
-- pins the theorem's hypothesis, linearly and election-free, instead of
-- searching for compatible cover families.
supportFiberObstructions ::
  ContextAlgebraSite store ctx classId analysis =>
  PreparedContextSite (ContextSiteOwner store) ctx ->
  ctx ->
  store ->
  SearchVerdict Void (QuotientDescentObstruction ctx classId)
supportFiberObstructions site parentContext store =
  supportFiberObstructionsWithPreparedCoverPlan
    parentContext
    (preparedCoverPlanAt site parentContext)
    store

supportFiberObstructionsWithPreparedCoverPlan ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  PreparedCoverPlan (ContextSiteOwner store) ctx classId ->
  store ->
  SearchVerdict Void (QuotientDescentObstruction ctx classId)
supportFiberObstructionsWithPreparedCoverPlan parentContext coverPlan store =
  foldPreparedCoverPlan
    (decidedSearchVerdict . rejectedOne . DescentCoverLookupObstruction parentContext)
    SearchAccepted
    (\_activeCoverContexts meetObstructions -> decidedSearchVerdict (rejectedFromList meetObstructions))
    ( \activeCoverContexts _meetContexts ->
        decidedSearchVerdict
          ( rejectedFromList
              (sectionMonotonicityObstructions parentContext activeCoverContexts store)
          )
    )
    coverPlan

sectionMonotonicityObstructions ::
  ContextAlgebraSite store ctx classId analysis =>
  ctx ->
  [ctx] ->
  store ->
  [QuotientDescentObstruction ctx classId]
sectionMonotonicityObstructions parentContext coverContexts store =
  case (classesFor parentContext store, traverse keyedCoverSection coverContexts) of
    (Left lookupFailure, _) ->
      [DescentClassSectionLookupObstruction parentContext lookupFailure]
    (_, Left lookupObstructions) ->
      lookupObstructions
    (Right parentSection, Right coverSections) ->
      let parentClassGroups =
            IntMap.fromListWith
              (<>)
              [ (encodeDenseKey classId, [memberKey])
                | (memberKey, classId) <- IntMap.toList parentSection
              ]
       in [ DescentMonotonicityObstruction
              parentContext
              coverContext
              (decodeDenseKey parentClassKey)
              (fmap decodeDenseKey (IntSet.toAscList divergentImageKeys))
              missingMemberKeys
            | (coverContext, coverSection) <- coverSections,
              (parentClassKey, memberKeys) <- IntMap.toAscList parentClassGroups,
              let (missingMemberKeys, imageKeys) =
                    partitionEithers
                      [ maybe (Left memberKey) (Right . encodeDenseKey) (IntMap.lookup memberKey coverSection)
                        | memberKey <- memberKeys
                      ],
              let divergentImageKeys = IntSet.fromList imageKeys,
              not (null missingMemberKeys) || IntSet.size divergentImageKeys > 1
          ]
  where
    keyedCoverSection coverContext =
      first
        (\lookupFailure -> [DescentClassSectionLookupObstruction coverContext lookupFailure])
        (fmap ((,) coverContext) (classesFor coverContext store))
