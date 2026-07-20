{-# LANGUAGE RecordWildCards #-}

module Moonlight.Sheaf.Inference.Algebra where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (minimumBy, sortOn, tails)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU
import Moonlight.Sheaf.Inference.Types
import Prelude

inferLogZWithOrder
  :: [Int]
  -> DomainIndex pid obj
  -> [WeightedFactor]
  -> Either InferenceExecutionError Double
inferLogZWithOrder order index factors0 = do
  let (const0, nonConst0) = pullConstantFactors factors0
  if isImpossibleLogWeight const0
    then Left InferenceImpossiblePosterior
    else do
      (constN, factorsN, _) <-
        foldM
          (eliminateOneVariable sumElim (\_ auxes -> auxes) (diDomainSizes index))
          (const0, nonConst0, [])
          order
      tailConst <- collapseConstantProduct (diDomainSizes index) factorsN
      possibleLogWeight InferenceImpossiblePosterior (addProductLogWeights constN tailConst)

sumElim :: VU.Vector Int -> Int -> WeightedFactor -> (WeightedFactor, ())
sumElim domainSizes varIdx factor = (sumOutVar domainSizes varIdx factor, ())

inferLogZAndMarginalsWithOrder
  :: [Int]
  -> DomainIndex pid obj
  -> [WeightedFactor]
  -> Either InferenceExecutionError (Double, VB.Vector [Double])
inferLogZAndMarginalsWithOrder order index factors0 =
  let (const0, _nonConst0) = pullConstantFactors factors0
      !varCount = VB.length (diVars index)
   in if isImpossibleLogWeight const0
        then Left InferenceImpossiblePosterior
        else do
          sumProductTrace <- buildSumProductTrace order (diDomainSizes index) factors0
          marginals <- sumProductMarginals (diDomainSizes index) varCount sumProductTrace
          logPartition <- possibleLogWeight InferenceImpossiblePosterior (sptLogZ sumProductTrace)
          pure (logPartition, marginals)

inferMapWithOrder
  :: [Int]
  -> DomainIndex pid obj
  -> [WeightedFactor]
  -> Either InferenceExecutionError (MapSolution pid obj)
inferMapWithOrder order index factors0 = do
  let (const0, nonConst0) = pullConstantFactors factors0
  if isImpossibleLogWeight const0
    then Left InferenceNoMapAssignment
    else do
      (constN, factorsN, decisionsRev) <-
        foldM
          (eliminateOneVariable maxOutVar (:) (diDomainSizes index))
          (const0, nonConst0, [])
          order
      tailConst <- collapseConstantProduct (diDomainSizes index) factorsN
      let logScore = addProductLogWeights constN tailConst
      if isImpossibleLogWeight logScore
        then Left InferenceNoMapAssignment
        else do
          assignmentIx <- reconstructAssignment (diDomainSizes index) decisionsRev
          let assignment =
                Map.fromDistinctAscList
                  [ (diVars index VB.! vi, (diDomains index VB.! vi) VB.! valueIx)
                  | (vi, valueIx) <- IntMap.toAscList assignmentIx
                  ]
          pure
            MapSolution
              { msAssignment = assignment
              , msLogScore = logWeightValue logScore
              }

data TaggedFactor = TaggedFactor
  { tfOrigin :: !(Maybe Int)
  , tfFactor :: !WeightedFactor
  }

taggedFactorValue :: TaggedFactor -> WeightedFactor
taggedFactorValue =
  tfFactor
{-# INLINE taggedFactorValue #-}

data SumProductBucket = SumProductBucket
  { spbIndex :: !Int
  , spbVariable :: !Int
  , spbFactors :: ![TaggedFactor]
  , spbMessageScope :: !(VU.Vector Int)
  , spbParent :: !(Maybe Int)
  }

data SumProductTrace = SumProductTrace
  { sptLogZ :: !LogWeight
  , sptBucketsByIndex :: !(IntMap SumProductBucket)
  , sptBucketsByVariable :: !(IntMap SumProductBucket)
  }

buildSumProductTrace
  :: [Int]
  -> VU.Vector Int
  -> [WeightedFactor]
  -> Either InferenceExecutionError SumProductTrace
buildSumProductTrace order domainSizes factors0 = do
  let (const0, nonConst0) = pullConstantFactors factors0
      !orderPositions = IntMap.fromList (zip order [0 ..])
  (constN, factorsN, bucketsRev, _nextIndex) <-
    foldM
      (sumProductEliminateOne orderPositions domainSizes)
      (const0, fmap (TaggedFactor Nothing) nonConst0, [], 0 :: Int)
      order
  logZ <-
    if isImpossibleLogWeight constN
      then Right impossibleLogWeight
      else fmap (addProductLogWeights constN) (collapseConstantProduct domainSizes (fmap taggedFactorValue factorsN))
  let !buckets = reverse bucketsRev
      !bucketsByIndex =
        IntMap.fromList
          [ (spbIndex bucket, bucket)
          | bucket <- buckets
          ]
      !bucketsByVariable =
        IntMap.fromList
          [ (spbVariable bucket, bucket)
          | bucket <- buckets
          ]
  pure
    SumProductTrace
      { sptLogZ = logZ
      , sptBucketsByIndex = bucketsByIndex
      , sptBucketsByVariable = bucketsByVariable
      }

sumProductEliminateOne
  :: IntMap Int
  -> VU.Vector Int
  -> (LogWeight, [TaggedFactor], [SumProductBucket], Int)
  -> Int
  -> Either InferenceExecutionError (LogWeight, [TaggedFactor], [SumProductBucket], Int)
sumProductEliminateOne orderPositions domainSizes (!constAcc, !factors, !buckets, !bucketIndex) !varIdx
  | isImpossibleLogWeight constAcc = Right (constAcc, factors, buckets, bucketIndex + 1)
  | otherwise =
      let (!bucket, !rest) = extractTaggedBucket varIdx factors
       in if null bucket
            then Right (constAcc, rest, buckets, bucketIndex + 1)
            else do
              joint <- multiplyMany domainSizes (fmap taggedFactorValue bucket)
              let !reduced = sumOutVar domainSizes varIdx joint
                  !messageScope = wfScope reduced
                  !parent = parentBucketFor orderPositions messageScope
                  !bucketValue =
                    SumProductBucket
                      { spbIndex = bucketIndex
                      , spbVariable = varIdx
                      , spbFactors = bucket
                      , spbMessageScope = messageScope
                      , spbParent = parent
                      }
              case factorConstantLog reduced of
                Just c ->
                  Right
                    ( addProductLogWeights constAcc c,
                      rest,
                      bucketValue : buckets,
                      bucketIndex + 1
                    )
                Nothing ->
                  Right
                    ( constAcc,
                      TaggedFactor (Just bucketIndex) reduced : rest,
                      bucketValue : buckets,
                      bucketIndex + 1
                    )

parentBucketFor :: IntMap Int -> VU.Vector Int -> Maybe Int
parentBucketFor orderPositions scope =
  fmap
    snd
    ( foldl'
        chooseEarlier
        Nothing
        (VU.toList scope)
    )
  where
    chooseEarlier Nothing variable =
      fmap (\position -> (position, position)) (IntMap.lookup variable orderPositions)
    chooseEarlier current@(Just (bestPosition, _)) variable =
      case IntMap.lookup variable orderPositions of
        Just position
          | position < bestPosition -> Just (position, position)
        _ -> current

sumProductMarginals
  :: VU.Vector Int
  -> Int
  -> SumProductTrace
  -> Either InferenceExecutionError (VB.Vector [Double])
sumProductMarginals domainSizes varCount sumProductTrace
  | isImpossibleLogWeight (sptLogZ sumProductTrace) = Left InferenceImpossiblePosterior
  | otherwise = do
      outside <- outsideFactorsForTrace domainSizes sumProductTrace
      VB.generateM
        varCount
        (sumProductMarginalForVariable domainSizes sumProductTrace outside)

sumProductMarginalForVariable
  :: VU.Vector Int
  -> SumProductTrace
  -> IntMap WeightedFactor
  -> Int
  -> Either InferenceExecutionError [Double]
sumProductMarginalForVariable domainSizes sumProductTrace outside variable =
  case IntMap.lookup variable (sptBucketsByVariable sumProductTrace) of
    Nothing ->
      normalizeLogMasses (replicate (domainSizes VU.! variable) zeroLogWeight)
    Just bucket -> do
      let !outsideFactor = IntMap.findWithDefault unitFactor (spbIndex bucket) outside
      belief <- multiplyMany domainSizes (outsideFactor : fmap taggedFactorValue (spbFactors bucket))
      marginal <- projectFactorToScopeSum domainSizes (VU.singleton variable) belief
      normalizeLogMasses (unaryLogWeights (domainSizes VU.! variable) marginal)

outsideFactorsForTrace
  :: VU.Vector Int
  -> SumProductTrace
  -> Either InferenceExecutionError (IntMap WeightedFactor)
outsideFactorsForTrace domainSizes sumProductTrace =
  foldM
    propagateFromParent
    IntMap.empty
    (reverse (IntMap.keys (sptBucketsByIndex sumProductTrace)))
  where
    childrenByParent =
      traceChildrenByParent sumProductTrace

    propagateFromParent outside parentIndex =
      case IntMap.lookup parentIndex (sptBucketsByIndex sumProductTrace) of
        Nothing -> Right outside
        Just parentBucket ->
          foldM
            (insertChildOutside parentBucket)
            outside
            (IntMap.findWithDefault [] parentIndex childrenByParent)

    insertChildOutside parentBucket outside childIndex =
      case IntMap.lookup childIndex (sptBucketsByIndex sumProductTrace) of
        Nothing -> Right outside
        Just childBucket -> do
          childOutside <-
            outsideFactorForChild
              domainSizes
              (IntMap.findWithDefault unitFactor (spbIndex parentBucket) outside)
              parentBucket
              childBucket
          pure (IntMap.insert childIndex childOutside outside)

traceChildrenByParent :: SumProductTrace -> IntMap [Int]
traceChildrenByParent sumProductTrace =
  foldl'
    insertChild
    IntMap.empty
    (IntMap.elems (sptBucketsByIndex sumProductTrace))
  where
    insertChild acc bucket =
      case spbParent bucket of
        Nothing -> acc
        Just parent ->
          IntMap.insertWith (<>) parent [spbIndex bucket] acc

outsideFactorForChild
  :: VU.Vector Int
  -> WeightedFactor
  -> SumProductBucket
  -> SumProductBucket
  -> Either InferenceExecutionError WeightedFactor
outsideFactorForChild domainSizes parentOutside parentBucket childBucket = do
  joint <-
    multiplyMany
      domainSizes
      ( parentOutside :
        [ factor
        | TaggedFactor origin factor <- spbFactors parentBucket,
          origin /= Just (spbIndex childBucket)
        ]
      )
  projectFactorToScopeSum domainSizes (spbMessageScope childBucket) joint

projectFactorToScopeSum
  :: VU.Vector Int
  -> VU.Vector Int
  -> WeightedFactor
  -> Either InferenceExecutionError WeightedFactor
projectFactorToScopeSum domainSizes targetScope factor
  | wfScope factor == targetScope = Right factor
  | VB.null (wfRows factor) = Right (impossibleFactor targetScope)
  | otherwise = do
      let !sourcePositions =
            IntMap.fromList (zip (VU.toList (wfScope factor)) [0 ..])
      targetPositions <-
        VU.fromList <$> traverse (lookupTargetPosition sourcePositions) (VU.toList targetScope)
      let !targetDims = dimsFor domainSizes targetScope
          !targetStrides = stridesFor targetDims
          !table =
            VB.foldl'
              ( \acc row ->
                  let !code =
                        assignmentKeyValue
                          ( encodeProjectionWithStrides
                              targetDims
                              targetStrides
                              targetPositions
                              (frAssign row)
                          )
                   in IntMap.insertWith logAddExp code (frLogWeight row) acc
              )
              IntMap.empty
              (wfRows factor)
      pure (factorFromTable targetScope targetDims table)
  where
    lookupTargetPosition :: IntMap Int -> Int -> Either InferenceExecutionError Int
    lookupTargetPosition positions variable =
      case IntMap.lookup variable positions of
        Just position -> Right position
        Nothing -> Left (InferenceJoinScopeInvariantViolation variable)

unaryLogWeights :: Int -> WeightedFactor -> [LogWeight]
unaryLogWeights domainSize factor =
  case factorConstantLog factor of
    Just constantLogWeight ->
      replicate domainSize constantLogWeight
    Nothing ->
      let !rowWeights =
            IntMap.fromList
              [ (frAssign row VU.! 0, frLogWeight row)
              | row <- VB.toList (wfRows factor)
              ]
       in [ IntMap.findWithDefault impossibleLogWeight value rowWeights
          | value <- [0 .. domainSize - 1]
          ]

extractTaggedBucket :: Int -> [TaggedFactor] -> ([TaggedFactor], [TaggedFactor])
extractTaggedBucket vi =
  foldl'
    ( \(!bucket, !rest) tagged@(TaggedFactor _origin factor) ->
        case findVarPos vi (wfScope factor) of
          Just _  -> (tagged : bucket, rest)
          Nothing -> (bucket, tagged : rest)
    )
    ([], [])

reconstructAssignment
  :: VU.Vector Int
  -> [DecisionFactor]
  -> Either InferenceExecutionError (IntMap Int)
reconstructAssignment domainSizes =
  foldM assignOne IntMap.empty
  where
    assignOne acc DecisionFactor{..} = do
      let scopeDims = dimsFor domainSizes dfScope
      scopeAssignment <-
        VU.generateM (VU.length dfScope) $ \k -> do
          let vi = dfScope VU.! k
          case IntMap.lookup vi acc of
            Just valueIx -> Right valueIx
            Nothing -> Left (InferenceMissingDownstreamAssignment dfVar vi)
      let code = assignmentKeyValue (encodeAssignment scopeDims scopeAssignment)
      valueIx <-
        case IntMap.lookup code dfChoice of
          Just chosen -> Right chosen
          Nothing -> Left (InferenceMissingArgmaxChoice dfVar code)
      pure (IntMap.insert dfVar valueIx acc)

eliminateOneVariable
  :: (VU.Vector Int -> Int -> WeightedFactor -> (WeightedFactor, aux))
  -> (aux -> [aux] -> [aux])
  -> VU.Vector Int
  -> (LogWeight, [WeightedFactor], [aux])
  -> Int
  -> Either InferenceExecutionError (LogWeight, [WeightedFactor], [aux])
eliminateOneVariable eliminator auxCons domainSizes (!constAcc, !factors, !auxes) !varIdx
  | isImpossibleLogWeight constAcc = Right (constAcc, factors, auxes)
  | otherwise =
      let (!bucket, !rest) = extractBucket varIdx factors
       in if null bucket
            then Right (constAcc, rest, auxes)
            else do
              joint <- multiplyMany domainSizes bucket
              let (!reduced, !aux) = eliminator domainSizes varIdx joint
                  !auxes' = auxCons aux auxes
              case factorConstantLog reduced of
                Just c  -> Right (addProductLogWeights constAcc c, rest, auxes')
                Nothing -> Right (constAcc, reduced : rest, auxes')

unitFactor :: WeightedFactor
unitFactor =
  WeightedFactor
    { wfScope = VU.empty
    , wfRows  = VB.singleton (FactorRow VU.empty zeroLogWeight)
    }

indicatorFactor :: Int -> Int -> WeightedFactor
indicatorFactor varIdx valueIx =
  WeightedFactor
    { wfScope = VU.singleton varIdx
    , wfRows  = VB.singleton (FactorRow (VU.singleton valueIx) zeroLogWeight)
    }

multiplyMany
  :: VU.Vector Int
  -> [WeightedFactor]
  -> Either InferenceExecutionError WeightedFactor
multiplyMany domainSizes =
  foldM (multiplyFactors domainSizes) unitFactor . sortOn (VU.length . wfScope)

multiplyFactors
  :: VU.Vector Int
  -> WeightedFactor
  -> WeightedFactor
  -> Either InferenceExecutionError WeightedFactor
multiplyFactors domainSizes fa fb
  | VB.null (wfRows fa) = Right (impossibleFactor (unionSorted (wfScope fa) (wfScope fb)))
  | VB.null (wfRows fb) = Right (impossibleFactor (unionSorted (wfScope fa) (wfScope fb)))
  | otherwise =
      case (factorConstantLog fa, factorConstantLog fb) of
        (Just ca, _) -> Right (scaleFactor ca fb)
        (_, Just cb) -> Right (scaleFactor cb fa)
        _ -> do
          plan <- makeJoinPlan domainSizes (wfScope fa) (wfScope fb)
          let !rowsA = wfRows fa
              !rowsB = wfRows fb
              (!smallRows, !smallPos, !bigRows, !bigPos, !smallIsA) =
                if VB.length rowsA <= VB.length rowsB
                  then (rowsA, jpOverlapPosA plan, rowsB, jpOverlapPosB plan, True)
                  else (rowsB, jpOverlapPosB plan, rowsA, jpOverlapPosA plan, False)
              !indexed = buildProjectionIndex (jpOverlapDims plan) (jpOverlapStrides plan) smallPos smallRows
              !table =
                VB.foldl'
                  (joinWithMatches plan indexed bigPos smallIsA)
                  IntMap.empty
                  bigRows
          pure (factorFromTable (jpUnionScope plan) (jpUnionDims plan) table)

joinWithMatches
  :: JoinPlan
  -> IntMap [FactorRow]
  -> VU.Vector Int
  -> Bool
  -> IntMap LogWeight
  -> FactorRow
  -> IntMap LogWeight
joinWithMatches plan indexed bigPos smallIsA !acc !bigRow =
  let !key =
        assignmentKeyValue
          (encodeProjectionWithStrides (jpOverlapDims plan) (jpOverlapStrides plan) bigPos (frAssign bigRow))
      !matches = IntMap.findWithDefault [] key indexed
   in foldl' combineOne acc matches
  where
    combineOne !acc0 !smallRow =
      let (!rowA, !rowB) =
            if smallIsA
              then (smallRow, bigRow)
              else (bigRow, smallRow)
          !code = assignmentKeyValue (encodeJoinedAssignment plan (frAssign rowA) (frAssign rowB))
          !weight = multiplyPossibleLogWeights (frLogWeight rowA) (frLogWeight rowB)
       in IntMap.insertWith logAddExp code weight acc0

sumOutVar
  :: VU.Vector Int
  -> Int
  -> WeightedFactor
  -> WeightedFactor
sumOutVar domainSizes varIdx factor =
  case findVarPos varIdx (wfScope factor) of
    Nothing -> factor
    Just pos ->
      let !newScope = deleteAt (wfScope factor) pos
          !newDims = dimsFor domainSizes newScope
          !table =
            VB.foldl'
              ( \acc FactorRow{..} ->
                  let !projected = deleteAt frAssign pos
                      !code = assignmentKeyValue (encodeAssignment newDims projected)
                   in IntMap.insertWith logAddExp code frLogWeight acc
              )
              IntMap.empty
              (wfRows factor)
       in factorFromTable newScope newDims table

maxOutVar
  :: VU.Vector Int
  -> Int
  -> WeightedFactor
  -> (WeightedFactor, DecisionFactor)
maxOutVar domainSizes varIdx factor =
  case findVarPos varIdx (wfScope factor) of
    Nothing ->
      ( factor
      , DecisionFactor
          { dfVar = varIdx
          , dfScope = wfScope factor
          , dfChoice = IntMap.empty
          }
      )
    Just pos ->
      let !newScope = deleteAt (wfScope factor) pos
          !newDims = dimsFor domainSizes newScope
          !best =
            VB.foldl'
              ( \acc FactorRow{..} ->
                  let !projected = deleteAt frAssign pos
                      !code = assignmentKeyValue (encodeAssignment newDims projected)
                      !valueIx = frAssign VU.! pos
                      !candidate = (frLogWeight, valueIx)
                   in IntMap.insertWith preferBest code candidate acc
              )
              IntMap.empty
              (wfRows factor)
          !table = fmap fst best
          !choices = fmap snd best
       in ( factorFromTable newScope newDims table
          , DecisionFactor
              { dfVar = varIdx
              , dfScope = newScope
              , dfChoice = choices
              }
          )
  where
    preferBest :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
    preferBest newer older
      | fst newer > fst older = newer
      | fst newer < fst older = older
      | snd newer < snd older = newer
      | otherwise             = older

scaleFactor :: LogWeight -> WeightedFactor -> WeightedFactor
scaleFactor c factor
  | isImpossibleLogWeight c = impossibleFactor (wfScope factor)
  | c == zeroLogWeight = factor
  | otherwise  =
      factor
        { wfRows =
            VB.map
              (\row -> row { frLogWeight = multiplyPossibleLogWeights (frLogWeight row) c })
              (wfRows factor)
        }

impossibleFactor :: VU.Vector Int -> WeightedFactor
impossibleFactor scope =
  WeightedFactor
    { wfScope = scope
    , wfRows  = VB.empty
    }

factorConstantLog :: WeightedFactor -> Maybe LogWeight
factorConstantLog WeightedFactor{..}
  | not (VU.null wfScope) = Nothing
  | VB.null wfRows        = Just impossibleLogWeight
  | otherwise             = Just (VB.foldl' (\acc row -> logAddExp acc (frLogWeight row)) impossibleLogWeight wfRows)

pullConstantFactors :: [WeightedFactor] -> (LogWeight, [WeightedFactor])
pullConstantFactors =
  foldl'
    step
    (zeroLogWeight, [])
  where
    step (!constAcc, !rest) factor =
      case factorConstantLog factor of
        Just c  -> (addProductLogWeights constAcc c, rest)
        Nothing -> (constAcc, factor : rest)

collapseConstantProduct :: VU.Vector Int -> [WeightedFactor] -> Either InferenceExecutionError LogWeight
collapseConstantProduct _ [] = Right zeroLogWeight
collapseConstantProduct domainSizes factors = do
  productFactor <- multiplyMany domainSizes factors
  case factorConstantLog productFactor of
    Just c  -> Right c
    Nothing -> Left (InferenceResidualFactorAfterElimination (wfScope productFactor))

factorFromTable
  :: VU.Vector Int
  -> VU.Vector Int
  -> IntMap LogWeight
  -> WeightedFactor
factorFromTable scope dims table =
  let !strides = stridesFor dims
   in WeightedFactor
        { wfScope = scope
        , wfRows  =
            VB.fromList
              [ FactorRow
                  { frAssign = decodeAssignmentWithStrides dims strides (AssignmentKey code)
                  , frLogWeight = weight
                  }
              | (code, weight) <- IntMap.toAscList table
              ]
        }

makeJoinPlan
  :: VU.Vector Int
  -> VU.Vector Int
  -> VU.Vector Int
  -> Either InferenceExecutionError JoinPlan
makeJoinPlan domainSizes scopeA scopeB = do
  let !unionScope = unionSorted scopeA scopeB
      !unionDims = dimsFor domainSizes unionScope
      !unionStrides = stridesFor unionDims
      !scopeAList = VU.toList scopeA
      !scopeBList = VU.toList scopeB
      !posA = IntMap.fromList (zip scopeAList [0 ..])
      !posB = IntMap.fromList (zip scopeBList [0 ..])
      !setA = IntSet.fromList scopeAList
      !setB = IntSet.fromList scopeBList
      !overlapVars = VU.fromList [ v | v <- scopeAList, IntSet.member v setB ]
  overlapPosA <-
    VU.fromList <$> traverse (lookupPos posA) (VU.toList overlapVars)
  overlapPosB <-
    VU.fromList <$> traverse (lookupPos posB) (VU.toList overlapVars)
  sourcePos <-
    VU.fromList
      <$> traverse
        ( \v ->
            if IntSet.member v setA
              then lookupPos posA v
              else lookupPos posB v
        )
        (VU.toList unionScope)
  let !overlapDims = dimsFor domainSizes overlapVars
      !overlapStrides = stridesFor overlapDims
      !fromA = VU.fromList [ IntSet.member v setA | v <- VU.toList unionScope ]
  pure
    JoinPlan
      { jpUnionScope  = unionScope
      , jpUnionDims   = unionDims
      , jpUnionStrides = unionStrides
      , jpOverlapPosA = overlapPosA
      , jpOverlapPosB = overlapPosB
      , jpOverlapDims = overlapDims
      , jpOverlapStrides = overlapStrides
      , jpFromA       = fromA
      , jpSourcePos   = sourcePos
      }
  where
    lookupPos :: IntMap Int -> Int -> Either InferenceExecutionError Int
    lookupPos posMap vi =
      case IntMap.lookup vi posMap of
        Just position -> Right position
        Nothing -> Left (InferenceJoinScopeInvariantViolation vi)

buildProjectionIndex
  :: VU.Vector Int
  -> VU.Vector Int
  -> VU.Vector Int
  -> VB.Vector FactorRow
  -> IntMap [FactorRow]
buildProjectionIndex overlapDims overlapStrides overlapPos =
  VB.foldl'
    ( \acc row ->
        let !key =
              assignmentKeyValue
                (encodeProjectionWithStrides overlapDims overlapStrides overlapPos (frAssign row))
         in IntMap.insertWith (++) key [row] acc
    )
    IntMap.empty

chooseEliminationOrder
  :: EliminationHeuristic
  -> Int
  -> [WeightedFactor]
  -> [Int]
chooseEliminationOrder heuristic varCount factors =
  let !graph0 = primalGraph varCount factors
      !remaining0 = IntSet.fromList [0 .. varCount - 1]
      !scores0 = candidateScores heuristic graph0 remaining0
   in go graph0 remaining0 scores0 []
  where
    go !_graph remaining !_scores !acc | IntSet.null remaining = reverse acc
    go !graph remaining !scores !acc =
      let !chosen = fst (minimumBy (comparing snd) (IntMap.toList scores))
          !neighbors = liveNeighbors graph remaining chosen
          !filledEdges = missingNeighborPairs graph neighbors
          !graph' = eliminateFromGraph graph remaining chosen
          !remaining' = IntSet.delete chosen remaining
          !affected = affectedCandidates graph remaining' neighbors filledEdges
          !scores' =
            refreshCandidateScores
              heuristic
              graph'
              remaining'
              affected
              (IntMap.delete chosen scores)
       in go graph' remaining' scores' (chosen : acc)

candidateScores ::
  EliminationHeuristic ->
  IntMap IntSet ->
  IntSet ->
  IntMap (Int, Int, Int)
candidateScores heuristic graph remaining =
  IntMap.fromList
    [ (candidate, scoreCandidate heuristic graph remaining candidate)
    | candidate <- IntSet.toList remaining
    ]

scoreCandidate ::
  EliminationHeuristic ->
  IntMap IntSet ->
  IntSet ->
  Int ->
  (Int, Int, Int)
scoreCandidate heuristic graph remaining candidate =
  let !neighbors = liveNeighbors graph remaining candidate
      !degree = IntSet.size neighbors
      !fill = fillEdgeCount graph neighbors
   in case heuristic of
        MinFill   -> (fill, degree, candidate)
        MinDegree -> (degree, fill, candidate)

refreshCandidateScores ::
  EliminationHeuristic ->
  IntMap IntSet ->
  IntSet ->
  IntSet ->
  IntMap (Int, Int, Int) ->
  IntMap (Int, Int, Int)
refreshCandidateScores heuristic graph remaining affected scores =
  foldl'
    ( \acc candidate ->
        if IntSet.member candidate remaining
          then IntMap.insert candidate (scoreCandidate heuristic graph remaining candidate) acc
          else acc
    )
    scores
    (IntSet.toList affected)

affectedCandidates ::
  IntMap IntSet ->
  IntSet ->
  IntSet ->
  [(Int, Int)] ->
  IntSet
affectedCandidates graph remaining neighbors filledEdges =
  IntSet.intersection
    remaining
    ( IntSet.unions
        ( neighbors :
          [ IntSet.intersection
              (IntMap.findWithDefault IntSet.empty left graph)
              (IntMap.findWithDefault IntSet.empty right graph)
          | (left, right) <- filledEdges
          ]
        )
    )

primalGraph :: Int -> [WeightedFactor] -> IntMap IntSet
primalGraph varCount = foldl' addFactorEdges initial
  where
    initial = IntMap.fromList [ (i, IntSet.empty) | i <- [0 .. varCount - 1] ]

    addFactorEdges !graph WeightedFactor{..} =
      addCliqueToGraph (VU.toList wfScope) graph

liveNeighbors :: IntMap IntSet -> IntSet -> Int -> IntSet
liveNeighbors graph remaining vi =
  IntSet.delete vi $
    IntSet.intersection remaining (IntMap.findWithDefault IntSet.empty vi graph)

fillEdgeCount :: IntMap IntSet -> IntSet -> Int
fillEdgeCount graph neighbors =
  length (missingNeighborPairs graph neighbors)

missingNeighborPairs :: IntMap IntSet -> IntSet -> [(Int, Int)]
missingNeighborPairs graph neighbors =
  let ns = IntSet.toList neighbors
   in [ (a, b) | a : rest <- tails ns, b <- rest, not (adjacent graph a b) ]

adjacent :: IntMap IntSet -> Int -> Int -> Bool
adjacent graph a b =
  IntSet.member b (IntMap.findWithDefault IntSet.empty a graph)

eliminateFromGraph :: IntMap IntSet -> IntSet -> Int -> IntMap IntSet
eliminateFromGraph graph remaining vi =
  let !nbrs = IntSet.toList (liveNeighbors graph remaining vi)
      !graphClique = addCliqueToGraph nbrs graph
      !graphNoVar = IntMap.delete vi graphClique
   in foldl' (\graph' n -> IntMap.adjust (IntSet.delete vi) n graph') graphNoVar nbrs

addCliqueToGraph :: [Int] -> IntMap IntSet -> IntMap IntSet
addCliqueToGraph [] graph = graph
addCliqueToGraph (x:xs) graph =
  let !graph' = foldl' (connectVertices x) graph xs
   in addCliqueToGraph xs graph'

connectVertices :: Int -> IntMap IntSet -> Int -> IntMap IntSet
connectVertices a graph b =
  let !graph1 = IntMap.adjust (IntSet.insert b) a graph
   in IntMap.adjust (IntSet.insert a) b graph1

extractBucket :: Int -> [WeightedFactor] -> ([WeightedFactor], [WeightedFactor])
extractBucket vi =
  foldl'
    ( \(!bucket, !rest) factor ->
        case findVarPos vi (wfScope factor) of
          Just _  -> (factor : bucket, rest)
          Nothing -> (bucket, factor : rest)
    )
    ([], [])

findVarPos :: Int -> VU.Vector Int -> Maybe Int
findVarPos vi scope = go 0
  where
    !n = VU.length scope
    go !i
      | i >= n = Nothing
      | otherwise =
          let !v = scope VU.! i
           in if v == vi
                then Just i
                else if v > vi then Nothing else go (i + 1)

dimsFor :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
dimsFor domainSizes = VU.map (domainSizes VU.!)

encodeAssignment :: VU.Vector Int -> VU.Vector Int -> AssignmentKey
encodeAssignment dims assignment =
  encodeAssignmentFrom dims (VU.length assignment) (assignment VU.!)

decodeAssignment :: VU.Vector Int -> AssignmentKey -> VU.Vector Int
decodeAssignment dims code0 =
  decodeAssignmentWithStrides dims (stridesFor dims) code0

decodeAssignmentWithStrides :: VU.Vector Int -> VU.Vector Int -> AssignmentKey -> VU.Vector Int
decodeAssignmentWithStrides dims strides (AssignmentKey code0) =
  VU.generate (VU.length dims) $ \i ->
    let !stride = strides VU.! i
        !d = max 1 (dims VU.! i)
     in (code0 `quot` stride) `rem` d

stridesFor :: VU.Vector Int -> VU.Vector Int
stridesFor =
  VU.prescanl' (\acc d -> acc * max 1 d) 1

encodeProjection
  :: VU.Vector Int
  -> VU.Vector Int
  -> VU.Vector Int
  -> AssignmentKey
encodeProjection dims =
  encodeProjectionWithStrides dims (stridesFor dims)

encodeProjectionWithStrides
  :: VU.Vector Int
  -> VU.Vector Int
  -> VU.Vector Int
  -> VU.Vector Int
  -> AssignmentKey
encodeProjectionWithStrides dims strides positions assignment
  | VU.null positions = AssignmentKey 0
  | otherwise =
      encodeAssignmentFromWithStrides
        dims
        strides
        (VU.length positions)
        (\i -> assignment VU.! (positions VU.! i))

encodeJoinedAssignment
  :: JoinPlan
  -> VU.Vector Int
  -> VU.Vector Int
  -> AssignmentKey
encodeJoinedAssignment JoinPlan{..} assignA assignB =
  encodeAssignmentFromWithStrides
    jpUnionDims
    jpUnionStrides
    (VU.length jpUnionScope)
    ( \u ->
        let !src = jpSourcePos VU.! u
         in if jpFromA VU.! u then assignA VU.! src else assignB VU.! src
    )

encodeAssignmentFrom :: VU.Vector Int -> Int -> (Int -> Int) -> AssignmentKey
encodeAssignmentFrom dims count f =
  encodeAssignmentFromWithStrides dims (stridesFor dims) count f

encodeAssignmentFromWithStrides :: VU.Vector Int -> VU.Vector Int -> Int -> (Int -> Int) -> AssignmentKey
encodeAssignmentFromWithStrides dims strides count f =
  let !n = min (VU.length dims) count
      go !acc !i
        | i >= n = acc
        | otherwise =
            let !x = f i
                !stride = strides VU.! i
             in go (acc + x * stride) (i + 1)
   in AssignmentKey (go 0 0)

unionSorted :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
unionSorted xs ys =
  VU.fromList (go (VU.toList xs) (VU.toList ys))
  where
    go :: [Int] -> [Int] -> [Int]
    go [] bs = bs
    go as [] = as
    go aa@(a:as) bb@(b:bs)
      | a < b     = a : go as bb
      | a > b     = b : go aa bs
      | otherwise = a : go as bs

deleteAt :: VU.Vector Int -> Int -> VU.Vector Int
deleteAt vec pos =
  VU.ifilter (\i _ -> i /= pos) vec

normalizeLogMasses :: [LogWeight] -> Either InferenceExecutionError [Double]
normalizeLogMasses [] = Right []
normalizeLogMasses [LogWeight leftValue, LogWeight rightValue]
  | leftValue == negativeInfinity && rightValue == negativeInfinity = Left InferenceImpossiblePosterior
  | leftValue == positiveInfinity = Right (if rightValue == positiveInfinity then [0.5, 0.5] else [1.0, 0.0])
  | rightValue == positiveInfinity = Right [0.0, 1.0]
  | leftValue >= rightValue =
      let !ratio = exp (rightValue - leftValue)
          !inverseNormalizer = recip (1.0 + ratio)
       in Right [inverseNormalizer, ratio * inverseNormalizer]
  | otherwise =
      let !ratio = exp (leftValue - rightValue)
          !inverseNormalizer = recip (1.0 + ratio)
       in Right [ratio * inverseNormalizer, inverseNormalizer]
normalizeLogMasses xs
  | lmsMaximum summary == negativeInfinity = Left InferenceImpossiblePosterior
  | lmsCertainCount summary > 0 =
      Right
        [ if isCertainLogWeight weight then certainProbability else 0.0
        | weight <- xs
        ]
  | otherwise =
      let !m = lmsMaximum summary
          !exps = [ exp (logWeightValue weight - m) | weight <- xs ]
          !z = sum exps
       in Right [ mass / z | mass <- exps ]
  where
    summary =
      foldl' summarizeLogMass LogMassSummary
        { lmsCertainCount = 0,
          lmsMaximum = negativeInfinity
        }
        xs
    certainProbability = recip (fromIntegral (lmsCertainCount summary))

data LogMassSummary = LogMassSummary
  { lmsCertainCount :: !Int,
    lmsMaximum :: !Double
  }

summarizeLogMass :: LogMassSummary -> LogWeight -> LogMassSummary
summarizeLogMass summary weight
  | isImpossibleLogWeight weight = summary
  | otherwise =
      LogMassSummary
        { lmsCertainCount =
            lmsCertainCount summary
              + if isCertainLogWeight weight then 1 else 0,
          lmsMaximum = max (lmsMaximum summary) (logWeightValue weight)
        }

mkLogWeight :: Double -> Either LogWeightError LogWeight
mkLogWeight value
  | isNaN value = Left LogWeightNaN
  | otherwise = Right (LogWeight value)

logWeightValue :: LogWeight -> Double
logWeightValue =
  unLogWeight
{-# INLINE logWeightValue #-}

zeroLogWeight :: LogWeight
zeroLogWeight =
  LogWeight 0.0

impossibleLogWeight :: LogWeight
impossibleLogWeight =
  LogWeight negativeInfinity

negativeInfinity :: Double
negativeInfinity =
  -(1.0 / 0.0)

positiveInfinity :: Double
positiveInfinity =
  1.0 / 0.0

isImpossibleLogWeight :: LogWeight -> Bool
isImpossibleLogWeight (LogWeight value) =
  value == negativeInfinity
{-# INLINE isImpossibleLogWeight #-}

isCertainLogWeight :: LogWeight -> Bool
isCertainLogWeight (LogWeight value) =
  value == positiveInfinity
{-# INLINE isCertainLogWeight #-}

logAddExp :: LogWeight -> LogWeight -> LogWeight
logAddExp leftWeight@(LogWeight leftValue) rightWeight@(LogWeight rightValue)
  | leftValue == negativeInfinity = rightWeight
  | rightValue == negativeInfinity = leftWeight
  | leftValue == positiveInfinity && rightValue == positiveInfinity = leftWeight
  | leftValue < rightValue =
      LogWeight (rightValue + log (1.0 + exp (leftValue - rightValue)))
  | otherwise =
      LogWeight (leftValue + log (1.0 + exp (rightValue - leftValue)))
{-# INLINE logAddExp #-}

-- Hard impossibility dominates multiplication. This gives a total policy for
-- the otherwise indeterminate @0 * infinity@ case and preserves hard factor
-- constraints.
addProductLogWeights :: LogWeight -> LogWeight -> LogWeight
addProductLogWeights (LogWeight leftValue) (LogWeight rightValue)
  | leftValue == negativeInfinity = impossibleLogWeight
  | rightValue == negativeInfinity = impossibleLogWeight
  | otherwise =
      LogWeight (leftValue + rightValue)
{-# INLINE addProductLogWeights #-}

-- Compiled factor rows never retain negative-infinite mass. Their hot product
-- therefore needs no repeated impossibility branch; hard-zero rows were
-- eliminated once at compilation.
multiplyPossibleLogWeights :: LogWeight -> LogWeight -> LogWeight
multiplyPossibleLogWeights (LogWeight leftValue) (LogWeight rightValue) =
  LogWeight (leftValue + rightValue)
{-# INLINE multiplyPossibleLogWeights #-}

possibleLogWeight :: InferenceExecutionError -> LogWeight -> Either InferenceExecutionError Double
possibleLogWeight impossibleError weight
  | isImpossibleLogWeight weight = Left impossibleError
  | otherwise = Right (logWeightValue weight)

checkAssignmentCardinality :: VU.Vector Int -> Either Integer ()
checkAssignmentCardinality dimensions
  | cardinality > toInteger (maxBound :: Int) = Left cardinality
  | otherwise = Right ()
  where
    cardinality =
      VU.foldl'
        (\productValue dimension -> productValue * toInteger (max 1 dimension))
        1
        dimensions
