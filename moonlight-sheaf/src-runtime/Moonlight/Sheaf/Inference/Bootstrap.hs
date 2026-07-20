{-# LANGUAGE RecordWildCards #-}

module Moonlight.Sheaf.Inference.Bootstrap where

import Control.Monad (foldM, foldM_)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU
import Moonlight.Core (indexMap)
import Moonlight.Sheaf.Inference.Algebra
import Moonlight.Sheaf.Inference.Types

buildDomainIndex
  :: (Ord pid, Ord obj)
  => Map pid (Set obj)
  -> DomainIndex pid obj
buildDomainIndex domains0 =
  let vars = Map.keys domains0
      varsV = VB.fromList vars
      varIx = indexMap vars
      domainsV =
        VB.fromList
          [ VB.fromList (Set.toAscList dom)
          | dom <- Map.elems domains0
          ]
      objIxV =
        VB.map
          (indexMap . VB.toList)
          domainsV
      domainSizes = VU.fromList [ VB.length dom | dom <- VB.toList domainsV ]
   in DomainIndex
        { diVars        = varsV
        , diVarIx       = varIx
        , diDomains     = domainsV
        , diObjIx       = objIxV
        , diDomainSizes = domainSizes
        }

compileUnaryFactors
  :: DomainIndex pid obj
  -> (pid -> obj -> Double)
  -> Either (FactorCompileError pid obj) (VB.Vector WeightedFactor)
compileUnaryFactors index localLogScore =
  VB.imapM compileOne (diDomains index)
  where
    compileOne !varIx domainV =
      let pid = diVars index VB.! varIx
       in do
            rows <- VB.imapM (compileRow pid) domainV
            pure
              WeightedFactor
                { wfScope = VU.singleton varIx,
                  wfRows = VB.filter (not . isImpossibleLogWeight . frLogWeight) rows
                }

    compileRow pid valueIx obj = do
      logWeight <-
        first (FactorLocalInvalidLogWeight pid obj) (mkLogWeight (localLogScore pid obj))
      pure
        FactorRow
          { frAssign = VU.singleton valueIx,
            frLogWeight = logWeight
          }

compileFactorSpec
  :: (Ord pid, Ord obj)
  => DomainIndex pid obj
  -> FactorSpec pid obj
  -> Either (FactorCompileError pid obj) WeightedFactor
compileFactorSpec index FactorSpec{..} = do
  let scopePids = Set.toAscList fsScope
  scopeIx <-
    VU.fromList <$> traverse lookupVarIx scopePids
  let scopeDims = dimsFor (diDomainSizes index) scopeIx
  _ <- first FactorAssignmentSpaceOverflow (checkAssignmentCardinality scopeDims)
  let insertTuple table (tupleIndex, (tupleMap, tupleLogWeight)) = do
        assignment <-
          VU.generateM (length scopePids) $ \k -> do
            let varIdx = scopeIx VU.! k
                pid = diVars index VB.! varIdx
                objIxMap = diObjIx index VB.! varIdx
            obj <-
              case Map.lookup pid tupleMap of
                Just value -> Right value
                Nothing -> Left (FactorTupleMissingVariable pid)
            case Map.lookup obj objIxMap of
              Just valueIndex -> Right valueIndex
              Nothing -> Left (FactorTupleObjectOutOfDomain pid obj)
        weight <-
          first (FactorTupleInvalidLogWeight tupleIndex) (mkLogWeight tupleLogWeight)
        let code = assignmentKeyValue (encodeAssignment scopeDims assignment)
        pure
          ( if isImpossibleLogWeight weight
              then table
              else IntMap.insertWith logAddExp code weight table
          )
  table <-
    foldM insertTuple IntMap.empty (zip [0 :: Int ..] fsTuples)
  pure (factorFromTable scopeIx scopeDims table)
  where
    lookupVarIx pid =
      case Map.lookup pid (diVarIx index) of
        Just ix -> Right ix
        Nothing -> Left (FactorScopeUnknownVariable pid)

mkBlueprint
  :: DomainIndex pid obj
  -> [WeightedFactor]
  -> Either (BlueprintError pid obj) (WeightedBlueprint pid obj)
mkBlueprint index factors = do
  validateDomains index
  _ <-
    first BlueprintAssignmentSpaceOverflow
      (checkAssignmentCardinality (diDomainSizes index))
  traverse_
    (uncurry (validateFactor index))
    (zip [0 :: Int ..] factors)
  pure
    WeightedBlueprint
      { wbIndex = index
      , wbFactors = VB.fromList (completeFactorCoverage index factors)
      }

buildWeightedBlueprint
  :: (Ord pid, Ord obj)
  => Map pid (Set obj)
  -> (pid -> obj -> Double)
  -> [FactorSpec pid obj]
  -> Either (BlueprintError pid obj) (WeightedBlueprint pid obj)
buildWeightedBlueprint domains0 localLogScore factorSpecs =
  let index = buildDomainIndex domains0
   in do
        unary <-
          fmap VB.toList
            (first BlueprintFactorCompileError (compileUnaryFactors index localLogScore))
        compat <-
          traverse
            (first BlueprintFactorCompileError . compileFactorSpec index)
            factorSpecs
        mkBlueprint index (unary <> compat)

completeFactorCoverage :: DomainIndex pid obj -> [WeightedFactor] -> [WeightedFactor]
completeFactorCoverage index factors =
  factors <> fmap unitVariableFactor uncoveredVariables
  where
    coveredVariables =
      foldMap (IntSet.fromList . VU.toList . wfScope) factors
    uncoveredVariables =
      filter
        (`IntSet.notMember` coveredVariables)
        [0 .. VB.length (diVars index) - 1]
    unitVariableFactor variableIndex =
      WeightedFactor
        { wfScope = VU.singleton variableIndex,
          wfRows =
            VB.generate
              (diDomainSizes index VU.! variableIndex)
              (\valueIndex -> FactorRow (VU.singleton valueIndex) zeroLogWeight)
        }

validateDomains ::
  DomainIndex pid obj ->
  Either (BlueprintError pid obj) ()
validateDomains index =
  traverse_
    validateDomain
    (zip (VB.toList (diVars index)) (VB.toList (diDomains index)))
  where
    validateDomain ::
      (pid, VB.Vector obj) ->
      Either (BlueprintError pid obj) ()
    validateDomain (pid, domainValues)
      | VB.null domainValues = Left (BlueprintEmptyDomain pid)
      | otherwise = Right ()

validateFactor ::
  DomainIndex pid obj ->
  Int ->
  WeightedFactor ->
  Either (BlueprintError pid obj) ()
validateFactor index factorIndex WeightedFactor{..} = do
  validateScope factorIndex (VB.length (diVars index)) wfScope
  let scopeDims = dimsFor (diDomainSizes index) wfScope
  validateRows factorIndex scopeDims wfRows

validateScope ::
  Int ->
  Int ->
  VU.Vector Int ->
  Either (BlueprintError pid obj) ()
validateScope factorIndex variableCount scope =
  if strictlyAscendingVector scope
    then traverse_ validateScopeIndex (VU.toList scope)
    else Left (BlueprintFactorScopeNotStrictlyAscending factorIndex scope)
  where
    validateScopeIndex variableIndex
      | variableIndex < 0 =
          Left (BlueprintFactorScopeIndexOutOfRange factorIndex variableIndex variableCount)
      | variableIndex >= variableCount =
          Left (BlueprintFactorScopeIndexOutOfRange factorIndex variableIndex variableCount)
      | otherwise =
          Right ()

validateRows ::
  Int ->
  VU.Vector Int ->
  VB.Vector FactorRow ->
  Either (BlueprintError pid obj) ()
validateRows factorIndex scopeDims rows =
  foldM_
    validateRow
    IntSet.empty
    (zip [0 :: Int ..] (VB.toList rows))
  where
    expectedArity =
      VU.length scopeDims

    validateRow seen (_rowIndex, FactorRow{..}) =
      if VU.length frAssign /= expectedArity
        then Left (BlueprintFactorRowArityMismatch factorIndex expectedArity (VU.length frAssign))
        else do
          traverse_
            (uncurry validateValue)
            (zip [0 :: Int ..] (VU.toList frAssign))
          let code = assignmentKeyValue (encodeAssignment scopeDims frAssign)
          if IntSet.member code seen
            then Left (BlueprintFactorDuplicateAssignment factorIndex code)
            else Right (IntSet.insert code seen)

    validateValue localIndex valueIndex =
      let domainSize = scopeDims VU.! localIndex
       in if valueIndex < 0 || valueIndex >= domainSize
            then Left (BlueprintFactorRowValueOutOfBounds factorIndex localIndex valueIndex domainSize)
            else Right ()

strictlyAscendingVector :: VU.Vector Int -> Bool
strictlyAscendingVector vectorValue =
  VU.and (VU.zipWith (<) vectorValue (VU.drop 1 vectorValue))
