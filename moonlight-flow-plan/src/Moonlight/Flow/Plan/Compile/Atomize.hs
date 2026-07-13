{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Plan.Compile.Atomize
  ( PatternAtomizeHost (..),
    PatternAtomizeObstruction (..),
    compiledPatternQueryDigestWith,
    compiledPatternQueryFingerprintWith,
    atomizePatternQueryWith,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core
  ( dedupStableOn,
  )
import Moonlight.Flow.Internal.Digest
  ( fingerprintWord64ToInt,
    maybeWord64DigestWords,
    mix64,
    wordOfInt,
  )
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Plan.Residual
  ( ResidualShape (ResidualDigestOnly),
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Plan.Query.Core qualified as RelPlan

type PatternAtomizeHost :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data PatternAtomizeHost query pattern var guard tag tuple rep output = PatternAtomizeHost
  { pahQueryPatterns :: !(query -> NonEmpty pattern),
    pahQueryResidualGuard :: !(query -> Maybe guard),
    pahResidualWords :: !(guard -> [Word64]),
    pahPatternVar :: !(pattern -> Maybe var),
    pahPatternNode :: !(pattern -> Maybe (tag, [pattern])),
    pahPatternVarKey :: !(var -> Int),
    pahTagDigest :: !(tag -> Word64)
  }

type PatternAtomizeObstruction :: Type
data PatternAtomizeObstruction
  = PatternAtomizeMalformedPattern {-# UNPACK #-} !Int
  | PatternAtomizeMissingNodeSlot {-# UNPACK #-} !Int
  | PatternAtomizeMissingVariableSlot {-# UNPACK #-} !Int
  | PatternAtomizeEmptyQuery
  | PatternAtomizeInvalidPlan ![PlanBuild.QueryPlanError]
  deriving stock (Eq, Ord, Show)

type AnnPattern :: Type -> Type -> Type
data AnnPattern var tag
  = APVar !var
  | APNode !Int !tag ![AnnPattern var tag]

annotatePatterns ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  [pattern] ->
  Either PatternAtomizeObstruction [AnnPattern var tag]
annotatePatterns host patterns =
  snd <$> listMapAccumLEither (annotatePattern host) 0 patterns

annotatePattern ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Int ->
  pattern ->
  Either PatternAtomizeObstruction (Int, AnnPattern var tag)
annotatePattern host counter patternValue =
  case pahPatternVar host patternValue of
    Just patternVar ->
      Right (counter, APVar patternVar)
    Nothing ->
      case pahPatternNode host patternValue of
        Just (tagValue, children) -> do
          let nodeId = counter
          (counter', annotatedChildren) <-
            listMapAccumLEither
              (annotatePattern host)
              (counter + 1)
              children
          Right (counter', APNode nodeId tagValue annotatedChildren)
        Nothing ->
          Left (PatternAtomizeMalformedPattern counter)

listMapAccumLEither ::
  (acc -> a -> Either err (acc, b)) ->
  acc ->
  [a] ->
  Either err (acc, [b])
listMapAccumLEither _ acc [] =
  Right (acc, [])
listMapAccumLEither f acc (x : xs) = do
  (acc', y) <- f acc x
  (acc'', ys) <- listMapAccumLEither f acc' xs
  Right (acc'', y : ys)

collectVarsOrdered :: Ord var => [AnnPattern var tag] -> [var]
collectVarsOrdered =
  dedupStableOn id . foldMap collectVars

collectVars :: AnnPattern var tag -> [var]
collectVars = \case
  APVar patternVar -> [patternVar]
  APNode _ _ children -> foldMap collectVars children

nodeCountOf :: [AnnPattern var tag] -> Int
nodeCountOf =
  foldl' max 0 . foldMap go
  where
    go :: AnnPattern var tag -> [Int]
    go = \case
      APVar _ -> []
      APNode nodeId _ children -> (nodeId + 1) : foldMap go children

rootSlotOf ::
  Ord var =>
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Map Int SlotId ->
  Map var SlotId ->
  AnnPattern var tag ->
  Either PatternAtomizeObstruction SlotId
rootSlotOf host nodeSlots varSlots = \case
  APVar patternVar ->
    lookupVarSlot host varSlots patternVar
  APNode nodeId _ _ ->
    lookupNodeSlot nodeSlots nodeId

lookupVarSlot ::
  Ord var =>
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Map var SlotId ->
  var ->
  Either PatternAtomizeObstruction SlotId
lookupVarSlot host varSlots patternVar =
  maybe
    (Left (PatternAtomizeMissingVariableSlot (pahPatternVarKey host patternVar)))
    Right
    (Map.lookup patternVar varSlots)

lookupNodeSlot ::
  Map Int SlotId ->
  Int ->
  Either PatternAtomizeObstruction SlotId
lookupNodeSlot nodeSlots nodeId =
  maybe
    (Left (PatternAtomizeMissingNodeSlot nodeId))
    Right
    (Map.lookup nodeId nodeSlots)

buildAtomSpec ::
  (tag -> Word64) ->
  AtomId ->
  tag ->
  SlotId ->
  [SlotId] ->
  RelPlan.AtomSpec tag tuple rep
buildAtomSpec tagDigest atomIdValue patternTag resultSlot childSlots =
  let slotSources =
        Map.fromListWith
          (<>)
          ((resultSlot, [RelPlan.SourceResult]) : [(slotIdValue, [RelPlan.SourceChild childIndex]) | (childIndex, slotIdValue) <- zip ([0 :: Int ..]) childSlots])
      columns =
        Vector.fromList (RelPlan.orderedSlotNub (resultSlot : childSlots))
      recipe =
        RelPlan.mkStalkRecipe
          ( Vector.fromList
              [ Map.findWithDefault [] slotIdValue slotSources
                | slotIdValue <- Vector.toList columns
              ]
          )
   in RelPlan.mkAtomSpec
        (RelPlan.mkQueryAtomId (RelPlan.atomIdKey atomIdValue))
        (RelPlan.mkSourceAtomId atomIdValue)
        patternTag
        (tagDigest patternTag)
        columns
        recipe

compileAtoms ::
  Ord var =>
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Map Int SlotId ->
  Map var SlotId ->
  AnnPattern var tag ->
  Either PatternAtomizeObstruction [(Int, RelPlan.AtomSpec tag tuple rep)]
compileAtoms host nodeSlots varSlots = \case
  APVar _ ->
    Right []
  APNode nodeId tagValue children -> do
    resultSlot <- lookupNodeSlot nodeSlots nodeId
    childSlots <- traverse (slotOf host nodeSlots varSlots) children
    let spec =
          buildAtomSpec
            (pahTagDigest host)
            (mkAtomId nodeId)
            tagValue
            resultSlot
            childSlots
    childAtoms <- traverse (compileAtoms host nodeSlots varSlots) children
    Right ((nodeId, spec) : concat childAtoms)

slotOf ::
  Ord var =>
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Map Int SlotId ->
  Map var SlotId ->
  AnnPattern var tag ->
  Either PatternAtomizeObstruction SlotId
slotOf host nodeSlots varSlots = \case
  APVar patternVar ->
    lookupVarSlot host varSlots patternVar
  APNode nodeId _ _ ->
    lookupNodeSlot nodeSlots nodeId

annPatternDigest ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  AnnPattern var tag ->
  Word64
annPatternDigest host = \case
  APVar patternVar ->
    digestWords
      [ 0x01,
        wordOfInt (pahPatternVarKey host patternVar)
      ]
  APNode nodeId tagValue children ->
    digestWords
      ( [ 0x02,
          wordOfInt nodeId,
          pahTagDigest host tagValue,
          wordOfInt (length children)
        ]
          <> fmap (annPatternDigest host) children
      )

compiledPatternQueryDigestWith ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  query ->
  Either PatternAtomizeObstruction Word64
compiledPatternQueryDigestWith host queryValue = do
  let patterns = NE.toList (pahQueryPatterns host queryValue)
  annotatedPatterns <- annotatePatterns host patterns
  let residualDigest =
        fmap
          (digestWords . pahResidualWords host)
          (pahQueryResidualGuard host queryValue)
  Right (compiledPatternQueryDigestFromAnnotated host residualDigest annotatedPatterns)

compiledPatternQueryDigestFromAnnotated ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  Maybe Word64 ->
  [AnnPattern var tag] ->
  Word64
compiledPatternQueryDigestFromAnnotated host residualDigest annotatedPatterns =
  digestWords
    ( [ 0x100,
        wordOfInt (length annotatedPatterns)
      ]
        <> fmap (annPatternDigest host) annotatedPatterns
        <> maybeWord64DigestWords residualDigest
    )

compiledPatternQueryFingerprintWith ::
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  query ->
  Either PatternAtomizeObstruction Int
compiledPatternQueryFingerprintWith host queryValue =
  fingerprintWord64ToInt <$> compiledPatternQueryDigestWith host queryValue

atomizePatternQueryWith ::
  ( Ord var,
    RelPlan.QueryOutput output rep,
    RelPlan.OutputVar output rep ~ var
  ) =>
  PatternAtomizeHost query pattern var guard tag tuple rep output ->
  query ->
  Either PatternAtomizeObstruction (RelPlan.QueryPlan query output guard tag tuple rep)
atomizePatternQueryWith host queryValue = do
  let patterns0 = NE.toList (pahQueryPatterns host queryValue)
  annotatedPatterns <- annotatePatterns host patterns0
  let varsOrdered = collectVarsOrdered annotatedPatterns
      nodeCount = nodeCountOf annotatedPatterns
      nodeSlots =
        Map.fromList [(nodeId, mkSlotId nodeId) | nodeId <- [0 :: Int .. nodeCount - 1]]
      varSlots =
        Map.fromList
          [(patternVar, mkSlotId (nodeCount + varIndex)) | (varIndex, patternVar) <- zip ([0 :: Int ..]) varsOrdered]

  atomSpecsList <-
    fmap concat $
      traverse
        (compileAtoms host nodeSlots varSlots)
        annotatedPatterns

  varOutputSlots <-
    traverse
      (lookupVarSlot host varSlots)
      varsOrdered

  primaryRootSlot <-
    case annotatedPatterns of
      annotatedPattern : _ ->
        rootSlotOf host nodeSlots varSlots annotatedPattern
      [] ->
        Left PatternAtomizeEmptyQuery

  let residualGuard = pahQueryResidualGuard host queryValue
      residualDigest = fmap (digestWords . pahResidualWords host) residualGuard
      queryDigest =
        compiledPatternQueryDigestFromAnnotated
          host
          residualDigest
          annotatedPatterns
      atomSpecsVec = Vector.fromList (fmap snd atomSpecsList)
      isRootDomainQuery =
        Vector.null atomSpecsVec
      fullSchema =
        Vector.fromList
          (fmap mkSlotId [0 :: Int .. nodeCount - 1] <> varOutputSlots)
      outputBindings =
        if isRootDomainQuery
          then
            fmap
              (PlanBuild.PlanOutputBinding primaryRootSlot)
              varsOrdered
          else
            zipWith PlanBuild.PlanOutputBinding varOutputSlots varsOrdered
      residual =
        maybe
          PlanBuild.NoQueryPlanResidual
          (\guardValue ->
            let identityWords = pahResidualWords host guardValue
                identityDigest = digestWords identityWords
             in PlanBuild.QueryPlanResidual
                  { PlanBuild.qprGuard = guardValue,
                    PlanBuild.qprIdentityDigest = identityDigest,
                    PlanBuild.qprShape = ResidualDigestOnly identityDigest identityWords
                  }
          )
          residualGuard

  let queryPlanInput =
        PlanBuild.QueryPlanInput
          { PlanBuild.qpiDomain =
              if isRootDomainQuery
                then PlanBuild.RootDomainQueryPlan
                else PlanBuild.StructuralQueryPlan,
            PlanBuild.qpiCompiled =
              queryValue,
            PlanBuild.qpiDigest =
              queryDigest,
            PlanBuild.qpiAtoms =
              atomSpecsVec,
            PlanBuild.qpiSchemaOrder =
              if isRootDomainQuery
                then Nothing
                else Just fullSchema,
            PlanBuild.qpiRootSlot =
              primaryRootSlot,
            PlanBuild.qpiOutputs =
              outputBindings,
            PlanBuild.qpiResidual =
              residual
          }
  case PlanBuild.mkQueryPlan queryPlanInput of
    Right plan ->
      Right plan
    Left errors ->
      Left (PatternAtomizeInvalidPlan errors)

digestWords :: [Word64] -> Word64
digestWords =
  foldl' mix64 0xcbf29ce484222325
{-# INLINE digestWords #-}
