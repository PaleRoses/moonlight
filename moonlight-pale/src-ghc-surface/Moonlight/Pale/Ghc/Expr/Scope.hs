module Moonlight.Pale.Ghc.Expr.Scope
  ( ScopeId,
    ScopeIdFailure (..),
    ScopeCtx (..),
    ScopeIndex,
    ScopeIndexFailure (..),
    ScopeLookupFailure (..),
    ScopeEulerLabel (..),
    FreeScopeSummary,
    mkScopeId,
    scopeIdKey,
    rootScopeId,
    mkScopeIndex,
    scopeIndexRoot,
    scopeParentId,
    scopeDepthOf,
    scopeIsAncestorOf,
    scopeComparable,
    scopeLca,
    scopeCtxLeq,
    scopeCtxMeet,
    scopeCtxJoin,
    scopeObservedCount,
    scopeObservedContexts,
    scopeTopCtx,
    scopeBottomCtx,
    binderIntroScope,
    binderSiteScope,
    emptyFreeScopeSummary,
    singletonFreeScopeSummary,
    mergeFreeScopeSummary,
    mergeFreeScopeSummaryBy,
    mergeFreeScopeSummaryByEither,
    deleteFreeScopeSummary,
    freeScopeSummaryContains,
    freeScopeSummarySize,
    freeScopeSummaryToList,
    freeScopeSupportAnchor,
  )
where

import Control.Monad (foldM, when)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.Primitive.SmallArray
  ( indexSmallArray,
    sizeofSmallArray,
    smallArrayFromList,
  )
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (BinderId (..), binderIdKey)
import Moonlight.Pale.Ghc.Expr.Scope.Internal (FreeScopeSummary (..), ScopeId (..), ScopeIndex (..))

type ScopeIdFailure :: Type
data ScopeIdFailure
  = NegativeScopeId !Int
  deriving stock (Eq, Ord, Show)

type ScopeCtx :: Type
data ScopeCtx
  = ActualScope !ScopeId
  | IncompatibleScope
  deriving stock (Eq, Ord, Show, Read)

type ScopeEulerLabel :: Type
data ScopeEulerLabel
  = ScopeEulerTin
  | ScopeEulerTout
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ScopeLookupFailure :: Type
data ScopeLookupFailure
  = ScopeIdOutsideIndex !ScopeId !Int
  | BinderIdOutsideIndex !BinderId !Int
  | ScopeLiftLevelOutsideIndex !Int !Int
  deriving stock (Eq, Ord, Show)

type ScopeIndexFailure :: Type
data ScopeIndexFailure
  = ScopeIndexEmpty
  | ScopeRootParentInvalid !Int
  | ScopeParentEdgeInvalid !ScopeId !Int
  | ScopeBinderIntroInvalid !BinderId !Int
  | ScopeDepthMissing !ScopeId
  | ScopeEulerMissing !ScopeEulerLabel !ScopeId
  | ScopeLiftMissing !ScopeId
  | ScopeIndexLookupFailed !ScopeLookupFailure
  deriving stock (Eq, Ord, Show)

mkScopeId :: Int -> Either ScopeIdFailure ScopeId
mkScopeId scopeKey
  | scopeKey < 0 =
      Left (NegativeScopeId scopeKey)
  | otherwise =
      Right (ScopeId scopeKey)

scopeIdKey :: ScopeId -> Int
scopeIdKey (ScopeId scopeKey) =
  scopeKey

rootScopeId :: ScopeId
rootScopeId =
  ScopeId 0

mkScopeIndex :: Vector Int -> Vector Int -> Either ScopeIndexFailure ScopeIndex
mkScopeIndex parentVector binderIntroVector = do
  case V.toList parentVector of
    [] ->
      Left ScopeIndexEmpty
    rootParent : _ ->
      when (rootParent /= 0) (Left (ScopeRootParentInvalid rootParent))
  traverse_ validateParentEdge (zip [1 ..] (drop 1 (V.toList parentVector)))
  traverse_ validateBinderIntro (zip [0 ..] (V.toList binderIntroVector))
  depthVector <- buildDepth parentVector
  let childVector = buildChildren parentVector
  (tinVector, toutVector) <- buildEuler childVector
  liftVector <- buildLift parentVector
  let scopeCount = V.length parentVector
  pure
    ScopeIndex
      { siParent = parentVector,
        siDepth = depthVector,
        siTin = tinVector,
        siTout = toutVector,
        siLift = liftVector,
        siObserved = V.generate scopeCount ScopeId,
        siRoot = rootScopeId,
        siBinderIntro = V.map ScopeId binderIntroVector
      }
  where
    validateParentEdge (scopeKey, parentKey) =
      when (parentKey < 0 || parentKey >= scopeKey) $
        Left (ScopeParentEdgeInvalid (ScopeId scopeKey) parentKey)

    validateBinderIntro (binderKey, introScopeKey) =
      when (introScopeKey < 0 || introScopeKey >= V.length parentVector) $
        Left (ScopeBinderIntroInvalid (BinderId binderKey) introScopeKey)

scopeIndexRoot :: ScopeIndex -> ScopeId
scopeIndexRoot =
  siRoot

scopeParentId :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure ScopeId
scopeParentId scopeIndex scopeId =
  ScopeId <$> scopeVectorValue ScopeIdOutsideIndex scopeId (siParent scopeIndex)

scopeDepthOf :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure Int
scopeDepthOf scopeIndex scopeId =
  scopeVectorValue ScopeIdOutsideIndex scopeId (siDepth scopeIndex)

scopeIsAncestorOf :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure Bool
scopeIsAncestorOf scopeIndex leftScope rightScope = do
  leftTin <- scopeTinOf scopeIndex leftScope
  rightTin <- scopeTinOf scopeIndex rightScope
  leftTout <- scopeToutOf scopeIndex leftScope
  rightTout <- scopeToutOf scopeIndex rightScope
  pure (leftTin <= rightTin && rightTout <= leftTout)

scopeComparable :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure Bool
scopeComparable scopeIndex leftScope rightScope =
  (||)
    <$> scopeIsAncestorOf scopeIndex leftScope rightScope
    <*> scopeIsAncestorOf scopeIndex rightScope leftScope

scopeLca :: ScopeIndex -> ScopeId -> ScopeId -> Either ScopeLookupFailure ScopeId
scopeLca scopeIndex leftScope rightScope = do
  leftAncestor <- scopeIsAncestorOf scopeIndex leftScope rightScope
  rightAncestor <- scopeIsAncestorOf scopeIndex rightScope leftScope
  if leftAncestor
    then pure leftScope
    else
      if rightAncestor
        then pure rightScope
        else scopeParentId scopeIndex =<< climb leftScope (V.length (siLift scopeIndex) - 1)
  where
    climb currentScope liftIndex
      | liftIndex < 0 =
          pure currentScope
      | otherwise = do
          ancestorScope <- liftAncestor scopeIndex liftIndex currentScope
          ancestorOfRight <- scopeIsAncestorOf scopeIndex ancestorScope rightScope
          if ancestorOfRight
            then climb currentScope (liftIndex - 1)
            else climb ancestorScope (liftIndex - 1)

scopeCtxLeq :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure Bool
scopeCtxLeq _ IncompatibleScope IncompatibleScope =
  Right True
scopeCtxLeq _ IncompatibleScope _ =
  Right False
scopeCtxLeq _ _ IncompatibleScope =
  Right True
scopeCtxLeq scopeIndex (ActualScope leftScope) (ActualScope rightScope) =
  scopeIsAncestorOf scopeIndex leftScope rightScope

scopeCtxMeet :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure ScopeCtx
scopeCtxMeet _ IncompatibleScope rightCtx =
  Right rightCtx
scopeCtxMeet _ leftCtx IncompatibleScope =
  Right leftCtx
scopeCtxMeet scopeIndex (ActualScope leftScope) (ActualScope rightScope) =
  ActualScope <$> scopeLca scopeIndex leftScope rightScope

scopeCtxJoin :: ScopeIndex -> ScopeCtx -> ScopeCtx -> Either ScopeLookupFailure ScopeCtx
scopeCtxJoin _ IncompatibleScope _ =
  Right IncompatibleScope
scopeCtxJoin _ _ IncompatibleScope =
  Right IncompatibleScope
scopeCtxJoin scopeIndex (ActualScope leftScope) (ActualScope rightScope) = do
  leftAncestor <- scopeIsAncestorOf scopeIndex leftScope rightScope
  rightAncestor <- scopeIsAncestorOf scopeIndex rightScope leftScope
  pure $
    if leftAncestor
      then ActualScope rightScope
      else
        if rightAncestor
          then ActualScope leftScope
          else IncompatibleScope

scopeObservedCount :: ScopeIndex -> Int
scopeObservedCount =
  V.length . siObserved

scopeObservedContexts :: ScopeIndex -> Either ScopeLookupFailure [ScopeCtx]
scopeObservedContexts scopeIndex = do
  needsIncompatible <- observedNeedsIncompatible scopeIndex
  pure (fmap ActualScope (V.toList (siObserved scopeIndex)) <> [IncompatibleScope | needsIncompatible])

scopeTopCtx :: ScopeIndex -> Either ScopeLookupFailure ScopeCtx
scopeTopCtx scopeIndex = do
  needsIncompatible <- observedNeedsIncompatible scopeIndex
  if needsIncompatible
    then pure IncompatibleScope
    else ActualScope <$> deepestObservedScope scopeIndex

scopeBottomCtx :: ScopeIndex -> ScopeCtx
scopeBottomCtx =
  ActualScope . siRoot

binderIntroScope :: ScopeIndex -> BinderId -> Either ScopeLookupFailure ScopeId
binderIntroScope scopeIndex binderId =
  binderVectorValue binderId (siBinderIntro scopeIndex)

binderSiteScope :: ScopeIndex -> BinderId -> Either ScopeLookupFailure ScopeId
binderSiteScope scopeIndex binderId =
  scopeParentId scopeIndex =<< binderIntroScope scopeIndex binderId

emptyFreeScopeSummary :: FreeScopeSummary
emptyFreeScopeSummary =
  FreeScopeSummary (smallArrayFromList [])

singletonFreeScopeSummary :: ScopeId -> FreeScopeSummary
singletonFreeScopeSummary scopeId =
  FreeScopeSummary (smallArrayFromList [scopeId])

mergeFreeScopeSummary :: ScopeIndex -> FreeScopeSummary -> FreeScopeSummary -> Either ScopeLookupFailure FreeScopeSummary
mergeFreeScopeSummary scopeIndex =
  mergeFreeScopeSummaryByEither (scopeDepthOf scopeIndex)

mergeFreeScopeSummaryBy :: (ScopeId -> Int) -> FreeScopeSummary -> FreeScopeSummary -> FreeScopeSummary
mergeFreeScopeSummaryBy depthOf leftSummary rightSummary =
  FreeScopeSummary
    ( smallArrayFromList
        (go (freeScopeSummaryToList leftSummary) (freeScopeSummaryToList rightSummary))
    )
  where
    go leftValues rightValues =
      case (leftValues, rightValues) of
        ([], []) ->
          []
        ([], _) ->
          rightValues
        (_, []) ->
          leftValues
        (leftScope : remainingLeft, rightScope : remainingRight)
          | leftScope == rightScope ->
              leftScope : go remainingLeft remainingRight
          | depthOf leftScope > depthOf rightScope ->
              leftScope : go remainingLeft rightValues
          | depthOf rightScope > depthOf leftScope ->
              rightScope : go leftValues remainingRight
          | otherwise ->
              []

mergeFreeScopeSummaryByEither ::
  (ScopeId -> Either failure Int) ->
  FreeScopeSummary ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
mergeFreeScopeSummaryByEither depthOf leftSummary rightSummary =
  FreeScopeSummary . smallArrayFromList
    <$> go (freeScopeSummaryToList leftSummary) (freeScopeSummaryToList rightSummary)
  where
    go leftValues rightValues =
      case (leftValues, rightValues) of
        ([], []) ->
          Right []
        ([], _) ->
          Right rightValues
        (_, []) ->
          Right leftValues
        (leftScope : remainingLeft, rightScope : remainingRight)
          | leftScope == rightScope ->
              (leftScope :) <$> go remainingLeft remainingRight
          | otherwise -> do
              leftDepth <- depthOf leftScope
              rightDepth <- depthOf rightScope
              case compare leftDepth rightDepth of
                GT ->
                  (leftScope :) <$> go remainingLeft rightValues
                LT ->
                  (rightScope :) <$> go leftValues remainingRight
                EQ ->
                  Right []

deleteFreeScopeSummary :: ScopeId -> FreeScopeSummary -> FreeScopeSummary
deleteFreeScopeSummary targetScope summaryValue =
  FreeScopeSummary
    ( smallArrayFromList
        (filter (/= targetScope) (freeScopeSummaryToList summaryValue))
    )

freeScopeSummaryContains :: ScopeId -> FreeScopeSummary -> Bool
freeScopeSummaryContains targetScope summaryValue =
  go 0
  where
    FreeScopeSummary scopeArray = summaryValue
    go indexValue
      | indexValue >= sizeofSmallArray scopeArray =
          False
      | indexSmallArray scopeArray indexValue == targetScope =
          True
      | otherwise =
          go (indexValue + 1)

freeScopeSummarySize :: FreeScopeSummary -> Int
freeScopeSummarySize (FreeScopeSummary scopeArray) =
  sizeofSmallArray scopeArray

freeScopeSummaryToList :: FreeScopeSummary -> [ScopeId]
freeScopeSummaryToList (FreeScopeSummary scopeArray) =
  [ indexSmallArray scopeArray indexValue
  | indexValue <- [0 .. sizeofSmallArray scopeArray - 1]
  ]

freeScopeSupportAnchor :: ScopeIndex -> FreeScopeSummary -> ScopeId
freeScopeSupportAnchor scopeIndex summaryValue =
  case freeScopeSummaryToList summaryValue of
    anchorScope : _ ->
      anchorScope
    [] ->
      siRoot scopeIndex

buildDepth :: Vector Int -> Either ScopeIndexFailure (Vector Int)
buildDepth parentVector =
  V.fromList . reverse . snd
    <$> foldM buildDepthEntry (IntMap.empty, []) (zip [0 ..] (V.toList parentVector))
  where
    buildDepthEntry ::
      Num depth =>
      (IntMap.IntMap depth, [depth]) ->
      (Int, Int) ->
      Either ScopeIndexFailure (IntMap.IntMap depth, [depth])
    buildDepthEntry (depthMap, depthValues) (scopeKey, parentKey)
      | scopeKey == 0 =
          Right (IntMap.singleton 0 0, 0 : depthValues)
      | otherwise =
          case IntMap.lookup parentKey depthMap of
            Nothing ->
              Left (ScopeDepthMissing (ScopeId parentKey))
            Just parentDepth ->
              let scopeDepth = parentDepth + 1
               in Right (IntMap.insert scopeKey scopeDepth depthMap, scopeDepth : depthValues)

buildChildren :: Vector Int -> Vector [Int]
buildChildren parentVector =
  let scopeCount = V.length parentVector
   in V.map reverse $
        V.accum
          (flip (:))
          (V.replicate scopeCount [])
          [ (parentKey, scopeKey)
          | (scopeKey, parentKey) <- zip [1 ..] (drop 1 (V.toList parentVector))
          ]

buildEuler :: Vector [Int] -> Either ScopeIndexFailure (Vector Int, Vector Int)
buildEuler childVector = do
  let scopeCount = V.length childVector
  (_, tinMap, toutMap) <- dfs 0 0 IntMap.empty IntMap.empty
  tinVector <- materialize ScopeEulerTin scopeCount tinMap
  toutVector <- materialize ScopeEulerTout scopeCount toutMap
  Right (tinVector, toutVector)
  where
    dfs scopeKey clockValue tinAcc toutAcc = do
      childKeys <-
        maybe
          (Left (ScopeIndexLookupFailed (ScopeIdOutsideIndex (ScopeId scopeKey) (V.length childVector))))
          Right
          (childVector V.!? scopeKey)
      let tinAcc' = IntMap.insert scopeKey clockValue tinAcc
          clockAfterEnter = clockValue + 1
      (clockAfterChildren, tinAcc'', toutAcc') <-
        foldM
          ( \(clockValue', tinValue', toutValue') childKey ->
              dfs childKey clockValue' tinValue' toutValue'
          )
          (clockAfterEnter, tinAcc', toutAcc)
          childKeys
      let toutAcc'' = IntMap.insert scopeKey clockAfterChildren toutAcc'
      Right (clockAfterChildren + 1, tinAcc'', toutAcc'')

    materialize :: ScopeEulerLabel -> Int -> IntMap.IntMap value -> Either ScopeIndexFailure (Vector value)
    materialize label scopeCount entries =
      V.fromList
        <$> traverse
          ( \scopeKey ->
              maybe
                (Left (ScopeEulerMissing label (ScopeId scopeKey)))
                Right
                (IntMap.lookup scopeKey entries)
          )
          [0 .. scopeCount - 1]

buildLift :: Vector Int -> Either ScopeIndexFailure (Vector (Vector Int))
buildLift parentVector =
  V.fromList . reverse
    <$> foldM appendLevel [parentVector] [1 .. levelCount - 1]
  where
    scopeCount = V.length parentVector

    levelCount =
      max 1 (1 + length (takeWhile (< scopeCount) (iterate (* 2) 1)))

    appendLevel :: [Vector Int] -> Int -> Either ScopeIndexFailure [Vector Int]
    appendLevel levels _ =
      case levels of
        [] ->
          Left (ScopeLiftMissing rootScopeId)
        previousLevel : _ -> do
          nextLevel <- traverse (nextAncestor previousLevel) previousLevel
          Right (nextLevel : levels)

    nextAncestor :: Vector Int -> Int -> Either ScopeIndexFailure Int
    nextAncestor previousLevel ancestorKey =
      maybe
        (Left (ScopeLiftMissing (ScopeId ancestorKey)))
        Right
        (previousLevel V.!? ancestorKey)

liftAncestor :: ScopeIndex -> Int -> ScopeId -> Either ScopeLookupFailure ScopeId
liftAncestor scopeIndex liftIndex scopeId =
  case siLift scopeIndex V.!? liftIndex of
    Nothing ->
      Left (ScopeLiftLevelOutsideIndex liftIndex (V.length (siLift scopeIndex)))
    Just liftLevel ->
      ScopeId <$> scopeVectorValue ScopeIdOutsideIndex scopeId liftLevel

observedNeedsIncompatible :: ScopeIndex -> Either ScopeLookupFailure Bool
observedNeedsIncompatible scopeIndex =
  or
    <$> traverse
      ( \(leftScope, rightScope) ->
          not <$> scopeComparable scopeIndex leftScope rightScope
      )
      [ (leftScope, rightScope)
      | leftScope : remainingScopes <- List.tails (V.toList (siObserved scopeIndex)),
        rightScope <- remainingScopes
      ]

deepestObservedScope :: ScopeIndex -> Either ScopeLookupFailure ScopeId
deepestObservedScope scopeIndex =
  case V.toList (siObserved scopeIndex) of
    [] ->
      Right (siRoot scopeIndex)
    firstScope : remainingScopes ->
      foldM deepest firstScope remainingScopes
  where
    deepest currentScope candidateScope = do
      currentDepth <- scopeDepthOf scopeIndex currentScope
      candidateDepth <- scopeDepthOf scopeIndex candidateScope
      pure $
        if candidateDepth > currentDepth
          then candidateScope
          else currentScope

scopeTinOf :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure Int
scopeTinOf scopeIndex scopeId =
  scopeVectorValue ScopeIdOutsideIndex scopeId (siTin scopeIndex)

scopeToutOf :: ScopeIndex -> ScopeId -> Either ScopeLookupFailure Int
scopeToutOf scopeIndex scopeId =
  scopeVectorValue ScopeIdOutsideIndex scopeId (siTout scopeIndex)

scopeVectorValue :: (ScopeId -> Int -> ScopeLookupFailure) -> ScopeId -> Vector value -> Either ScopeLookupFailure value
scopeVectorValue failure scopeId vectorValue =
  maybe
    (Left (failure scopeId (V.length vectorValue)))
    Right
    (vectorValue V.!? scopeIdKey scopeId)

binderVectorValue :: BinderId -> Vector ScopeId -> Either ScopeLookupFailure ScopeId
binderVectorValue binderId vectorValue =
  maybe
    (Left (BinderIdOutsideIndex binderId (V.length vectorValue)))
    Right
    (vectorValue V.!? binderIdKey binderId)
