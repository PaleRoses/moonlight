{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Guard language and evaluator for system logic.
-- Owns CNF normalization, canonical guard digests, capability resolution,
-- clause/literal evidence for equality, facts, and capabilities, and
-- store-independent literal grounding for the closure engine.
-- Contracts: negative fact literals produce typed absence witnesses, and
-- ground literals reproduce evaluator evidence at any store.
module Moonlight.Rewrite.System.Logic.Guard
  ( GuardCapabilityResolver (..),
    emptyGuardCapabilityResolver,
    GuardBase (..),
    GuardChildIndex,
    guardChildIndex,
    guardChildIndexValue,
    GuardPath (..),
    GuardRef (..),
    data GuardRoot,
    data GuardVar,
    guardProject,
    GuardTerm (..),
    guardRefTerm,
    guardProjectTerm,
    GuardAtom (..),
    GuardExpr,
    RewriteCondition (..),
    CompiledGuard,
    compiledGuardNormalizedExpression,
    compiledGuardClauses,
    mapCompiledGuard,
    GuardLiteralEvidence (..),
    GuardClauseEvidence (..),
    GuardEvidence (..),
    guardTrue,
    guardFalse,
    guardEquivalent,
    guardHasFact,
    guardHasFactTerms,
    guardHasCapability,
    guardHasCapabilityTerms,
    combineCompiledGuards,
    compileGuard,
    checkCompiledGuard,
    guardVariables,
    guardAtomVariables,
    guardTermVariables,
    guardRefVariables,
    guardRefPatternVar,
    guardAtomCase,
    compiledGuardVariables,
    compiledGuardAtoms,
    compiledGuardDigestWith,
    compiledGuardCanonicalWordsWith,
    compiledGuardCanonicalNodeWordsWith,
    canonicalizeGuardEvidence,
    evaluateCompiledGuardWithEvidenceAndCapabilities,
    GroundGuardLiteral (..),
    groundCompiledGuard,
  )
where

import Data.Foldable qualified as Foldable
import Data.Functor (void)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Constraint
  ( CNF,
    Clause,
    ConstraintExpr (..),
    Literal,
    atoms,
    literalPolarity,
    literalVariable,
    normalize,
    toCNF,
  )
import Moonlight.Core
  ( ClassId,
    HasConstructorTag (..),
    Language,
    Pattern,
    PatternVar,
    mixWord64,
    patternVarKey,
  )
import Moonlight.Rewrite.System.Logic.Store
  ( FactId (..),
    FactStore,
    FactTuple (..),
    FactWitness (..),
    GuardClauseEvidence (..),
    GuardEvidence (..),
    GuardLiteralEvidence (..),
    canonicalizeFactTuple,
    canonicalizeFactWitness,
    emptyFactStore,
    guardEvidenceFromClauses,
    hasFact,
  )

type GuardCapabilityResolver :: Type -> Type
newtype GuardCapabilityResolver capability = GuardCapabilityResolver
  { runGuardCapabilityResolver :: capability -> [ClassId] -> Bool
  }

emptyGuardCapabilityResolver :: GuardCapabilityResolver capability
emptyGuardCapabilityResolver =
  GuardCapabilityResolver (\_ _ -> False)

type GuardBase :: Type
data GuardBase
  = GuardFromRoot
  | GuardFromVar PatternVar
  deriving stock (Eq, Ord, Show, Read)

type GuardChildIndex :: Type
newtype GuardChildIndex = GuardChildIndex
  { guardChildIndexValue :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)

guardChildIndex :: Word64 -> GuardChildIndex
guardChildIndex =
  GuardChildIndex

type GuardPath :: Type
newtype GuardPath = GuardPath
  { unGuardPath :: [GuardChildIndex]
  }
  deriving stock (Eq, Ord, Show, Read)

type GuardRef :: Type
newtype GuardRef = GuardRef
  { unGuardRef :: (GuardBase, GuardPath)
  }
  deriving stock (Eq, Ord, Show, Read)

pattern GuardRoot :: GuardRef
pattern GuardRoot = GuardRef (GuardFromRoot, GuardPath [])

pattern GuardVar :: PatternVar -> GuardRef
pattern GuardVar patternVar = GuardRef (GuardFromVar patternVar, GuardPath [])

guardProject :: GuardRef -> GuardChildIndex -> GuardRef
guardProject (GuardRef (guardBase, GuardPath childIndices)) childIndex =
  GuardRef (guardBase, GuardPath (childIndices <> [childIndex]))

type GuardTerm :: (Type -> Type) -> Type
data GuardTerm f
  = GuardRefTerm GuardRef
  | GuardProjectTerm (GuardTerm f) GuardChildIndex
  | GuardNodeTerm (f (GuardTerm f))

instance (forall a. Ord a => Ord (f a)) => Eq (GuardTerm f) where
  leftGuardTerm == rightGuardTerm =
    compare leftGuardTerm rightGuardTerm == EQ

instance (forall a. Ord a => Ord (f a)) => Ord (GuardTerm f) where
  compare leftGuardTerm rightGuardTerm =
    case (leftGuardTerm, rightGuardTerm) of
      (GuardRefTerm leftRef, GuardRefTerm rightRef) -> compare leftRef rightRef
      (GuardRefTerm _, _) -> LT
      (_, GuardRefTerm _) -> GT
      (GuardProjectTerm leftTerm leftChildIndex, GuardProjectTerm rightTerm rightChildIndex) ->
        compare (leftTerm, leftChildIndex) (rightTerm, rightChildIndex)
      (GuardProjectTerm _ _, GuardNodeTerm _) -> LT
      (GuardNodeTerm _, GuardProjectTerm _ _) -> GT
      (GuardNodeTerm leftNode, GuardNodeTerm rightNode) -> compare leftNode rightNode

instance (forall a. Show a => Show (f a)) => Show (GuardTerm f) where
  showsPrec precedence guardTerm =
    case guardTerm of
      GuardRefTerm guardRef ->
        showParen (precedence > applicationPrecedence)
          (showString "GuardRefTerm " . showsPrec (applicationPrecedence + 1) guardRef)
      GuardProjectTerm baseTerm childIndex ->
        showParen (precedence > applicationPrecedence)
          ( showString "GuardProjectTerm "
              . showsPrec (applicationPrecedence + 1) baseTerm
              . showString " "
              . showsPrec (applicationPrecedence + 1) childIndex
          )
      GuardNodeTerm guardNode ->
        showParen (precedence > applicationPrecedence)
          (showString "GuardNodeTerm " . showsPrec (applicationPrecedence + 1) guardNode)
    where
      applicationPrecedence = 10

guardRefTerm :: GuardRef -> GuardTerm f
guardRefTerm = GuardRefTerm

guardProjectTerm :: GuardTerm f -> GuardChildIndex -> GuardTerm f
guardProjectTerm = GuardProjectTerm

type GuardAtom :: Type -> (Type -> Type) -> Type
data GuardAtom capability f
  = ClassesEquivalent (GuardTerm f) (GuardTerm f)
  | HasFact FactId [GuardTerm f]
  | HasCapability capability [GuardTerm f]

deriving stock instance (Eq capability, Eq (GuardTerm f)) => Eq (GuardAtom capability f)
deriving stock instance (Ord capability, Ord (GuardTerm f)) => Ord (GuardAtom capability f)
deriving stock instance (Show capability, Show (GuardTerm f)) => Show (GuardAtom capability f)

type GuardExpr :: Type -> (Type -> Type) -> Type
type GuardExpr capability f = ConstraintExpr (GuardAtom capability f)

type RewriteCondition :: Type -> (Type -> Type) -> Type
newtype RewriteCondition capability f = RewriteCondition
  { rewriteGuardExpr :: GuardExpr capability f
  }

deriving stock instance (Eq capability, Eq (GuardTerm f)) => Eq (RewriteCondition capability f)
deriving stock instance (Ord capability, Ord (GuardTerm f)) => Ord (RewriteCondition capability f)
deriving stock instance (Show capability, Show (GuardTerm f)) => Show (RewriteCondition capability f)

instance (Ord capability, Ord (GuardTerm f)) => Semigroup (RewriteCondition capability f) where
  leftCondition <> rightCondition =
    RewriteCondition
      ( normalize
          (And [rewriteGuardExpr leftCondition, rewriteGuardExpr rightCondition])
      )

instance (Ord capability, Ord (GuardTerm f)) => Monoid (RewriteCondition capability f) where
  mempty =
    RewriteCondition guardTrue

type CompiledGuard :: Type -> (Type -> Type) -> Type
data CompiledGuard capability f
  = CompiledGuard
      { cgNormalizedExpr :: GuardExpr capability f,
        cgClauses :: CNF (GuardAtom capability f)
      }

deriving stock instance (Eq capability, Eq (GuardTerm f)) => Eq (CompiledGuard capability f)
deriving stock instance (Ord capability, Ord (GuardTerm f)) => Ord (CompiledGuard capability f)
deriving stock instance (Show capability, Show (GuardTerm f)) => Show (CompiledGuard capability f)

compiledGuardNormalizedExpression :: CompiledGuard capability f -> GuardExpr capability f
compiledGuardNormalizedExpression =
  cgNormalizedExpr

compiledGuardClauses :: CompiledGuard capability f -> CNF (GuardAtom capability f)
compiledGuardClauses =
  cgClauses

mapCompiledGuard ::
  (Ord mappedCapability, Ord (GuardTerm mappedF)) =>
  (GuardExpr capability f -> GuardExpr mappedCapability mappedF) ->
  CompiledGuard capability f ->
  CompiledGuard mappedCapability mappedF
mapCompiledGuard transformExpression compiledGuard =
  let normalizedExpression =
        normalize (transformExpression (compiledGuardNormalizedExpression compiledGuard))
   in CompiledGuard normalizedExpression (toCNF normalizedExpression)

instance (Ord capability, Ord (GuardTerm f)) => Semigroup (CompiledGuard capability f) where
  (<>) =
    andCompiledGuards

instance (Ord capability, Ord (GuardTerm f)) => Monoid (CompiledGuard capability f) where
  mempty =
    CompiledGuard guardTrue (toCNF guardTrue)

guardTrue :: GuardExpr capability f
guardTrue =
  And []

guardFalse :: GuardExpr capability f
guardFalse =
  Or []

guardEquivalent :: PatternVar -> PatternVar -> GuardExpr capability f
guardEquivalent leftPatternVar rightPatternVar =
  Atom (ClassesEquivalent (guardRefTerm (GuardVar leftPatternVar)) (guardRefTerm (GuardVar rightPatternVar)))

guardHasFact :: FactId -> [GuardRef] -> GuardExpr capability f
guardHasFact factId guardRefs =
  guardHasFactTerms factId (fmap guardRefTerm guardRefs)

guardHasFactTerms :: FactId -> [GuardTerm f] -> GuardExpr capability f
guardHasFactTerms factId guardTerms =
  Atom (HasFact factId guardTerms)

guardHasCapability :: capability -> [GuardRef] -> GuardExpr capability f
guardHasCapability capability guardRefs =
  guardHasCapabilityTerms capability (fmap guardRefTerm guardRefs)

guardHasCapabilityTerms :: capability -> [GuardTerm f] -> GuardExpr capability f
guardHasCapabilityTerms capability guardTerms =
  Atom (HasCapability capability guardTerms)

combineCompiledGuards :: (Ord capability, Ord (GuardTerm f)) => [CompiledGuard capability f] -> Maybe (CompiledGuard capability f)
combineCompiledGuards compiledGuards =
  sconcat <$> NonEmpty.nonEmpty compiledGuards

compileGuard ::
  (Foldable f, Ord capability, Ord (GuardTerm f)) =>
  Set PatternVar ->
  RewriteCondition capability f ->
  Either [PatternVar] (CompiledGuard capability f)
compileGuard boundVariables (RewriteCondition guardExpr) =
  let unboundGuardVariables =
        Set.toAscList (Set.difference (guardVariables guardExpr) boundVariables)
      normalizedExpr = normalize guardExpr
   in if null unboundGuardVariables
        then Right (CompiledGuard normalizedExpr (toCNF normalizedExpr))
        else Left unboundGuardVariables

checkCompiledGuard ::
  Foldable f =>
  Ord capability =>
  Ord (GuardTerm f) =>
  Map PatternVar (Pattern f) ->
  CompiledGuard capability f ->
  Bool
checkCompiledGuard substitution compiledGuard =
  all (`Map.member` substitution) (compiledGuardVariables compiledGuard)

compiledGuardVariables :: (Foldable f, Ord capability, Ord (GuardTerm f)) => CompiledGuard capability f -> Set PatternVar
compiledGuardVariables =
  \case
    CompiledGuard normalizedExpr _ ->
      guardVariables normalizedExpr

guardVariables :: (Foldable f, Ord capability, Ord (GuardTerm f)) => GuardExpr capability f -> Set PatternVar
guardVariables =
  foldMap guardAtomVariables . atoms

guardAtomVariables :: Foldable f => GuardAtom capability f -> Set PatternVar
guardAtomVariables guardAtom =
  case guardAtom of
    ClassesEquivalent leftTerm rightTerm ->
      guardTermVariables leftTerm <> guardTermVariables rightTerm
    HasFact _ guardTerms ->
      foldMap guardTermVariables guardTerms
    HasCapability _ guardTerms ->
      foldMap guardTermVariables guardTerms

guardTermVariables :: Foldable f => GuardTerm f -> Set PatternVar
guardTermVariables guardTerm =
  case guardTerm of
    GuardRefTerm guardRef -> guardRefVariables guardRef
    GuardProjectTerm baseTerm _ -> guardTermVariables baseTerm
    GuardNodeTerm guardNode -> foldMap guardTermVariables guardNode

guardRefVariables :: GuardRef -> Set PatternVar
guardRefVariables =
  maybe Set.empty Set.singleton . guardRefPatternVar

guardRefPatternVar :: GuardRef -> Maybe PatternVar
guardRefPatternVar (GuardRef (guardBase, _)) =
  case guardBase of
    GuardFromRoot -> Nothing
    GuardFromVar patternVar -> Just patternVar

guardAtomCase ::
  (GuardTerm f -> GuardTerm f -> resultValue) ->
  (FactId -> [GuardTerm f] -> resultValue) ->
  (capability -> [GuardTerm f] -> resultValue) ->
  GuardAtom capability f ->
  resultValue
guardAtomCase handleClassesEquivalent handleHasFact handleHasCapability =
  \case
    ClassesEquivalent leftTerm rightTerm -> handleClassesEquivalent leftTerm rightTerm
    HasFact factId guardTerms -> handleHasFact factId guardTerms
    HasCapability capability guardTerms -> handleHasCapability capability guardTerms

compiledGuardAtoms :: CompiledGuard capability f -> [GuardAtom capability f]
compiledGuardAtoms =
  \case
    CompiledGuard _ clauses ->
      concatMap (concatMap (\literal -> [literalVariable literal]) . Set.toList) clauses

compiledGuardDigestWith ::
  HasConstructorTag f =>
  (capability -> Word64) ->
  (ConstructorTag f -> Word64) ->
  CompiledGuard capability f ->
  Word64
compiledGuardDigestWith capabilityDigest tagDigest =
  digestWords . compiledGuardCanonicalWordsWith capabilityDigest tagDigest

compiledGuardCanonicalWordsWith ::
  HasConstructorTag f =>
  (capability -> Word64) ->
  (ConstructorTag f -> Word64) ->
  CompiledGuard capability f ->
  [Word64]
compiledGuardCanonicalWordsWith capabilityDigest tagDigest =
  compiledGuardCanonicalNodeWordsWith capabilityDigest (tagDigest . constructorTag)

compiledGuardCanonicalNodeWordsWith ::
  Language f =>
  (capability -> Word64) ->
  (f () -> Word64) ->
  CompiledGuard capability f ->
  [Word64]
compiledGuardCanonicalNodeWordsWith capabilityDigest nodeDigest (CompiledGuard _ clauses) =
  let clauseStreams =
        List.sort (fmap (guardClauseWordsWith capabilityDigest nodeDigest) clauses)
   in 0x400 : wordOfInt (length clauseStreams) : concat clauseStreams

guardClauseWordsWith ::
  Language f =>
  (capability -> Word64) ->
  (f () -> Word64) ->
  Set (Literal (GuardAtom capability f)) ->
  [Word64]
guardClauseWordsWith capabilityDigest nodeDigest clause =
  0x401
    : wordOfInt (Set.size clause)
    : concatMap (guardLiteralWordsWith capabilityDigest nodeDigest) (Set.toAscList clause)

guardLiteralWordsWith ::
  Language f =>
  (capability -> Word64) ->
  (f () -> Word64) ->
  Literal (GuardAtom capability f) ->
  [Word64]
guardLiteralWordsWith capabilityDigest nodeDigest literal =
  (if literalPolarity literal then 0x402 else 0x403)
    : guardAtomWordsWith capabilityDigest nodeDigest (literalVariable literal)

guardAtomWordsWith ::
  Language f =>
  (capability -> Word64) ->
  (f () -> Word64) ->
  GuardAtom capability f ->
  [Word64]
guardAtomWordsWith capabilityDigest nodeDigest =
  guardAtomCase
    (classesEquivalentWordsWith nodeDigest)
    (\(FactId factKey) guardTerms -> 0x20 : wordOfInt factKey : wordOfInt (length guardTerms) : concatMap (guardTermWordsWith nodeDigest) guardTerms)
    (\capability guardTerms -> 0x30 : capabilityDigest capability : wordOfInt (length guardTerms) : concatMap (guardTermWordsWith nodeDigest) guardTerms)

classesEquivalentWordsWith ::
  Language f =>
  (f () -> Word64) ->
  GuardTerm f ->
  GuardTerm f ->
  [Word64]
classesEquivalentWordsWith nodeDigest leftTerm rightTerm =
  let leftWords =
        guardTermWordsWith nodeDigest leftTerm
      rightWords =
        guardTermWordsWith nodeDigest rightTerm
      (lowerWords, upperWords) =
        if leftWords <= rightWords
          then (leftWords, rightWords)
          else (rightWords, leftWords)
   in 0x10 : lowerWords <> upperWords

guardTermWordsWith ::
  Language f =>
  (f () -> Word64) ->
  GuardTerm f ->
  [Word64]
guardTermWordsWith nodeDigest guardTerm =
  guardTermWordsWithSuffix nodeDigest guardTerm []

guardTermWordsWithSuffix ::
  Language f =>
  (f () -> Word64) ->
  GuardTerm f ->
  [Word64] ->
  [Word64]
guardTermWordsWithSuffix nodeDigest guardTerm suffix =
  case guardTerm of
    GuardRefTerm guardRef ->
      0x100 : guardRefWords guardRef <> suffix
    GuardProjectTerm nestedTerm childIndex ->
      0x101
        : guardTermWordsWithSuffix
          nodeDigest
          nestedTerm
          (guardChildIndexValue childIndex : suffix)
    GuardNodeTerm guardNode ->
      let childTerms = Foldable.toList guardNode
       in 0x102
            : nodeDigest (void guardNode)
            : wordOfInt (length childTerms)
            : foldr (guardTermWordsWithSuffix nodeDigest) suffix childTerms

guardRefWords :: GuardRef -> [Word64]
guardRefWords (GuardRef (guardBase, GuardPath childIndices)) =
  guardBaseWords guardBase
    <> (wordOfInt (length childIndices) : fmap guardChildIndexWord childIndices)
  where
    guardBaseWords = \case
      GuardFromRoot ->
        [0x110]
      GuardFromVar patternVar ->
        [0x111, wordOfInt (patternVarKey patternVar)]

    guardChildIndexWord =
      guardChildIndexValue

digestWords :: [Word64] -> Word64
digestWords =
  foldl' mixWord64 0xcbf29ce484222325

wordOfInt :: Int -> Word64
wordOfInt =
  fromIntegral

canonicalizeGuardEvidence :: (ClassId -> ClassId) -> GuardEvidence -> GuardEvidence
canonicalizeGuardEvidence canonicalizeClassId =
  guardEvidenceFromClauses
    . fmap canonicalizeGuardClauseEvidence
    . geClauses
  where
    canonicalizeGuardClauseEvidence (GuardClauseEvidence literalEvidences) =
      GuardClauseEvidence (fmap canonicalizeGuardLiteralEvidence literalEvidences)

    canonicalizeGuardLiteralEvidence = \case
      GuardFactPresent factWitness ->
        GuardFactPresent (canonicalizeFactWitness canonicalizeClassId factWitness)
      GuardFactAbsent factId factTuple ->
        GuardFactAbsent factId (canonicalizeFactTuple canonicalizeClassId factTuple)
      GuardClassesEqual leftClassId rightClassId ->
        GuardClassesEqual (canonicalizeClassId leftClassId) (canonicalizeClassId rightClassId)
      GuardClassesDistinct leftClassId rightClassId ->
        GuardClassesDistinct (canonicalizeClassId leftClassId) (canonicalizeClassId rightClassId)
      GuardCapabilityHeld ->
        GuardCapabilityHeld
      GuardCapabilityMissing ->
        GuardCapabilityMissing

evaluateCompiledGuardWithEvidenceAndCapabilities ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  CompiledGuard capability f ->
  Maybe GuardEvidence
evaluateCompiledGuardWithEvidenceAndCapabilities factStore capabilityResolver canonicalizeClassId resolveTerm =
  \case
    CompiledGuard _ clauses ->
      guardEvidenceFromClauses
        <$> traverse
          (clauseEvidence factStore capabilityResolver canonicalizeClassId resolveTerm)
          clauses

clauseEvidence ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  Clause (GuardAtom capability f) ->
  Maybe GuardClauseEvidence
clauseEvidence factStore capabilityResolver canonicalizeClassId resolveTerm clause =
  let literalAssessments =
        fmap
          (literalEvidence factStore capabilityResolver canonicalizeClassId resolveTerm)
          (Set.toAscList clause)
      satisfiedLiteralEvidences =
        foldMap
          (\literalAssessment ->
             if glaSatisfied literalAssessment
               then glaEvidence literalAssessment
               else []
          )
          literalAssessments
   in if any glaSatisfied literalAssessments
        then Just (GuardClauseEvidence satisfiedLiteralEvidences)
        else Nothing

literalEvidence ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  Literal (GuardAtom capability f) ->
  GuardLiteralAssessment
literalEvidence factStore capabilityResolver canonicalizeClassId resolveTerm literal =
  let atomAssessment =
        atomEvidence factStore capabilityResolver canonicalizeClassId resolveTerm (literalVariable literal)
      satisfied =
        if literalPolarity literal
          then gaaSatisfied atomAssessment
          else not (gaaSatisfied atomAssessment)
   in GuardLiteralAssessment
        { glaSatisfied = satisfied,
          glaEvidence =
            if satisfied
              then
                if literalPolarity literal
                  then gaaPositiveEvidence atomAssessment
                  else gaaNegativeEvidence atomAssessment
              else []
        }

atomEvidence ::
  FactStore ->
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  GuardAtom capability f ->
  GuardAtomAssessment
atomEvidence factStore capabilityResolver canonicalizeClassId resolveTerm =
  \case
    ClassesEquivalent leftTerm rightTerm ->
      case (,) <$> resolveCanonicalTerm leftTerm <*> resolveCanonicalTerm rightTerm of
        Nothing ->
          unsatisfiedAtom
        Just (leftClassId, rightClassId) ->
          let classesEqual =
                leftClassId == rightClassId
           in GuardAtomAssessment
                { gaaSatisfied = classesEqual,
                  gaaPositiveEvidence =
                    if classesEqual
                      then [GuardClassesEqual leftClassId rightClassId]
                      else [],
                  gaaNegativeEvidence =
                    if classesEqual
                      then []
                      else [GuardClassesDistinct leftClassId rightClassId]
                }
    HasFact factId guardTerms ->
      case FactTuple <$> traverse resolveCanonicalTerm guardTerms of
        Nothing ->
          unsatisfiedAtom
        Just factTuple ->
          let factWitness =
                FactWitness factId factTuple
              factPresent =
                hasFact factId factTuple factStore
           in GuardAtomAssessment
                { gaaSatisfied = factPresent,
                  gaaPositiveEvidence =
                    if factPresent
                      then [GuardFactPresent factWitness]
                      else [],
                  gaaNegativeEvidence =
                    if factPresent
                      then []
                      else [GuardFactAbsent factId factTuple]
                }
    HasCapability capability guardTerms ->
      case traverse resolveCanonicalTerm guardTerms of
        Nothing ->
          unsatisfiedAtom
        Just classIds ->
          let capabilityHeld =
                runGuardCapabilityResolver capabilityResolver capability classIds
           in GuardAtomAssessment
                { gaaSatisfied = capabilityHeld,
                  gaaPositiveEvidence =
                    if capabilityHeld
                      then [GuardCapabilityHeld]
                      else [],
                  gaaNegativeEvidence =
                    if capabilityHeld
                      then []
                      else [GuardCapabilityMissing]
                }
  where
    resolveCanonicalTerm guardTerm =
      canonicalizeClassId <$> resolveTerm guardTerm

-- | A guard literal grounded against a fixed match: the term resolver and
-- canonicalizer are host-determined and constant for a whole closure, so
-- every atom except a fact atom with a resolvable tuple has a permanent
-- verdict. Static literals carry their evidence exactly as the evaluator
-- would report it; fact literals carry polarity and the canonical witness
-- whose store membership alone decides their truth.
type GroundGuardLiteral :: Type
data GroundGuardLiteral
  = GroundStaticLiteral !Bool ![GuardLiteralEvidence]
  | GroundFactLiteral !Bool !FactWitness
  deriving stock (Eq, Ord, Show)

-- | Ground a compiled guard for one match. Clause order follows the CNF and
-- literal order inside each clause is ascending, mirroring the evaluator's
-- traversal so that evidence assembled from ground literals is identical to
-- 'evaluateCompiledGuardWithEvidenceAndCapabilities' at any store. Grounding
-- is store-independent: fact atoms whose tuples fail to resolve are frozen
-- with the evaluator's unresolved-atom verdict.
groundCompiledGuard ::
  GuardCapabilityResolver capability ->
  (ClassId -> ClassId) ->
  (GuardTerm f -> Maybe ClassId) ->
  CompiledGuard capability f ->
  [[GroundGuardLiteral]]
groundCompiledGuard capabilityResolver canonicalizeClassId resolveTerm =
  \case
    CompiledGuard _ clauses ->
      fmap (fmap groundLiteral . Set.toAscList) clauses
  where
    groundLiteral literal =
      case literalVariable literal of
        HasFact factId guardTerms
          | Just factTuple <-
              FactTuple <$> traverse resolveCanonicalTerm guardTerms ->
              GroundFactLiteral (literalPolarity literal) (FactWitness factId factTuple)
        _ ->
          let assessment =
                literalEvidence emptyFactStore capabilityResolver canonicalizeClassId resolveTerm literal
           in GroundStaticLiteral (glaSatisfied assessment) (glaEvidence assessment)

    resolveCanonicalTerm guardTerm =
      canonicalizeClassId <$> resolveTerm guardTerm

unsatisfiedAtom :: GuardAtomAssessment
unsatisfiedAtom =
  GuardAtomAssessment
    { gaaSatisfied = False,
      gaaPositiveEvidence = [],
      gaaNegativeEvidence = []
    }

type GuardLiteralAssessment :: Type
data GuardLiteralAssessment = GuardLiteralAssessment
  { glaSatisfied :: !Bool,
    glaEvidence :: ![GuardLiteralEvidence]
  }

type GuardAtomAssessment :: Type
data GuardAtomAssessment = GuardAtomAssessment
  { gaaSatisfied :: !Bool,
    gaaPositiveEvidence :: ![GuardLiteralEvidence],
    gaaNegativeEvidence :: ![GuardLiteralEvidence]
  }

andCompiledGuards ::
  (Ord capability, Ord (GuardTerm f)) =>
  CompiledGuard capability f ->
  CompiledGuard capability f ->
  CompiledGuard capability f
andCompiledGuards leftGuard rightGuard =
  let combinedExpr =
        And [cgNormalizedExpr leftGuard, cgNormalizedExpr rightGuard]
      normalizedExpr = normalize combinedExpr
   in CompiledGuard normalizedExpr (toCNF normalizedExpr)
