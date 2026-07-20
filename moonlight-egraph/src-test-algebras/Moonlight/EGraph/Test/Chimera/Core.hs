{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Test.Chimera.Core
  ( TissueF (..),
    TissueTag (..),
    TissueCount (..),
    Anatomy (..),
    TissueProofNote (..),
    tissueAnalysis,
    anatomyLeq,
    bone,
    keratin,
    chitin,
    cartilage,
    marrow,
    graft,
    anatomyLattice,
    fixtureChimera,
    baseTissueCost,
    anatomyCostOverlay,
    renderTissueTerm,
    renderTissueFix,
    graftCommuteRule,
    graftAssociativityRule,
    graftIdempotenceRule,
    tissueCompatibilityFactId,
    tissueCompatibilityFactRule,
    compatibleGraftReductionRule,
    tissueProofBuilder,
    allRegions,
    allPairs,
    allTriples,
    comparableTriples,
    comparableQuadruples,
    isStructuralMismatch,
    isContextBarrier,
    isRestrictionBarrier,
    isPropagationBarrier,
  )
where

import Moonlight.Core (ZipMatch (..))
import Moonlight.Core (UnionFindAllocationError)
import Data.Kind (Type)
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
  )
import Moonlight.Core (HasConstructorTag (..), ConstructorTag, zipSameNodeShape)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (ContextEGraph, withEmptyContextEGraph)
import Moonlight.EGraph.Pure.Extraction (AnalysisCostAlgebra (..), CostAlgebra (..), ExtractionResult (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.Core
  ( Pattern (..)
  )
import Moonlight.Core qualified as EGraph
import Moonlight.Rewrite.System
  ( RawRewriteRule (..)
  )
import Moonlight.Rewrite.System
  ( RewriteCondition (..),
    data GuardRoot,
    guardHasFact,
  )
import Moonlight.Rewrite.System
  ( FactRule,
    FactRuleId (..),
    RawFactRule (..),
  )
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.EGraph.Pure.Types (ClassId (..), emptyEGraph, RewriteRuleId (..))
import Moonlight.EGraph.Test.Assertions
  ( isContextBarrier,
    isPropagationBarrier,
    isRestrictionBarrier,
    isStructuralMismatch,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder (..), ProofAnnotationInput (..), ProofContextEvidence (..))
import Moonlight.Sheaf.Twist.Cost
  ( CostOverlay,
    guardedCostOverlay,
  )
import Moonlight.FiniteLattice
  ( ContextLattice (..),
    latticeContext
  )
type TissueF :: Type -> Type
data TissueF a
  = Bone
  | Keratin
  | Chitin
  | Cartilage
  | Graft a a
  | Marrow Int
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type TissueTag :: Type
data TissueTag
  = BoneTag
  | KeratinTag
  | ChitinTag
  | CartilageTag
  | GraftTag
  | MarrowTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag TissueF where
  type ConstructorTag TissueF = TissueTag
  constructorTag = \case
    Bone -> BoneTag
    Keratin -> KeratinTag
    Chitin -> ChitinTag
    Cartilage -> CartilageTag
    Marrow _ -> MarrowTag
    Graft {} -> GraftTag

instance ZipMatch TissueF where
  zipMatch = zipSameNodeShape

type TissueCount :: Type
newtype TissueCount = TissueCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice TissueCount where
  join (TissueCount leftCount) (TissueCount rightCount) =
    TissueCount (max leftCount rightCount)

tissueAnalysis :: AnalysisSpec TissueF TissueCount
tissueAnalysis =
  semilatticeAnalysis $ \case
    Bone -> TissueCount 1
    Keratin -> TissueCount 1
    Chitin -> TissueCount 1
    Cartilage -> TissueCount 1
    Marrow _ -> TissueCount 1
    Graft (TissueCount leftCount) (TissueCount rightCount) ->
      TissueCount (leftCount + rightCount + 1)

type Anatomy :: Type
data Anatomy
  = Whole
  | Skull
  | Trunk
  | Local
  deriving stock (Eq, Ord, Show, Enum, Bounded)

anatomyLeq :: Anatomy -> Anatomy -> Bool
anatomyLeq leftRegion rightRegion
  | leftRegion == rightRegion = True
  | leftRegion == Whole = True
  | rightRegion == Local = True
  | otherwise = False

instance JoinSemilattice Anatomy where
  join leftRegion rightRegion
    | anatomyLeq leftRegion rightRegion = rightRegion
    | anatomyLeq rightRegion leftRegion = leftRegion
    | otherwise = Local

instance BoundedJoinSemilattice Anatomy where
  bottom = Whole

instance MeetSemilattice Anatomy where
  meet leftRegion rightRegion
    | anatomyLeq leftRegion rightRegion = leftRegion
    | anatomyLeq rightRegion leftRegion = rightRegion
    | otherwise = Whole

instance BoundedMeetSemilattice Anatomy where
  top = Local

instance Lattice Anatomy

bone :: Fix TissueF
bone = Fix Bone

keratin :: Fix TissueF
keratin = Fix Keratin

chitin :: Fix TissueF
chitin = Fix Chitin

cartilage :: Fix TissueF
cartilage = Fix Cartilage

marrow :: Int -> Fix TissueF
marrow marrowIndex = Fix (Marrow marrowIndex)

graft :: Fix TissueF -> Fix TissueF -> Fix TissueF
graft leftStructure rightStructure = Fix (Graft leftStructure rightStructure)

anatomyLattice :: ContextLattice Anatomy
anatomyLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid Anatomy lattice fixture: " <> show compileError)

fixtureChimera ::
  (forall owner. (ClassId, ClassId, ClassId, ClassId, ContextEGraph owner TissueF TissueCount Anatomy) -> result) ->
  Either UnionFindAllocationError result
fixtureChimera useFixture = do
  let graph0 = emptyEGraph tissueAnalysis
  (boneId, graph1) <- addTerm bone graph0
  (keratinId, graph2) <- addTerm keratin graph1
  (chitinId, graph3) <- addTerm chitin graph2
  (cartilageId, graph4) <- addTerm cartilage graph3
  pure
    ( withEmptyContextEGraph anatomyLattice graph4 $ \contextGraph ->
        useFixture (boneId, keratinId, chitinId, cartilageId, contextGraph)
    )

baseTissueCost :: CostAlgebra TissueF Int
baseTissueCost =
  CostAlgebra $ \case
    Bone -> 3
    Keratin -> 2
    Chitin -> 4
    Cartilage -> 1
    Marrow _ -> 2
    Graft leftCost rightCost -> leftCost + rightCost + 1

anatomyCostOverlay :: CostOverlay Anatomy (AnalysisCostAlgebra TissueF TissueCount Int)
anatomyCostOverlay =
  guardedCostOverlay
    (== Skull)
    ( const
        ( AnalysisCostAlgebra
            ( \_ tissueNode -> case tissueNode of
                Bone -> 3
                Keratin -> 1
                Chitin -> 5
                Cartilage -> 1
                Marrow _ -> 2
                Graft (_, leftCost) (_, rightCost) -> leftCost + rightCost + 1
            )
        )
    )
    <> guardedCostOverlay
      (== Trunk)
      ( const
          ( AnalysisCostAlgebra
              ( \_ tissueNode -> case tissueNode of
                  Bone -> 1
                  Keratin -> 4
                  Chitin -> 2
                  Cartilage -> 3
                  Marrow _ -> 2
                  Graft (_, leftCost) (_, rightCost) -> leftCost + rightCost + 1
              )
          )
      )

renderTissueTerm :: ExtractionResult TissueF cost -> String
renderTissueTerm = renderTissueFix . erTerm

renderTissueFix :: Fix TissueF -> String
renderTissueFix = \case
  Fix Bone -> "bone"
  Fix Keratin -> "keratin"
  Fix Chitin -> "chitin"
  Fix Cartilage -> "cartilage"
  Fix (Marrow marrowIndex) -> "marrow(" <> show marrowIndex <> ")"
  Fix (Graft leftChild rightChild) ->
    "graft(" <> renderTissueFix leftChild <> "," <> renderTissueFix rightChild <> ")"

graftCommuteRule :: RawRewriteRule (RewriteCondition capability TissueF) TissueF
graftCommuteRule =
  tissueRewriteRule
    (RewriteRuleId 0)
    (graftPattern (tissuePatternVar 0) (tissuePatternVar 1))
    (graftPattern (tissuePatternVar 1) (tissuePatternVar 0))
    Nothing

graftAssociativityRule :: RawRewriteRule (RewriteCondition capability TissueF) TissueF
graftAssociativityRule =
  tissueRewriteRule
    (RewriteRuleId 1)
    (graftPattern (graftPattern (tissuePatternVar 0) (tissuePatternVar 1)) (tissuePatternVar 2))
    (graftPattern (tissuePatternVar 0) (graftPattern (tissuePatternVar 1) (tissuePatternVar 2)))
    Nothing

graftIdempotenceRule :: RawRewriteRule (RewriteCondition capability TissueF) TissueF
graftIdempotenceRule =
  tissueRewriteRule
    (RewriteRuleId 2)
    (graftPattern (tissuePatternVar 0) (tissuePatternVar 0))
    (tissuePatternVar 0)
    Nothing

tissueCompatibilityFactId :: FactId
tissueCompatibilityFactId =
  FactId 0

tissueCompatibilityFactRule :: FactRule capability TissueF
tissueCompatibilityFactRule =
  FactRule
    { frId = FactRuleId 0,
      frName = "chimera-tissue-compatibility",
      frPattern = compatibleGraftPattern,
      frProjection = [GuardRoot],
      frFactId = tissueCompatibilityFactId,
      frCondition = Nothing
    }

compatibleGraftReductionRule :: RawRewriteRule (RewriteCondition capability TissueF) TissueF
compatibleGraftReductionRule =
  tissueRewriteRule
    (RewriteRuleId 3)
    compatibleGraftPattern
    (PatternNode Bone)
    (Just (RewriteCondition (guardHasFact tissueCompatibilityFactId [GuardRoot])))

tissueRewriteRule ::
  RewriteRuleId ->
  Pattern TissueF ->
  Pattern TissueF ->
  Maybe (RewriteCondition capability TissueF) ->
  RawRewriteRule (RewriteCondition capability TissueF) TissueF
tissueRewriteRule rewriteRuleId lhsPattern rhsPattern rewriteCondition =
  RawRewriteRule
    { rrId = rewriteRuleId,
      rrLhs = lhsPattern,
      rrRhs = rhsPattern,
      rrCondition = rewriteCondition,
      rrApplicationCondition = Nothing,
      rrPostSubst = Nothing
    }

tissuePatternVar :: Int -> Pattern TissueF
tissuePatternVar =
  PatternVar . EGraph.mkPatternVar

graftPattern :: Pattern TissueF -> Pattern TissueF -> Pattern TissueF
graftPattern leftPattern rightPattern =
  PatternNode (Graft leftPattern rightPattern)

compatibleGraftPattern :: Pattern TissueF
compatibleGraftPattern =
  graftPattern (tissuePatternVar 0) (PatternNode Cartilage)

type TissueProofNote :: Type
data TissueProofNote = TissueProofNote
  { tpnRuleId :: RewriteRuleId,
    tpnActiveContext :: Maybe Anatomy,
    tpnHasRestrictions :: Bool
  }
  deriving stock (Eq, Show)

tissueProofBuilder :: ProofAnnotationBuilder Anatomy TissueProofNote
tissueProofBuilder =
  ProofAnnotationBuilder $ \proofAnnotationInput ->
    TissueProofNote
      { tpnRuleId = paiRewriteRuleId proofAnnotationInput,
        tpnActiveContext = pceActiveContext =<< paiContextEvidence proofAnnotationInput,
        tpnHasRestrictions = maybe False (not . null . pceRestrictions) (paiContextEvidence proofAnnotationInput)
      }

allRegions :: [Anatomy]
allRegions = [minBound .. maxBound]

allPairs :: [(Anatomy, Anatomy)]
allPairs = [(leftRegion, rightRegion) | leftRegion <- allRegions, rightRegion <- allRegions]

allTriples :: [(Anatomy, Anatomy, Anatomy)]
allTriples = [(a, b, c) | a <- allRegions, b <- allRegions, c <- allRegions]

comparableTriples :: [(Anatomy, Anatomy, Anatomy)]
comparableTriples =
  [(a, b, c) | a <- allRegions, b <- allRegions, c <- allRegions,
   anatomyLeq c b && anatomyLeq b a]

comparableQuadruples :: [(Anatomy, Anatomy, Anatomy, Anatomy)]
comparableQuadruples =
  [(a, b, c, d) | a <- allRegions, b <- allRegions, c <- allRegions, d <- allRegions,
   anatomyLeq d c && anatomyLeq c b && anatomyLeq b a]
