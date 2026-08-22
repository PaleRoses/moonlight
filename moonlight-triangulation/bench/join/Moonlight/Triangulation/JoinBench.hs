{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | What the join costs, and against what.
--
-- These lanes compare the public union schedules with rebuild, local insertion,
-- and explicit canonical observation. Schedule claims live or die by these
-- measurements rather than by asymptotic theatre.
module Moonlight.Triangulation.JoinBench
  ( benchmarks
  , publicationBenchmarks
  ) where

import BenchSupport (randomPoints, requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, unless)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.BulkLoad (insertMany)
import Moonlight.Triangulation.Internal.PointIndex (lookupPointIndex)
import Moonlight.Triangulation.Internal.Representation
  ( Triangulation (triPointIndex, triPointX, triPointY)
  )
import System.Mem (performGC)

type Mesh = DelaunayTriangulation ()
type SiteMesh = DelaunayTriangulation (Point)

benchmarks :: IO ()
benchmarks = do
  benchmarkBalanced 20_000
  benchmarkSeparated 20_000
  benchmarkSkew 20_000 200
  benchmarkOverlap 20_000
  benchmarkTournament 20_000 16
  benchmarkSpatialTournament 20_000 16
  benchmarkTournamentScaling 20_000
  benchmarkCanonicalize 20_000
  benchmarkSetAlgebra 20_000

publicationBenchmarks :: IO ()
publicationBenchmarks = do
  benchmarkIndexedSupportContexts 1_000_000 5_000
  benchmarkPersistentSetOperations 1_000_000 5_000
  benchmarkPersistentPublication 5_000_000 5_000

benchmarkIndexedSupportContexts :: Int -> Int -> IO ()
benchmarkIndexedSupportContexts baseCount deltaCount = do
  let baseSites = randomPoints 0xcbbb9d5dc1059ed8 baseCount
      retainedSites = drop deltaCount baseSites
  benchmarkColdRelationWarmIntersection baseSites retainedSites
  performGC
  benchmarkColdIntersectionWarmRelation baseSites retainedSites
  performGC

benchmarkColdRelationWarmIntersection :: [Point] -> [Point] -> IO ()
benchmarkColdRelationWarmIntersection baseSites retainedSites = do
  base <- geometryMesh baseSites
  retained <- geometryMesh retainedSites
  _ <- evaluate (force (base, retained))
  relation <- timedValue "set-relation-near-full-cold-index" (evaluate (force (siteRelation base retained)))
  unless (relation == RightProperSubset) $
    fail "cold indexed relation misclassified the retained operand"
  forceExactPointIndex base baseSites
  benchmarkIndexedIntersection "set-intersection-near-full-warm-index" base retained

benchmarkColdIntersectionWarmRelation :: [Point] -> [Point] -> IO ()
benchmarkColdIntersectionWarmRelation baseSites retainedSites = do
  base <- geometryMesh baseSites
  retained <- geometryMesh retainedSites
  _ <- evaluate (force (base, retained))
  benchmarkIndexedIntersection "set-intersection-near-full-cold-index" base retained
  forceExactPointIndex base baseSites
  relation <- timedValue "set-relation-near-full-warm-index" (evaluate (force (siteRelation base retained)))
  unless (relation == RightProperSubset) $
    fail "warm indexed relation misclassified the retained operand"

benchmarkIndexedIntersection :: String -> Mesh -> Mesh -> IO ()
benchmarkIndexedIntersection label base retained = do
  result <- benchmarkValidatedSetOperation label (intersection base retained)
  observedCanonical <- evaluate . force =<< requireRight (canonicalize result)
  expectedCanonical <- evaluate . force =<< requireRight (canonicalize retained)
  unless (observedCanonical == expectedCanonical) $
    fail (label <> " disagreed with the retained operand")

forceExactPointIndex :: Mesh -> [Point] -> IO ()
forceExactPointIndex triangulation points =
  case points of
    [] -> fail "cannot warm a point index without a witness"
    witness : _ ->
      case
          lookupPointIndex
            (triPointX triangulation)
            (triPointY triangulation)
            (triPointIndex triangulation)
            witness
        of
          Nothing -> fail "point-index warmup missed its exact witness"
          Just vertex -> () <$ evaluate (force vertex)

-- | Two halves of one point set, joined.
--
-- The balanced pair rebuilds from its combined site set. The input-order and
-- ranked lanes distinguish ordinary construction from construction whose
-- vertex numbering is already canonical.
benchmarkBalanced :: Int -> IO ()
benchmarkBalanced total = do
  let sites = randomPoints 0x9e3779b97f4a7c15 total
      (left, right) = splitAt (total `div` 2) sites
  leftMesh <- geometryMesh left
  rightMesh <- geometryMesh right
  _ <- evaluate (force (leftMesh, rightMesh))
  _ <- timedValue "join-balanced" (evaluate . force =<< requireRight (union leftMesh rightMesh))
  _ <- timedValue "join-balanced-rebuild-input-order" (evaluate . force =<< geometryMesh sites)
  _ <- timedValue "join-balanced-rebuild-ranked-order" (evaluate . force =<< geometryMesh (canonical sites))
  pure ()

-- | Two operands whose sites are separated by a vertical line.
--
-- This is the stratum a seam merge is defined on, and the number here is the
-- one it has to beat: the reference schedule does not know the operands are
-- separated and rebuilds the union regardless. A linear-time merge wins
-- asymptotically over an @O(n log n)@ rebuild; whether it wins at the sizes
-- anything actually merges at is this measurement and not an argument.
--
-- Three gap widths, because the seam's work is the cross-edge chain and the
-- deletions it drives, and how far the two clouds stand apart decides how much
-- of each interior the chain disturbs. A distant pair is the easy case — the
-- chain is short and nothing inside either operand dies. An abutting pair is
-- the hard one.
benchmarkSeparated :: Int -> IO ()
benchmarkSeparated total = do
  let half = total `div` 2
      sites = randomPoints 0xd1b54a32d192ed03 half
      extent = 2 * maximum [abs x | Point x _ <- sites]
  leftMesh <- geometryMesh sites
  _ <- evaluate (force leftMesh)
  mapM_
    ( \(name, gap) -> do
        let shifted = [Point (x + gap * extent) y | Point x y <- sites]
        rightMesh <- geometryMesh shifted
        _ <- evaluate (force rightMesh)
        _ <- timedValue ("join-separated-" <> name) (evaluate . force =<< requireRight (union leftMesh rightMesh))
        pure ()
    )
    [("distant" :: String, 8), ("near", 2), ("abutting", 1.02)]

-- | A large mesh joined with a small one. Rebuilding costs the whole union;
-- inserting the small operand's sites into the large mesh costs only the
-- insertions. This is the ratio that says whether a skewed lane is worth
-- having, and it needs no new algorithm — 'insertMany' is already the
-- one-transaction batch path.
--
-- Both lanes carry the same vertex payload so the comparison is of the
-- schedules and not of the stores.
benchmarkSkew :: Int -> Int -> IO ()
benchmarkSkew large small = do
  let bulk = randomPoints 0xbf58476d1ce4e5b9 large
      addition = randomPoints 0x94d049bb133111eb small
  bulkMesh <- siteMesh bulk
  _ <- evaluate (force bulkMesh)
  _ <-
    timedValue
      "join-skew-rebuild"
      (evaluate . force =<< siteMesh (canonical (bulk <> addition)))
  _ <-
    timedValue
      "join-skew-insert-many"
      ( evaluate . force . buildTriangulation
          =<< requireRight (insertMany bulkMesh (V.fromList addition))
      )
  pure ()

benchmarkPersistentPublication :: Int -> Int -> IO ()
benchmarkPersistentPublication baseCount extensionCount = do
  let baseSites = randomPoints 0x6a09e667f3bcc909 baseCount
      extensionSites =
        fmap
          (\(Point x y) -> Point (1.2 + 0.1 * x) y)
          (randomPoints 0xbb67ae8584caa73b extensionCount)
  base <- geometryMesh baseSites
  extension <- geometryMesh extensionSites
  _ <- evaluate (force (base, extension))
  putStrLn ("publication-base-sites: " <> show (numVertices base))
  putStrLn ("publication-base-faces: " <> show (numFaces base))
  putStrLn ("publication-extension-sites: " <> show (numVertices extension))
  putStrLn ("publication-extension-faces: " <> show (numFaces extension))
  joined <-
    timedValue
      "publication-skew-union"
      (evaluate . force =<< requireRight (union base extension))
  case validateTriangulation joined of
    [] -> pure ()
    violations -> fail ("publication-skew-union invalid: " <> show violations)
  canonicalResult <-
    timedValue
      "publication-explicit-canonicalize"
      (evaluate . force =<< requireRight (canonicalize joined))
  unless (numVertices canonicalResult == numVertices joined) $
    fail "publication canonicalization changed the site count"
  putStrLn ("publication-result-sites: " <> show (numVertices joined))
  putStrLn ("publication-result-faces: " <> show (numFaces joined))

benchmarkPersistentSetOperations :: Int -> Int -> IO ()
benchmarkPersistentSetOperations baseCount deltaCount = do
  let baseSites = randomPoints 0xcbbb9d5dc1059ed8 baseCount
      removedSites = take deltaCount baseSites
      retainedSites = drop deltaCount baseSites
      extensionSites =
        fmap
          (\(Point x y) -> Point (1.2 + 0.1 * x) y)
          (randomPoints 0x629a292a367cd507 deltaCount)
  base <- geometryMesh baseSites
  removed <- geometryMesh removedSites
  retained <- geometryMesh retainedSites
  extension <- geometryMesh extensionSites
  empty <- requireRight (unions [])
  _ <- evaluate (force (base, removed, retained, extension, empty))
  putStrLn ("set-publication-base-sites: " <> show (numVertices base))
  putStrLn ("set-publication-delta-sites: " <> show (numVertices removed))
  _ <- benchmarkValidatedSetOperation "set-publication-difference-right-empty" (difference base empty)
  _ <- benchmarkValidatedSetOperation "set-publication-symmetric-difference-left-empty" (symmetricDifference empty base)
  _ <- benchmarkValidatedSetOperation "set-publication-symmetric-difference-right-empty" (symmetricDifference base empty)
  benchmarkPublishedSetOperation
    "set-publication-difference-skew"
    (difference base removed)
    (pure retained)
  benchmarkPublishedSetOperation
    "set-publication-intersection-skew"
    (intersection base retained)
    (pure retained)
  benchmarkPublishedSetOperation
    "set-publication-symmetric-difference-disjoint-skew"
    (symmetricDifference base extension)
    (geometryMesh (baseSites <> extensionSites))
  benchmarkPublishedSetOperation
    "set-publication-symmetric-difference-small-output"
    (symmetricDifference base retained)
    (pure removed)

benchmarkPublishedSetOperation :: String -> Either BuildError Mesh -> IO Mesh -> IO ()
benchmarkPublishedSetOperation label operation expectedWitness = do
  result <- benchmarkValidatedSetOperation label operation
  observedCanonical <-
    timedValue
      (label <> "-explicit-canonicalize")
      (evaluate . force =<< requireRight (canonicalize result))
  expected <- expectedWitness
  expectedCanonical <- evaluate . force =<< requireRight (canonicalize expected)
  unless (observedCanonical == expectedCanonical) $
    fail (label <> " disagreed with the independently rebuilt witness")

benchmarkValidatedSetOperation :: String -> Either BuildError Mesh -> IO Mesh
benchmarkValidatedSetOperation label operation = do
  result <- timedValue label (evaluate . force =<< requireRight operation)
  case validateTriangulation result of
    [] -> pure ()
    violations -> fail (label <> " invalid: " <> show violations)
  pure result

-- | The same operand sizes at three overlap fractions. A join is sized by the
-- union, so wholly overlapping operands must cost what one of them costs.
benchmarkOverlap :: Int -> IO ()
benchmarkOverlap total = do
  let sites = randomPoints 0x2545f4914f6cdd1d total
      half = total `div` 2
  disjointLeft <- geometryMesh (take half sites)
  disjointRight <- geometryMesh (drop half sites)
  halfLeft <- geometryMesh (take half sites)
  halfRight <- geometryMesh (drop (half `div` 2) (take (half + half `div` 2) sites))
  sameLeft <- geometryMesh (take half sites)
  sameRight <- geometryMesh (reverse (take half sites))
  _ <- evaluate (force (disjointLeft, disjointRight, halfLeft, halfRight, sameLeft, sameRight))
  _ <- timedValue "join-overlap-000" (evaluate . force =<< requireRight (union disjointLeft disjointRight))
  _ <- timedValue "join-overlap-050" (evaluate . force =<< requireRight (union halfLeft halfRight))
  _ <- timedValue "join-overlap-100" (evaluate . force =<< requireRight (union sameLeft sameRight))
  pure ()

-- | 'unions' is a balanced tournament and not a fold, which is a cost claim
-- and therefore has to be measured rather than asserted. A fold republishes an
-- accumulator that grows by one shard per step.
--
-- This lane once reported the fold as the faster of the two, which was true and
-- was not a fact about the schedules: @joinBalanced@ carried no specialization,
-- so every join inside the tournament ran through a dictionary while
-- the left-associated schedule at a known element type ran specialized. The tournament was
-- paying twice for arithmetic, and that swamped the asymptotic gap it was
-- supposed to be demonstrating.
--
-- The shards are dealt round-robin, so every one of them spans the whole extent
-- and no join in the tournament is separable. That is deliberate: it is the
-- adversarial sharding, and it measures the operator with no structure to
-- exploit. 'benchmarkSpatialTournament' is the same tournament over the
-- sharding a caller who wanted it to be fast would actually choose.
benchmarkTournament :: Int -> Int -> IO ()
benchmarkTournament total shardCount = do
  let sites = randomPoints 0x14057b7ef767814f total
      indexed = zip [0 :: Int ..] sites
  shards <-
    traverse
      (\shard -> geometryMesh [site | (index, site) <- indexed, index `mod` shardCount == shard])
      [0 .. shardCount - 1]
  _ <- evaluate (force shards)
  _ <- timedValue "join-tournament" (evaluate . force =<< requireRight (unions shards))
  _ <- timedValue "join-left-fold" (evaluate . force =<< requireRight (unionsLeftAssociated shards))
  pure ()

-- | Where the tournament's advantage over the fold actually appears.
--
-- A fold republishes an accumulator that grows by one shard per step, so it
-- rebuilds @Θ(nk)@ sites over @k@ shards where halving rebuilds @Θ(n log k)@.
-- That is a statement about @k@, and at the sixteen shards the lane above uses
-- the predicted factor is barely two — small enough to be swamped by the
-- per-join costs both schedules pay fifteen times each. This sweep is here
-- because a cost claim that only holds asymptotically has to say at what size
-- it starts holding, and the answer has to be measured rather than asserted.
benchmarkTournamentScaling :: Int -> IO ()
benchmarkTournamentScaling total =
  mapM_
    ( \shardCount -> do
        let sites = randomPoints 0x9e3779b97f4a7c15 total
            indexed = zip [0 :: Int ..] sites
        shards <-
          traverse
            (\shard -> geometryMesh [site | (index, site) <- indexed, index `mod` shardCount == shard])
            [0 .. shardCount - 1]
        _ <- evaluate (force shards)
        _ <- timedValue ("join-shards-" <> show shardCount <> "-tournament") (evaluate . force =<< requireRight (unions shards))
        _ <- timedValue ("join-shards-" <> show shardCount <> "-fold") (evaluate . force =<< requireRight (unionsLeftAssociated shards))
        pure ()
    )
    [4 :: Int, 16, 64]


-- | The same tournament over shards cut by abscissa rather than dealt.
--
-- This is the workload a seam schedule exists for, and the only one where it
-- can pay off more than once. Shards cut into contiguous x-ranges are pairwise
-- separated; so is every intermediate result, because the union of two adjacent
-- ranges is a range. Every one of the fifteen joins in the tournament is
-- therefore separable — the tournament /is/ the divide-and-conquer recursion,
-- entered from the leaves.
--
-- Against the reference schedule this must cost about what the dealt
-- tournament costs, since a rebuild cannot tell the two shardings apart. That
-- agreement is the baseline; the gap that opens between these two lanes is the
-- whole return on a merge kernel.
benchmarkSpatialTournament :: Int -> Int -> IO ()
benchmarkSpatialTournament total shardCount = do
  let sites = randomPoints 0x3c6ef372fe94f82a total
      ordered = sortBy (comparing (\(Point x _) -> x)) sites
      width = (total + shardCount - 1) `div` shardCount
  shards <-
    traverse
      (\shard -> geometryMesh (take width (drop (shard * width) ordered)))
      [0 .. shardCount - 1]
  _ <- evaluate (force shards)
  _ <- timedValue "join-spatial-tournament" (evaluate . force =<< requireRight (unions shards))
  pure ()

-- | The renumbering pass on its own, against the construction it follows.
benchmarkCanonicalize :: Int -> IO ()
benchmarkCanonicalize total = do
  let sites = randomPoints 0x27d4eb2f165667c5 total
  mesh <- geometryMesh sites
  _ <- evaluate (force mesh)
  _ <- timedValue "canonicalize-alone" (evaluate (force (canonicalize mesh)))
  pure ()

-- | The shared canonical rebuild boundary under half overlap, on both its
-- geometry-only specializations and its annotation-preserving surface. Setup
-- and source publication are forced before every clock; the measurements are
-- therefore the exact site classification, rebuild, canonical publication and
-- payload transport the public operations own.
benchmarkSetAlgebra :: Int -> IO ()
benchmarkSetAlgebra total = do
  let common = total `quot` 2
      sites = randomPoints 0x6A09E667F3BCC909 (total + common)
      leftPoints = take total sites
      rightPoints = drop common sites
  leftGeometry <- geometryMesh leftPoints
  rightGeometry <- geometryMesh rightPoints
  emptyGeometry <- requireRight (unions [])
  _ <- evaluate (force (leftGeometry, rightGeometry, emptyGeometry))
  _ <- timedValue "set-intersection-unit" (evaluate . force =<< requireRight (intersection leftGeometry rightGeometry))
  _ <- timedValue "set-difference-unit" (evaluate . force =<< requireRight (difference leftGeometry rightGeometry))
  _ <-
    timedValue
      "set-symmetric-difference-unit"
      (evaluate . force =<< requireRight (symmetricDifference leftGeometry rightGeometry))
  _ <-
    timedValue
      "set-difference-right-empty"
      (evaluate . force =<< requireRight (difference leftGeometry emptyGeometry))
  _ <-
    timedValue
      "set-symmetric-difference-left-empty"
      (evaluate . force =<< requireRight (symmetricDifference emptyGeometry leftGeometry))
  _ <- timedValue "set-relation-half-overlap" (evaluate (force (siteRelation leftGeometry rightGeometry)))
  leftAnnotated <- siteMesh leftPoints
  rightAnnotated <- siteMesh rightPoints
  _ <- evaluate (force (leftAnnotated, rightAnnotated))
  _ <-
    timedValue
      "set-intersection-annotated"
      (evaluate . force =<< requireRight (intersectionWith (,) leftAnnotated rightAnnotated))
  _ <-
    timedValue
      "set-difference-annotated"
      (evaluate . force =<< requireRight (difference leftAnnotated rightAnnotated))
  _ <-
    timedValue
      "set-symmetric-difference-annotated"
      (evaluate . force =<< requireRight (symmetricDifference leftAnnotated rightAnnotated))
  pure ()


canonical :: [Point] -> [Point]
canonical points = [Point x y | (x, y) <- dropAdjacentDuplicates (sort [(x, y) | Point x y <- points])]

dropAdjacentDuplicates :: Eq a => [a] -> [a]
dropAdjacentDuplicates (first : second : rest)
  | first == second = dropAdjacentDuplicates (second : rest)
  | otherwise = first : dropAdjacentDuplicates (second : rest)
dropAdjacentDuplicates rest = rest

geometryMesh :: [Point] -> IO Mesh
geometryMesh points = requireRight (delaunayGeometry (V.fromList points))

siteMesh :: [Point] -> IO SiteMesh
siteMesh points =
  buildTriangulation <$> requireRight (delaunay unitElementDefaults (V.fromList points))

unionsLeftAssociated :: [Mesh] -> Either BuildError Mesh
unionsLeftAssociated meshes = unions [] >>= \identity -> foldM union identity meshes
