{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  ) where

import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (intercalate, sort, subsequences)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.Category (FinObjectId (..))
import Fixture
  ( BenchmarkChecksum (..)
  , BenchmarkResult
  , benchmarkSuccess
  , checksumDerivedGF2
  , checksumPoset
  , forceChecksum
  )
import Moonlight.Derived.Complex
  ( Derived
  )
import Moonlight.Derived.Functor
  ( closedSupportResolution
  , mkClosedSupport
  , PreparedProperPullback
  , prepareProperPullback
  , properPullback
  , pullback
  , pushforward
  )
import Moonlight.Derived.Morse (hypercohomologyDims)
import Moonlight.Derived.Site
  ( DerivedPoset
  , DerivedPosetFunctor
  , LocalClosed
  , derivedPosetCoversUp
  , derivedPosetNodes
  , localClosedNodes
  , memberOfDerivedPoset
  , mkLocalClosed
  , mkDerivedPosetFromOrderEdges
  , mkDerivedPosetFunctor
  )
import Moonlight.LinAlg (GF2)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import Text.Read (readMaybe)

data AnchorOptions = AnchorOptions
  { aoCsvFile :: !(Maybe FilePath)
  , aoManifestFile :: !(Maybe FilePath)
  }

data AnchorFixture = AnchorFixture
  { afId :: !String
  , afSourceNodes :: ![FinObjectId]
  , afSourceCovers :: ![(FinObjectId, FinObjectId)]
  , afTargetNodes :: ![FinObjectId]
  , afTargetCovers :: ![(FinObjectId, FinObjectId)]
  , afMapEntries :: !(IntMap FinObjectId)
  , afSupportSet :: !IntSet
  , afExpectedSemanticSignature :: ![Int]
  }

data AnchorOperation = AnchorOperation
  { opId :: !String
  , opRun :: AnchorPreparedFixture -> BenchmarkResult
  }

data AnchorPreparedFixture = AnchorPreparedFixture
  { apFixture :: !AnchorFixture
  , apSourcePoset :: !DerivedPoset
  , apTargetPoset :: !DerivedPoset
  , apSourceDerived :: !(Derived GF2)
  , apTargetDerived :: !(Derived GF2)
  , apFunctor :: !DerivedPosetFunctor
  , apWitnessSupport :: !LocalClosed
  , apPreparedProperPullback :: !(PreparedProperPullback GF2)
  }

data AnchorRow = AnchorRow
  { arEngine :: !String
  , arOperation :: !String
  , arFixture :: !String
  , arStatus :: !String
  , arElapsedNanoseconds :: !Word64
  , arElapsedMilliseconds :: !Word64
  , arChecksum :: !(Maybe Int)
  , arMessage :: !String
  }

main :: IO ()
main = do
  options <- getArgs >>= either die pure . parseArgs
  manifestFile <- maybe (die "moonlight-derived-desc-anchor: --manifest is required") pure (aoManifestFile options)
  anchorFixtures <- readFile manifestFile >>= either die pure . parseAnchorManifest
  preparedFixtures <- either die pure (traverse prepareAnchorFixture anchorFixtures)
  _ <- traverse evaluatePreparedFixture preparedFixtures
  rows <- traverse (uncurry measureAnchorOperation) (anchorWork preparedFixtures)
  let renderedCsv = renderAnchorCsv rows
  putStr renderedCsv
  traverse_ (\csvFile -> writeFile csvFile renderedCsv) (aoCsvFile options)
  if all ((== "success") . arStatus) rows then pure () else exitFailure

anchorWork :: [AnchorPreparedFixture] -> [(AnchorOperation, AnchorPreparedFixture)]
anchorWork preparedFixtures =
  [ (operationValue, fixtureValue)
  | operationValue <- anchorOperations
  , fixtureValue <- preparedFixtures
  ]

anchorOperations :: [AnchorOperation]
anchorOperations =
  [ AnchorOperation "constant-resolution" runConstantResolution
  , AnchorOperation "pushforward" runPushforward
  , AnchorOperation "pullback" runPullback
  , AnchorOperation "proper-pullback" runProperPullback
  , AnchorOperation "hypercohomology" runHypercohomology
  ]

prepareAnchorFixture :: AnchorFixture -> Either String AnchorPreparedFixture
prepareAnchorFixture fixture = do
  sourcePoset <- sourcePosetFromFixture fixture
  targetPoset <- targetPosetFromFixture fixture
  mapToTarget <- anchorMapFromFixture fixture sourcePoset targetPoset
  validateAnchorFixture fixture sourcePoset targetPoset mapToTarget
  functorValue <-
    firstShow
      ( mkDerivedPosetFunctor
          sourcePoset
          targetPoset
          ( Map.fromList
              [ (FinObjectId sourceKey, FinObjectId targetKey)
              | sourceNode@(FinObjectId sourceKey) <- afSourceNodes fixture
              , let FinObjectId targetKey = mapToTarget sourceNode
              ]
          )
      )
  sourceDerived <- resolutionOn sourcePoset (fullSourceSupportSet fixture)
  targetDerived <- resolutionOn targetPoset (fullTargetSupportSet fixture)
  witnessSupport <- firstShow (mkLocalClosed sourcePoset (witnessSupportSet fixture))
  preparedProperPullback <- firstShow (prepareProperPullback witnessSupport sourceDerived)
  pure
    AnchorPreparedFixture
      { apFixture = fixture
      , apSourcePoset = sourcePoset
      , apTargetPoset = targetPoset
      , apSourceDerived = sourceDerived
      , apTargetDerived = targetDerived
      , apFunctor = functorValue
      , apWitnessSupport = witnessSupport
      , apPreparedProperPullback = preparedProperPullback
      }

evaluatePreparedFixture :: AnchorPreparedFixture -> IO Int
evaluatePreparedFixture AnchorPreparedFixture {apSourcePoset, apTargetPoset, apSourceDerived, apTargetDerived} =
  evaluate
    ( forceChecksum (checksumPoset apSourcePoset)
        + forceChecksum (checksumPoset apTargetPoset)
        + forceChecksum (checksumDerivedGF2 apSourceDerived)
        + forceChecksum (checksumDerivedGF2 apTargetDerived)
    )

measureAnchorOperation :: AnchorOperation -> AnchorPreparedFixture -> IO AnchorRow
measureAnchorOperation AnchorOperation {opId, opRun} preparedFixture = do
  startNanoseconds <- getMonotonicTimeNSec
  result <- evaluateBenchmarkResult (opRun preparedFixture)
  endNanoseconds <- getMonotonicTimeNSec
  let fixture = apFixture preparedFixture
      elapsedNanosecondsValue = elapsedNanoseconds startNanoseconds endNanoseconds
  pure
    ( case result of
        Left failureMessage ->
          AnchorRow
            { arEngine = "moonlight-derived"
            , arOperation = opId
            , arFixture = afId fixture
            , arStatus = "failure"
            , arElapsedNanoseconds = elapsedNanosecondsValue
            , arElapsedMilliseconds = nanosecondsToMilliseconds elapsedNanosecondsValue
            , arChecksum = Nothing
            , arMessage = failureMessage
            }
        Right checksumValue ->
          AnchorRow
            { arEngine = "moonlight-derived"
            , arOperation = opId
            , arFixture = afId fixture
            , arStatus = "success"
            , arElapsedNanoseconds = elapsedNanosecondsValue
            , arElapsedMilliseconds = nanosecondsToMilliseconds elapsedNanosecondsValue
            , arChecksum = Just checksumValue
            , arMessage = ""
            }
    )

evaluateBenchmarkResult :: BenchmarkResult -> IO BenchmarkResult
evaluateBenchmarkResult result =
  evaluate (forceBenchmarkResult result) *> pure result

forceBenchmarkResult :: BenchmarkResult -> Int
forceBenchmarkResult result =
  case result of
    Left failureMessage ->
      length failureMessage
    Right checksumValue ->
      checksumValue

elapsedNanoseconds :: Word64 -> Word64 -> Word64
elapsedNanoseconds startNanoseconds endNanoseconds =
  endNanoseconds - startNanoseconds

nanosecondsToMilliseconds :: Word64 -> Word64
nanosecondsToMilliseconds nanoseconds =
  nanoseconds `div` 1000000

runConstantResolution :: AnchorPreparedFixture -> BenchmarkResult
runConstantResolution AnchorPreparedFixture {apFixture, apSourcePoset} =
  benchmarkEitherWith
    checksumDerivedGF2
    (resolutionOn apSourcePoset (fullSourceSupportSet apFixture))

runPushforward :: AnchorPreparedFixture -> BenchmarkResult
runPushforward AnchorPreparedFixture {apSourceDerived, apFunctor} =
  benchmarkEitherWith checksumDerivedGF2 $ do
    firstShow
      (pushforward apFunctor apSourceDerived)

runPullback :: AnchorPreparedFixture -> BenchmarkResult
runPullback AnchorPreparedFixture {apTargetDerived, apFunctor} =
  benchmarkEitherWith checksumDerivedGF2 $ do
    firstShow
      (pullback apFunctor apTargetDerived)

runProperPullback :: AnchorPreparedFixture -> BenchmarkResult
runProperPullback AnchorPreparedFixture {apWitnessSupport, apPreparedProperPullback} =
  benchmarkSuccess
    ( checksumSupportedDerived
        (localClosedNodes apWitnessSupport)
        (properPullback apPreparedProperPullback)
    )

runHypercohomology :: AnchorPreparedFixture -> BenchmarkResult
runHypercohomology AnchorPreparedFixture {apWitnessSupport, apPreparedProperPullback} =
  benchmarkEitherWith (checksumSupportedDimensions (localClosedNodes apWitnessSupport)) $ do
    firstShow
      ( hypercohomologyDims
          (properPullback apPreparedProperPullback)
      )

resolutionOn :: DerivedPoset -> IntSet -> Either String (Derived GF2)
resolutionOn poset support =
  firstShow (mkClosedSupport poset support >>= closedSupportResolution)

sourcePosetFromFixture :: AnchorFixture -> Either String DerivedPoset
sourcePosetFromFixture fixture =
  firstShow (mkDerivedPosetFromOrderEdges (afSourceNodes fixture) (afSourceCovers fixture))

targetPosetFromFixture :: AnchorFixture -> Either String DerivedPoset
targetPosetFromFixture fixture =
  firstShow (mkDerivedPosetFromOrderEdges (afTargetNodes fixture) (afTargetCovers fixture))

chainFixture :: String -> Int -> Int -> Int -> [Int] -> AnchorFixture
chainFixture fixtureId sourceNodeCount targetNodeCount supportNodeCount expectedSignature =
  AnchorFixture
    { afId = fixtureId
    , afSourceNodes = sourceNodes
    , afSourceCovers = chainCovers sourceNodes
    , afTargetNodes = targetNodes
    , afTargetCovers = chainCovers targetNodes
    , afMapEntries =
        IntMap.fromList
          [ (sourceKey, FinObjectId (min (targetNodeCount - 1) ((sourceKey * targetNodeCount) `div` sourceNodeCount)))
          | sourceKey <- [0 .. sourceNodeCount - 1]
          ]
    , afSupportSet = IntSet.fromAscList [0 .. max 0 supportNodeCount - 1]
    , afExpectedSemanticSignature = expectedSignature
    }
  where
    sourceNodes = fmap FinObjectId [0 .. sourceNodeCount - 1]
    targetNodes = fmap FinObjectId [0 .. targetNodeCount - 1]

chainCovers :: [FinObjectId] -> [(FinObjectId, FinObjectId)]
chainCovers nodes =
  zip nodes (drop 1 nodes)

simplicialFixture :: String -> Int -> Int -> Int -> [Int] -> AnchorFixture
simplicialFixture fixtureId sourceVertexCount targetVertexCount supportVertexCount expectedSignature =
  AnchorFixture
    { afId = fixtureId
    , afSourceNodes = sourceNodes
    , afSourceCovers = simplicialCovers sourceFaces
    , afTargetNodes = targetNodes
    , afTargetCovers = simplicialCovers targetFaces
    , afMapEntries =
        IntMap.fromList
          [ (unFinObjectId (simplexNode sourceFace), simplexNode (imageFace sourceFace))
          | sourceFace <- sourceFaces
          ]
    , afSupportSet =
        IntSet.fromAscList
          [ unFinObjectId (simplexNode [vertexKey])
          | vertexKey <- [0 .. max 0 supportVertexCount - 1]
          ]
    , afExpectedSemanticSignature = expectedSignature
    }
  where
    sourceFaces = facesFromFacets (pathTriangleFacets sourceVertexCount)
    targetFaces = facesFromFacets (pathEdgeFacets targetVertexCount)
    sourceNodes = fmap simplexNode sourceFaces
    targetNodes = fmap simplexNode targetFaces
    imageFace =
      Set.toAscList
        . Set.fromList
        . fmap
          ( \sourceVertex ->
              min
                (targetVertexCount - 1)
                ((sourceVertex * targetVertexCount) `div` sourceVertexCount)
          )

pathTriangleFacets :: Int -> [[Int]]
pathTriangleFacets vertexCount =
  [ [vertexKey, vertexKey + 1, vertexKey + 2]
  | vertexKey <- [0 .. max 0 (vertexCount - 3)]
  ]

pathEdgeFacets :: Int -> [[Int]]
pathEdgeFacets vertexCount =
  [ [vertexKey, vertexKey + 1]
  | vertexKey <- [0 .. max 0 (vertexCount - 2)]
  ]

facesFromFacets :: [[Int]] -> [[Int]]
facesFromFacets =
  Set.toAscList
    . Set.fromList
    . concatMap (filter (not . null) . subsequences)

simplicialCovers :: [[Int]] -> [(FinObjectId, FinObjectId)]
simplicialCovers faces =
  [ (simplexNode face, simplexNode coface)
  | face <- faces
  , coface <- faces
  , length coface == length face + 1
  , face `isSubfaceOf` coface
  ]

isSubfaceOf :: [Int] -> [Int] -> Bool
isSubfaceOf face coface =
  all (`elem` coface) face

simplexNode :: [Int] -> FinObjectId
simplexNode =
  FinObjectId . sum . fmap (2 ^)

anchorMapFromFixture :: AnchorFixture -> DerivedPoset -> DerivedPoset -> Either String (FinObjectId -> FinObjectId)
anchorMapFromFixture AnchorFixture {afSourceNodes, afMapEntries} sourcePoset targetPoset =
  traverse validateMappedNode afSourceNodes
    *> Right (\sourceNode@(FinObjectId sourceKey) -> IntMap.findWithDefault sourceNode sourceKey afMapEntries)
  where
    validateMappedNode sourceNode@(FinObjectId sourceKey) =
      case IntMap.lookup sourceKey afMapEntries of
        Nothing ->
          Left ("missing anchor map entry for source node " <> show sourceNode)
        Just targetNode ->
          if memberOfDerivedPoset sourcePoset sourceNode && memberOfDerivedPoset targetPoset targetNode
            then Right ()
            else Left ("anchor map sends " <> show sourceNode <> " outside target poset: " <> show targetNode)

witnessSupportSet :: AnchorFixture -> IntSet
witnessSupportSet fixture =
  afSupportSet fixture

fullSourceSupportSet :: AnchorFixture -> IntSet
fullSourceSupportSet =
  nodeSupportSet . afSourceNodes

fullTargetSupportSet :: AnchorFixture -> IntSet
fullTargetSupportSet =
  nodeSupportSet . afTargetNodes

nodeSupportSet :: [FinObjectId] -> IntSet
nodeSupportSet =
  IntSet.fromList . fmap unFinObjectId

validateAnchorFixture :: AnchorFixture -> DerivedPoset -> DerivedPoset -> (FinObjectId -> FinObjectId) -> Either String ()
validateAnchorFixture fixture sourcePoset targetPoset mapToTarget =
  if actualSignature == afExpectedSemanticSignature fixture
    then Right ()
    else
      Left
        ( "fixture parity failure for "
            <> afId fixture
            <> ": expected "
            <> show (afExpectedSemanticSignature fixture)
            <> ", got "
            <> show actualSignature
        )
  where
    sourceNodes = canonicalPosetNodes sourcePoset
    sourceCovers = canonicalPosetCovers sourcePoset
    targetNodes = canonicalPosetNodes targetPoset
    targetCovers = canonicalPosetCovers targetPoset
    mapEntries = sort (fmap (\sourceKey -> (sourceKey, unFinObjectId (mapToTarget (FinObjectId sourceKey)))) sourceNodes)
    supportNodes = IntSet.toAscList (afSupportSet fixture)
    actualSignature =
      [ length sourceNodes
      , length sourceCovers
      , length targetNodes
      , length targetCovers
      , length mapEntries
      , length supportNodes
      , checksumSemanticValues sourceNodes
      , checksumSemanticPairs sourceCovers
      , checksumSemanticValues targetNodes
      , checksumSemanticPairs targetCovers
      , checksumSemanticPairs mapEntries
      , checksumSemanticValues supportNodes
      ]

canonicalPosetNodes :: DerivedPoset -> [Int]
canonicalPosetNodes =
  sort . fmap unFinObjectId . foldMap pure . derivedPosetNodes

canonicalPosetCovers :: DerivedPoset -> [(Int, Int)]
canonicalPosetCovers posetValue =
  sort
    [ (sourceKey, targetKey)
    | (sourceKey, targetKeys) <- IntMap.toAscList (derivedPosetCoversUp posetValue)
    , targetKey <- IntSet.toAscList targetKeys
    ]

checksumSemanticValues :: [Int] -> Int
checksumSemanticValues values =
  sum (zipWith (\indexValue value -> indexValue * (value + 17)) [1 ..] values) `mod` 2147483647

checksumSemanticPairs :: [(Int, Int)] -> Int
checksumSemanticPairs =
  checksumSemanticValues . fmap (\(leftValue, rightValue) -> leftValue * 65599 + rightValue)

checksumDimensions :: IntMap Int -> BenchmarkChecksum
checksumDimensions =
  BenchmarkChecksum
    . IntMap.foldlWithKey'
      (\checksumValue degreeValue dimensionValue -> checksumValue * 65599 + degreeValue * 17 + dimensionValue)
      16777619

checksumSupportedDerived :: IntSet -> Derived GF2 -> BenchmarkChecksum
checksumSupportedDerived witnessSupport derivedValue =
  BenchmarkChecksum
    ( mixChecksumInts
        [ checksumIntSet witnessSupport
        , forceChecksum (checksumDerivedGF2 derivedValue)
        ]
    )

checksumSupportedDimensions :: IntSet -> IntMap Int -> BenchmarkChecksum
checksumSupportedDimensions witnessSupport dimensionsValue =
  BenchmarkChecksum
    ( mixChecksumInts
        [ checksumIntSet witnessSupport
        , forceChecksum (checksumDimensions dimensionsValue)
        ]
    )

checksumIntSet :: IntSet -> Int
checksumIntSet =
  IntSet.foldl' (\checksumValue nodeKey -> checksumValue * 65599 + nodeKey) 16777619

mixChecksumInts :: [Int] -> Int
mixChecksumInts =
  foldl' (\checksumValue value -> checksumValue * 16777619 + value) 216613626

benchmarkEitherWith :: Show errorValue => (value -> BenchmarkChecksum) -> Either errorValue value -> BenchmarkResult
benchmarkEitherWith checksumValue =
  either (Left . show) (Right . forceChecksum . checksumValue)

firstShow :: Show errorValue => Either errorValue value -> Either String value
firstShow =
  either (Left . show) Right

renderAnchorCsv :: [AnchorRow] -> String
renderAnchorCsv rows =
  unlines
    ( "engine,operation,fixture,status,elapsed_ns,elapsed_ms,checksum,message"
        : fmap renderAnchorRow rows
    )

renderAnchorRow :: AnchorRow -> String
renderAnchorRow AnchorRow {arEngine, arOperation, arFixture, arStatus, arElapsedNanoseconds, arElapsedMilliseconds, arChecksum, arMessage} =
  intercalate
    ","
    ( fmap
        csvField
        [ arEngine
        , arOperation
        , arFixture
        , arStatus
        , show arElapsedNanoseconds
        , show arElapsedMilliseconds
        , maybe "" show arChecksum
        , arMessage
        ]
    )

csvField :: String -> String
csvField rawValue =
  "\"" <> concatMap escapeCharacter rawValue <> "\""

escapeCharacter :: Char -> String
escapeCharacter characterValue =
  case characterValue of
    '"' ->
      "\"\""
    _ ->
      [characterValue]

parseArgs :: [String] -> Either String AnchorOptions
parseArgs =
  go AnchorOptions {aoCsvFile = Nothing, aoManifestFile = Nothing}
  where
    go options [] =
      Right options
    go options ("--csv" : csvFile : rest) =
      go options {aoCsvFile = Just csvFile} rest
    go options ("--manifest" : manifestFile : rest) =
      go options {aoManifestFile = Just manifestFile} rest
    go _ ("--csv" : []) =
      Left "moonlight-derived-desc-anchor: --csv requires a file path"
    go _ ("--manifest" : []) =
      Left "moonlight-derived-desc-anchor: --manifest requires a file path"
    go _ (unknownOption : _)
      | "--" `isPrefixOfString` unknownOption =
          Left ("moonlight-derived-desc-anchor: unknown option " <> show unknownOption)
    go _ (unexpectedArgument : _) =
      Left ("moonlight-derived-desc-anchor: unexpected argument " <> show unexpectedArgument)

isPrefixOfString :: String -> String -> Bool
isPrefixOfString prefixValue stringValue =
  take (length prefixValue) stringValue == prefixValue

parseAnchorManifest :: String -> Either String [AnchorFixture]
parseAnchorManifest manifestContents =
  case filter (not . null) (lines manifestContents) of
    [] ->
      Left "DESC anchor manifest is empty"
    headerLine : fixtureLines
      | headerLine /= anchorManifestHeader ->
          Left "DESC anchor manifest header does not match the protocol"
      | otherwise -> do
          fixtures <- traverse parseAnchorManifestLine fixtureLines
          if Set.size (Set.fromList (fmap afId fixtures)) == length fixtures
            then Right fixtures
            else Left "DESC anchor manifest contains duplicate fixture ids"

anchorManifestHeader :: String
anchorManifestHeader =
  intercalate
    "\t"
    [ "fixture_id"
    , "shape"
    , "source_size"
    , "target_size"
    , "support_size"
    , "source_node_count"
    , "source_cover_count"
    , "target_node_count"
    , "target_cover_count"
    , "map_count"
    , "support_count"
    , "source_nodes_checksum"
    , "source_covers_checksum"
    , "target_nodes_checksum"
    , "target_covers_checksum"
    , "map_checksum"
    , "support_checksum"
    ]

parseAnchorManifestLine :: String -> Either String AnchorFixture
parseAnchorManifestLine manifestLine =
  case splitTab manifestLine of
    fixtureId : shapeValue : numericFields
      | length numericFields == 15 -> do
          parsedFields <- traverse parseManifestInteger numericFields
          case parsedFields of
            sourceSize : targetSize : supportSize : expectedSignature ->
              case shapeValue of
                "chain" ->
                  Right (chainFixture fixtureId sourceSize targetSize supportSize expectedSignature)
                "simplicial" ->
                  Right (simplicialFixture fixtureId sourceSize targetSize supportSize expectedSignature)
                _ ->
                  Left ("DESC anchor manifest has unknown shape " <> show shapeValue)
            _ ->
              Left "DESC anchor manifest row has an impossible numeric shape"
    _ ->
      Left ("DESC anchor manifest row has the wrong field count: " <> show manifestLine)

parseManifestInteger :: String -> Either String Int
parseManifestInteger rawValue =
  maybe
    (Left ("DESC anchor manifest has invalid integer " <> show rawValue))
    Right
    (readMaybe rawValue)

splitTab :: String -> [String]
splitTab =
  foldr splitCharacter [""]
  where
    splitCharacter '\t' fieldsValue =
      "" : fieldsValue
    splitCharacter characterValue fieldsValue =
      case fieldsValue of
        [] ->
          [[characterValue]]
        currentField : remainingFields ->
          (characterValue : currentField) : remainingFields
