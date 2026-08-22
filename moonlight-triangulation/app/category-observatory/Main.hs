{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Moonlight.Homology.Pure.Topology.CellCategory
  ( ComplexCat,
    ComplexMor (..),
    ComplexOb (..),
    complexCategory,
  )
import Moonlight.Triangulation.CellComplex
  ( DCELComplex,
    fromExactCellSet,
  )
import Moonlight.Category
  ( Category (..),
    FiniteComposableCategory (..),
    chainVertices,
  )
import Moonlight.Category.Simplicial
  ( GeneratedSSet,
    NerveSimplex,
    TruncatedNormalizedSSet,
    generatedSimplicesAtDimension,
    nerveSimplexChain,
    normalizedNerve,
    simplicesAtDimension,
    unnormalizedNerve,
  )
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
  )
import Moonlight.Triangulation.BulkLoad (delaunayGeometry)
import Moonlight.Triangulation.CellSet
  ( CellSelectionError,
    ExactCellSet,
    closeFaceCellSet,
    exactCellSetEdgeCount,
    exactCellSetFaceCount,
    exactCellSetVertexCount,
  )
import Moonlight.Triangulation.Dcel
  ( faceVertices,
    undirectedEndpoints,
    vertexPoint,
  )
import Moonlight.Triangulation.Handles.HandleDefs
  ( FaceId,
    UndirectedEdgeId,
    VertexId,
    unFaceId,
    unUndirectedEdgeId,
    unVertexId,
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (innerFaces)
import Moonlight.Triangulation.Types
  ( BuildError,
    DelaunayTriangulation,
    Point (..),
  )
import Numeric.Natural (Natural)
import System.Exit (die)

main :: IO ()
main =
  either
    (die . renderExhibitError)
    (LazyByteString.putStrLn . Aeson.encode)
    buildExhibit

data ExhibitError
  = ExhibitTriangulationBuildFailed BuildError
  | ExhibitCellSelectionFailed CellSelectionError
  | ExhibitMorphismEndpointUnavailable MorphismEndpoint Int
  | ExhibitFaceWithoutVertices FaceId
  | ExhibitMalformedTwoSimplex Int
  | ExhibitInventoryMismatch (Int, Int, Int) (Int, Int, Int)
  | ExhibitCategoryEnumerationMismatch (Int, Int, Int, Int) (Int, Int, Int, Int)
  | ExhibitNerveCountMismatch Int Int
  deriving stock (Show)

data MorphismEndpoint
  = MorphismSource
  | MorphismTarget
  deriving stock (Show)

renderExhibitError :: ExhibitError -> String
renderExhibitError = show

data Exhibit = Exhibit
  { schemaVersion :: Int,
    exhibitTitle :: String,
    mesh :: MeshProjection,
    category :: CategoryProjection,
    nerve :: NerveProjection
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshProjection = MeshProjection
  { meshVertices :: [MeshVertex],
    meshEdges :: [MeshEdge],
    meshFaces :: [MeshFace]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshVertex = MeshVertex
  { meshVertexIdentifier :: String,
    meshVertexObjectIdentifier :: String,
    meshVertexPosition :: Position
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshEdge = MeshEdge
  { meshEdgeIdentifier :: String,
    meshEdgeObjectIdentifier :: String,
    meshEdgeVertexIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshFace = MeshFace
  { meshFaceIdentifier :: String,
    meshFaceObjectIdentifier :: String,
    meshFaceVertexIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Position = Position
  { positionX :: Double,
    positionY :: Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryProjection = CategoryProjection
  { categoryObjects :: [CategoryObject],
    categoryMorphisms :: [CategoryMorphism]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryObject = CategoryObject
  { categoryObjectIdentifier :: String,
    categoryObjectKind :: CategoryObjectKind,
    categoryObjectLabel :: String,
    categoryObjectPosition :: Position
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryObjectKind
  = VertexObject
  | EdgeObject
  | FaceObject
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryMorphism = CategoryMorphism
  { categoryMorphismIdentifier :: String,
    categoryMorphismKind :: CategoryMorphismKind,
    categoryMorphismSourceObject :: String,
    categoryMorphismTargetObject :: String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryMorphismKind
  = IdentityMorphism
  | FaceEdgeMorphism
  | EdgeVertexMorphism
  | FaceVertexComposite
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveProjection = NerveProjection
  { nerveTwoSimplices :: [NerveTwoSimplex],
    nerveSimplexCounts :: [NerveSimplexCount]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveSimplexCount = NerveSimplexCount
  { nerveDimension :: Natural,
    normalizedSimplexCount :: Int,
    unnormalizedSimplexCount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveTwoSimplex = NerveTwoSimplex
  { nerveSimplexIdentifier :: String,
    nerveSimplexObjectIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

buildExhibit :: Either ExhibitError Exhibit
buildExhibit = do
  triangulation <- first ExhibitTriangulationBuildFailed (delaunayGeometry exhibitPoints)
  selectedCells <-
    first ExhibitCellSelectionFailed
      (closeFaceCellSet triangulation (innerFaces triangulation))
  let complexValue = fromExactCellSet selectedCells
      categoryValue = complexCategory complexValue
  validateInventory selectedCells complexValue
  validateCategoryEnumeration complexValue categoryValue
  categoryProjection <- projectCategory triangulation categoryValue
  nerveProjection <- projectNerve categoryValue
  validateNerveCount complexValue nerveProjection
  pure
    Exhibit
      { schemaVersion = 2,
        exhibitTitle = "Triangulation, incidence category, and flag nerve",
        mesh = projectMesh triangulation complexValue,
        category = categoryProjection,
        nerve = nerveProjection
      }

validateInventory :: ExactCellSet -> DCELComplex -> Either ExhibitError ()
validateInventory cellSet complexValue =
  let expectedCounts =
        ( exactCellSetVertexCount cellSet,
          exactCellSetEdgeCount cellSet,
          exactCellSetFaceCount cellSet
        )
      observedCounts =
        ( length (vertices complexValue),
          length (edges complexValue),
          length (faces complexValue)
        )
   in if observedCounts == expectedCounts
        then Right ()
        else Left (ExhibitInventoryMismatch expectedCounts observedCounts)

validateCategoryEnumeration ::
  DCELComplex ->
  ComplexCat DCELComplex ->
  Either ExhibitError ()
validateCategoryEnumeration complexValue categoryValue =
  let morphisms = enumerateMorphisms categoryValue
      expectedCounts =
        ( length (vertices complexValue) + length (edges complexValue) + length (faces complexValue),
          sum (fmap (length . faceBoundary complexValue) (faces complexValue)),
          sum (fmap (edgeEndpointCount complexValue) (edges complexValue)),
          sum (fmap (faceFlagCount complexValue) (faces complexValue))
        )
      observedCounts =
        ( length (filter isIdentityMorphism morphisms),
          length (filter isFaceEdgeMorphism morphisms),
          length (filter isEdgeVertexMorphism morphisms),
          length (filter isFaceVertexMorphism morphisms)
        )
   in if observedCounts == expectedCounts
        then Right ()
        else Left (ExhibitCategoryEnumerationMismatch expectedCounts observedCounts)

validateNerveCount :: DCELComplex -> NerveProjection -> Either ExhibitError ()
validateNerveCount complexValue nerveProjection =
  let expectedCount = sum (fmap (faceFlagCount complexValue) (faces complexValue))
      observedCount = length (nerveTwoSimplices nerveProjection)
   in if observedCount == expectedCount
        then Right ()
        else Left (ExhibitNerveCountMismatch expectedCount observedCount)

edgeEndpointCount :: DCELComplex -> Edge DCELComplex -> Int
edgeEndpointCount complexValue edgeValue =
  let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
   in length (nubOrd [sourceVertex, targetVertex])

faceFlagCount :: DCELComplex -> Face DCELComplex -> Int
faceFlagCount complexValue faceValue =
  sum
    ( fmap
        (edgeEndpointCount complexValue . orientedEdge)
        (faceBoundary complexValue faceValue)
    )

isIdentityMorphism :: ComplexMor DCELComplex -> Bool
isIdentityMorphism morphismValue =
  case morphismValue of
    IdentityMor _ -> True
    _ -> False

isFaceEdgeMorphism :: ComplexMor DCELComplex -> Bool
isFaceEdgeMorphism morphismValue =
  case morphismValue of
    FaceToEdge _ _ _ -> True
    _ -> False

isEdgeVertexMorphism :: ComplexMor DCELComplex -> Bool
isEdgeVertexMorphism morphismValue =
  case morphismValue of
    EdgeToVertex _ _ -> True
    _ -> False

isFaceVertexMorphism :: ComplexMor DCELComplex -> Bool
isFaceVertexMorphism morphismValue =
  case morphismValue of
    FaceToVertex _ _ _ -> True
    _ -> False

projectMesh :: DelaunayTriangulation () -> DCELComplex -> MeshProjection
projectMesh triangulation complexValue =
  MeshProjection
    { meshVertices = fmap (projectMeshVertex triangulation) (vertices complexValue),
      meshEdges = fmap (projectMeshEdge triangulation) (edges complexValue),
      meshFaces = fmap (projectMeshFace triangulation) (faces complexValue)
    }

projectMeshVertex :: DelaunayTriangulation () -> VertexId -> MeshVertex
projectMeshVertex triangulation vertexValue =
  MeshVertex
    { meshVertexIdentifier = vertexIdentifier vertexValue,
      meshVertexObjectIdentifier = vertexObjectIdentifier vertexValue,
      meshVertexPosition = pointPosition (vertexPoint triangulation vertexValue)
    }

projectMeshEdge :: DelaunayTriangulation () -> UndirectedEdgeId -> MeshEdge
projectMeshEdge triangulation edgeValue =
  let (sourceVertex, targetVertex) = undirectedEndpoints triangulation edgeValue
   in MeshEdge
        { meshEdgeIdentifier = edgeIdentifier edgeValue,
          meshEdgeObjectIdentifier = edgeObjectIdentifier edgeValue,
          meshEdgeVertexIdentifiers = fmap vertexIdentifier [sourceVertex, targetVertex]
        }

projectMeshFace :: DelaunayTriangulation () -> FaceId -> MeshFace
projectMeshFace triangulation faceValue =
  MeshFace
    { meshFaceIdentifier = faceIdentifier faceValue,
      meshFaceObjectIdentifier = faceObjectIdentifier faceValue,
      meshFaceVertexIdentifiers = fmap vertexIdentifier (faceVertices triangulation faceValue)
    }

projectCategory ::
  DelaunayTriangulation () ->
  ComplexCat DCELComplex ->
  Either ExhibitError CategoryProjection
projectCategory triangulation categoryValue = do
  objectProjections <-
    traverse
      (projectCategoryObject triangulation)
      (enumerateObjects categoryValue)
  morphismProjections <-
    traverse
      (projectCategoryMorphism categoryValue)
      (zip [0 ..] (enumerateMorphisms categoryValue))
  pure
    CategoryProjection
      { categoryObjects = objectProjections,
        categoryMorphisms = morphismProjections
      }

projectCategoryObject ::
  DelaunayTriangulation () ->
  ComplexOb DCELComplex ->
  Either ExhibitError CategoryObject
projectCategoryObject triangulation objectValue = do
  objectPosition <- positionForObject triangulation objectValue
  pure
    CategoryObject
      { categoryObjectIdentifier = objectIdentifier objectValue,
        categoryObjectKind = objectKind objectValue,
        categoryObjectLabel = objectLabel objectValue,
        categoryObjectPosition = objectPosition
      }

projectCategoryMorphism ::
  ComplexCat DCELComplex ->
  (Int, ComplexMor DCELComplex) ->
  Either ExhibitError CategoryMorphism
projectCategoryMorphism categoryValue (morphismIndex, morphismValue) = do
  sourceObject <-
    first
      (const (ExhibitMorphismEndpointUnavailable MorphismSource morphismIndex))
      (source categoryValue morphismValue)
  targetObject <-
    first
      (const (ExhibitMorphismEndpointUnavailable MorphismTarget morphismIndex))
      (target categoryValue morphismValue)
  pure
    CategoryMorphism
      { categoryMorphismIdentifier = "morphism-" <> show morphismIndex,
        categoryMorphismKind = morphismKind morphismValue,
        categoryMorphismSourceObject = objectIdentifier sourceObject,
        categoryMorphismTargetObject = objectIdentifier targetObject
      }

projectNerve :: ComplexCat DCELComplex -> Either ExhibitError NerveProjection
projectNerve categoryValue = do
  let normalizedNerveValue = normalizedNerve categoryValue nerveObservationBound
      unnormalizedNerveValue = unnormalizedNerve categoryValue nerveObservationBound
  twoSimplices <-
    traverse
      projectTwoSimplex
      (zip [0 ..] (simplicesAtDimension normalizedNerveValue nerveObservationBound))
  pure
    NerveProjection
      { nerveTwoSimplices = twoSimplices,
        nerveSimplexCounts =
          fmap
            (projectNerveSimplexCount normalizedNerveValue unnormalizedNerveValue)
            nerveObservationDimensions
      }

nerveObservationBound :: Natural
nerveObservationBound = 2

nerveObservationDimensions :: [Natural]
nerveObservationDimensions = [0, 1, 2]

projectNerveSimplexCount ::
  TruncatedNormalizedSSet (NerveSimplex (ComplexCat DCELComplex)) ->
  GeneratedSSet (NerveSimplex (ComplexCat DCELComplex)) ->
  Natural ->
  NerveSimplexCount
projectNerveSimplexCount normalizedNerveValue unnormalizedNerveValue dimensionValue =
  NerveSimplexCount
    { nerveDimension = dimensionValue,
      normalizedSimplexCount = length (simplicesAtDimension normalizedNerveValue dimensionValue),
      unnormalizedSimplexCount = length (generatedSimplicesAtDimension unnormalizedNerveValue dimensionValue)
    }

projectTwoSimplex ::
  (Int, NerveSimplex (ComplexCat DCELComplex)) ->
  Either ExhibitError NerveTwoSimplex
projectTwoSimplex (simplexIndex, simplexValue) =
  case chainVertices (nerveSimplexChain simplexValue) of
    faceObject :| [edgeObject, vertexObject] ->
      pure
        NerveTwoSimplex
          { nerveSimplexIdentifier = "simplex-" <> show simplexIndex,
            nerveSimplexObjectIdentifiers =
              fmap objectIdentifier [faceObject, edgeObject, vertexObject]
          }
    _ -> Left (ExhibitMalformedTwoSimplex simplexIndex)

positionForObject ::
  DelaunayTriangulation () ->
  ComplexOb DCELComplex ->
  Either ExhibitError Position
positionForObject triangulation objectValue =
  case objectValue of
    VertexOb vertexValue ->
      Right (pointPosition (vertexPoint triangulation vertexValue))
    EdgeOb edgeValue ->
      let (sourceVertex, targetVertex) = undirectedEndpoints triangulation edgeValue
       in Right
            ( midpoint
                (pointPosition (vertexPoint triangulation sourceVertex))
                (pointPosition (vertexPoint triangulation targetVertex))
            )
    FaceOb faceValue ->
      centroid faceValue
        (fmap (pointPosition . vertexPoint triangulation) (faceVertices triangulation faceValue))

centroid :: FaceId -> [Position] -> Either ExhibitError Position
centroid faceValue positions =
  case positions of
    [] -> Left (ExhibitFaceWithoutVertices faceValue)
    _ ->
      let (totalX, totalY) =
            foldl'
              (\(accumulatedX, accumulatedY) positionValue ->
                 ( accumulatedX + positionX positionValue,
                   accumulatedY + positionY positionValue
                 )
              )
              (0, 0)
              positions
          positionCount = fromIntegral (length positions)
       in Right (Position (totalX / positionCount) (totalY / positionCount))

midpoint :: Position -> Position -> Position
midpoint firstPosition secondPosition =
  Position
    { positionX = (positionX firstPosition + positionX secondPosition) / 2,
      positionY = (positionY firstPosition + positionY secondPosition) / 2
    }

pointPosition :: Point -> Position
pointPosition pointValue =
  Position
    { positionX = pointX pointValue,
      positionY = pointY pointValue
    }

objectIdentifier :: ComplexOb DCELComplex -> String
objectIdentifier objectValue =
  case objectValue of
    VertexOb vertexValue -> vertexObjectIdentifier vertexValue
    EdgeOb edgeValue -> edgeObjectIdentifier edgeValue
    FaceOb faceValue -> faceObjectIdentifier faceValue

objectKind :: ComplexOb DCELComplex -> CategoryObjectKind
objectKind objectValue =
  case objectValue of
    VertexOb _ -> VertexObject
    EdgeOb _ -> EdgeObject
    FaceOb _ -> FaceObject

objectLabel :: ComplexOb DCELComplex -> String
objectLabel objectValue =
  case objectValue of
    VertexOb vertexValue -> "v" <> show (unVertexId vertexValue)
    EdgeOb edgeValue -> "e" <> show (unUndirectedEdgeId edgeValue)
    FaceOb faceValue -> "f" <> show (unFaceId faceValue)

morphismKind :: ComplexMor DCELComplex -> CategoryMorphismKind
morphismKind morphismValue =
  case morphismValue of
    IdentityMor _ -> IdentityMorphism
    FaceToEdge _ _ _ -> FaceEdgeMorphism
    EdgeToVertex _ _ -> EdgeVertexMorphism
    FaceToVertex _ _ _ -> FaceVertexComposite

vertexIdentifier :: VertexId -> String
vertexIdentifier vertexValue = "vertex-" <> show (unVertexId vertexValue)

edgeIdentifier :: UndirectedEdgeId -> String
edgeIdentifier edgeValue = "edge-" <> show (unUndirectedEdgeId edgeValue)

faceIdentifier :: FaceId -> String
faceIdentifier faceValue = "face-" <> show (unFaceId faceValue)

vertexObjectIdentifier :: VertexId -> String
vertexObjectIdentifier vertexValue = "object-" <> vertexIdentifier vertexValue

edgeObjectIdentifier :: UndirectedEdgeId -> String
edgeObjectIdentifier edgeValue = "object-" <> edgeIdentifier edgeValue

faceObjectIdentifier :: FaceId -> String
faceObjectIdentifier faceValue = "object-" <> faceIdentifier faceValue

exhibitPoints :: Vector.Vector Point
exhibitPoints =
  Vector.fromList
    [ Point 80 72,
      Point 222 48,
      Point 362 92,
      Point 52 208,
      Point 176 166,
      Point 310 190,
      Point 414 232,
      Point 112 330,
      Point 258 314,
      Point 380 354
    ]
