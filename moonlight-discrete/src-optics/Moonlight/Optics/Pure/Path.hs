module Moonlight.Optics.Pure.Path
  ( PathWorldTopology (..),
    PathComplex (..),
    PathFace (..),
    PathEdge (..),
    PathVertex (..),
    PathCell (..),
    HasStalk (stalk),
    complex,
    face,
    edge,
    vertex,
  )
where

import Data.Kind (Constraint, Type)
import Optics.Core

type PathWorldTopology :: Type -> Type -> Type
data PathWorldTopology complexId complex = PathWorldTopology
  { worldComplexAt :: complexId -> complex
  }

type PathComplex :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data PathComplex faceId edgeId vertexId faceValue edgeValue vertexValue = PathComplex
  { complexFaceAt :: faceId -> faceValue,
    complexEdgeAt :: edgeId -> edgeValue,
    complexVertexAt :: vertexId -> vertexValue
  }

type PathFace :: Type -> Type
newtype PathFace cell = PathFace
  { faceCell :: cell
  }
  deriving stock (Eq, Show)

type PathEdge :: Type -> Type
newtype PathEdge cell = PathEdge
  { edgeCell :: cell
  }
  deriving stock (Eq, Show)

type PathVertex :: Type -> Type
newtype PathVertex cell = PathVertex
  { vertexCell :: cell
  }
  deriving stock (Eq, Show)

type PathCell :: Type -> Type
newtype PathCell stalkValue = PathCell
  { cellStalk :: stalkValue
  }
  deriving stock (Eq, Show)

type HasStalk :: Type -> Type -> Constraint
class HasStalk cell stalk | cell -> stalk where
  stalk :: Getter cell stalk

instance HasStalk (PathCell stalkValue) stalkValue where
  stalk = to cellStalk

instance HasStalk cell stalkValue => HasStalk (PathFace cell) stalkValue where
  stalk = to (view stalk . faceCell)

instance HasStalk cell stalkValue => HasStalk (PathEdge cell) stalkValue where
  stalk = to (view stalk . edgeCell)

instance HasStalk cell stalkValue => HasStalk (PathVertex cell) stalkValue where
  stalk = to (view stalk . vertexCell)

complex :: complexId -> IxGetter complexId (PathWorldTopology complexId complex) complex
complex complexId =
  ito (\world -> (complexId, worldComplexAt world complexId))

face :: faceId -> IxGetter faceId (PathComplex faceId edgeId vertexId faceValue edgeValue vertexValue) faceValue
face faceId =
  ito (\complexValue -> (faceId, complexFaceAt complexValue faceId))

edge :: edgeId -> IxGetter edgeId (PathComplex faceId edgeId vertexId faceValue edgeValue vertexValue) edgeValue
edge edgeId =
  ito (\complexValue -> (edgeId, complexEdgeAt complexValue edgeId))

vertex :: vertexId -> IxGetter vertexId (PathComplex faceId edgeId vertexId faceValue edgeValue vertexValue) vertexValue
vertex vertexId =
  ito (\complexValue -> (vertexId, complexVertexAt complexValue vertexId))
