{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Spec.LambdaBindingGoal.Lambda
  ( Name,
    LamF (..),
    LamAnalysis,
    lamAnalysisSpec,
    nameString,
    lamTerm,
    appTerm,
    varTerm,
    litTerm,
    addLamTerm,
  )
where

import Moonlight.Core (ZipMatch (..))
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Moonlight.Core (HasConstructorTag (..), zipSameNodeShape)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Data.Fix (Fix (..))

type Name :: Type
newtype Name = Name String
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString)

nameString :: Name -> String
nameString (Name rawName) =
  rawName

type LamF :: Type -> Type
data LamF a
  = LVar !Name
  | LLam !Name !a
  | LApp !a !a
  | LLit !Int
  | LAdd !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Hashable a => Hashable (LamF a) where
  hashWithSalt salt = \case
    LVar n -> hashWithSalt (hashWithSalt salt (0 :: Int)) n
    LLam n body -> hashWithSalt (hashWithSalt (hashWithSalt salt (1 :: Int)) n) body
    LApp f x -> hashWithSalt (hashWithSalt (hashWithSalt salt (2 :: Int)) f) x
    LLit n -> hashWithSalt (hashWithSalt salt (3 :: Int)) n
    LAdd l r -> hashWithSalt (hashWithSalt (hashWithSalt salt (4 :: Int)) l) r

type LamTag :: Type
data LamTag = VarTag | LamTag | AppTag | LitTag | AddTag
  deriving stock (Eq, Ord, Show)

instance HasConstructorTag LamF where
  type ConstructorTag LamF = LamTag
  constructorTag = \case
    LVar _ -> VarTag
    LLam _ _ -> LamTag
    LApp _ _ -> AppTag
    LLit _ -> LitTag
    LAdd _ _ -> AddTag

instance ZipMatch LamF where
  zipMatch =
    zipSameNodeShape

type LamAnalysis :: Type
type LamAnalysis = ()

lamAnalysisSpec :: AnalysisSpec LamF LamAnalysis
lamAnalysisSpec =
  AnalysisSpec
    { asMake = \_ -> (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

varTerm :: String -> Fix LamF
varTerm n = Fix (LVar (Name n))

litTerm :: Int -> Fix LamF
litTerm n = Fix (LLit n)

lamTerm :: String -> Fix LamF -> Fix LamF
lamTerm n body = Fix (LLam (Name n) body)

appTerm :: Fix LamF -> Fix LamF -> Fix LamF
appTerm f x = Fix (LApp f x)

addLamTerm :: Fix LamF -> Fix LamF -> Fix LamF
addLamTerm l r = Fix (LAdd l r)
