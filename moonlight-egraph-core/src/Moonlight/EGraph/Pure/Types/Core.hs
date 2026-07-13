module Moonlight.EGraph.Pure.Types.Core
  ( EGraphRevision (..),
    initialEGraphRevision,
    nextEGraphRevision,
    ENode (..),
    EClass (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Moonlight.Core (ClassId)

newtype EGraphRevision = EGraphRevision {eGraphRevisionValue :: Int}
  deriving stock (Eq, Ord, Show)

initialEGraphRevision :: EGraphRevision
initialEGraphRevision =
  EGraphRevision 0

nextEGraphRevision :: EGraphRevision -> EGraphRevision
nextEGraphRevision (EGraphRevision revisionValue) =
  EGraphRevision (revisionValue + 1)

type ENode :: (Type -> Type) -> Type
newtype ENode f = ENode {unENode :: f ClassId}

deriving stock instance (forall a. Ord a => Ord (f a)) => Eq (ENode f)
deriving stock instance (forall a. Ord a => Ord (f a)) => Ord (ENode f)

instance (forall a. Show a => Show (f a)) => Show (ENode f) where
  showsPrec precedence (ENode nodeValue) =
    showParen (precedence > 10) $
      showString "ENode " . showsPrec 11 nodeValue

type EClass :: (Type -> Type) -> Type -> Type
data EClass f a = EClass
  { eClassId :: !ClassId,
    eClassNodes :: !(Set (ENode f)),
    eClassData :: !a,
    eClassParents :: ![(ClassId, ENode f)]
  }

deriving stock instance (Eq a, forall classId. Ord classId => Ord (f classId)) => Eq (EClass f a)

instance (Show a, forall x. Show x => Show (f x)) => Show (EClass f a) where
  showsPrec precedence eClassValue =
    showParen (precedence > 10) $
      showString "EClass "
        . showsPrec 11 (eClassId eClassValue)
        . showChar ' '
        . showsPrec 11 (eClassNodes eClassValue)
        . showChar ' '
        . showsPrec 11 (eClassData eClassValue)
        . showChar ' '
        . showsPrec 11 (eClassParents eClassValue)
