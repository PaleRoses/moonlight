{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Site.Microsupport
  ( LocalClosed
  , mkLocalClosed
  , localClosedPoset
  , localClosedNodes
  , Criticality (..)
  , isLocallyClosed
  ) where

import Data.Kind (Type)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , leq
  , memberOfDerivedPoset
  )
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))

type LocalClosed :: Type
data LocalClosed = LocalClosed
  { localClosedPoset :: !DerivedPoset
  , localClosedNodes :: !IntSet
  }
  deriving stock (Eq, Show)

type Criticality :: Type
data Criticality = NonCritical | Critical
  deriving stock (Eq, Ord, Show, Read)

mkLocalClosed :: DerivedPoset -> IntSet -> Either DerivedFailure LocalClosed
mkLocalClosed posetValue nodeKeys
  | not (IS.null unknownNodeKeys) =
      Left
        ( DerivedFunctorInvalidSupport
            ("locally closed support contains foreign nodes " <> show (IS.toAscList unknownNodeKeys))
        )
  | IS.size nodeKeys > 1 && not (isLocallyClosed posetValue nodeKeys) =
      Left
        ( DerivedFunctorInvalidSupport
            ("support is not order-convex: " <> show (IS.toAscList nodeKeys))
        )
  | otherwise =
      Right
        LocalClosed
          { localClosedPoset = posetValue
          , localClosedNodes = nodeKeys
          }
  where
    unknownNodeKeys =
      IS.filter (not . memberOfDerivedPoset posetValue . FinObjectId) nodeKeys

isLocallyClosed :: DerivedPoset -> IntSet -> Bool
isLocallyClosed p z =
  IS.isSubsetOf z posetNodeKeys
    && all okay (V.toList (derivedPosetNodes p))
  where
    posetNodeKeys =
      IS.fromList (fmap unFinObjectId (V.toList (derivedPosetNodes p)))
    inside = [ objectValue | objectValue@(FinObjectId objectKey) <- V.toList (derivedPosetNodes p), IS.member objectKey z ]
    okay objectValue@(FinObjectId objectKey)
      | IS.member objectKey z = True
      | otherwise = not (existsLower && existsUpper)
      where
        existsLower = any (\lowerObject -> leq p lowerObject objectValue) inside
        existsUpper = any (leq p objectValue) inside
