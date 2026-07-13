module Moonlight.Pale.Ghc.Expr.Scope.Internal
  ( ScopeId (..),
    ScopeIndex (..),
    FreeScopeSummary (..),
  )
where

import Data.Kind (Type)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Vector (Vector)

type ScopeId :: Type
newtype ScopeId = ScopeId Int
  deriving stock (Eq, Ord, Show, Read)

type ScopeIndex :: Type
data ScopeIndex = ScopeIndex
  { siParent :: !(Vector Int),
    siDepth :: !(Vector Int),
    siTin :: !(Vector Int),
    siTout :: !(Vector Int),
    siLift :: !(Vector (Vector Int)),
    siObserved :: !(Vector ScopeId),
    siRoot :: !ScopeId,
    siBinderIntro :: !(Vector ScopeId)
  }
  deriving stock (Eq, Ord, Show)

type FreeScopeSummary :: Type
newtype FreeScopeSummary = FreeScopeSummary (SmallArray ScopeId)

instance Eq FreeScopeSummary where
  FreeScopeSummary leftArray == FreeScopeSummary rightArray =
    freeScopeSummaryToList leftArray == freeScopeSummaryToList rightArray

instance Ord FreeScopeSummary where
  compare (FreeScopeSummary leftArray) (FreeScopeSummary rightArray) =
    compare (freeScopeSummaryToList leftArray) (freeScopeSummaryToList rightArray)

instance Show FreeScopeSummary where
  showsPrec precedence (FreeScopeSummary scopeArray) =
    showParen
      (precedence > 10)
      (showString "FreeScopeSummary " . shows (freeScopeSummaryToList scopeArray))

freeScopeSummaryToList :: SmallArray ScopeId -> [ScopeId]
freeScopeSummaryToList scopeArray =
  [ indexSmallArray scopeArray indexValue
  | indexValue <- [0 .. sizeofSmallArray scopeArray - 1]
  ]
