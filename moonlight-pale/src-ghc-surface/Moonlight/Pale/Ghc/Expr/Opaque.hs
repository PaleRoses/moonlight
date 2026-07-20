module Moonlight.Pale.Ghc.Expr.Opaque
  ( HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    hsOpaqueTagName,
    hsPatOpaqueTagName,
  )
where

import Data.Kind (Type)

type HsOpaqueTag :: Type
data HsOpaqueTag
  = OpaqueOverLabel
  | OpaqueIPVar
  | OpaqueAppType
  | OpaqueExplicitSum
  | OpaqueMultiIf
  | OpaqueRecordUpd
  | OpaqueGetField
  | OpaqueProjection
  | OpaqueExprWithTySig
  | OpaqueArithSeq
  | OpaqueTypedBracket
  | OpaqueUntypedBracket
  | OpaqueTypedSplice
  | OpaqueUntypedSplice
  | OpaqueProc
  | OpaqueStatic
  | OpaquePragE
  | OpaqueEmbTy
  | OpaqueHole
  | OpaqueForAll
  | OpaqueQual
  | OpaqueFunArr
  | OpaqueXExpr
  | OpaqueLambdaMatchGroup
  | OpaqueCaseAlternative
  | OpaqueLocalIPBinds
  | OpaqueXLocalBinds
  | OpaqueValBindsExtension
  | OpaqueUnsupportedBind
  | OpaqueUnsupportedStmt
  | OpaqueUnsupportedGuard
  | OpaqueMissingGuardFallback
  | OpaqueUnsupportedRecordField
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type HsPatOpaqueTag :: Type
data HsPatOpaqueTag
  = PatOpaqueOr
  | PatOpaqueSum
  | PatOpaqueView
  | PatOpaqueSplice
  | PatOpaqueNPlusK
  | PatOpaqueSig
  | PatOpaqueEmbTy
  | PatOpaqueInvis
  | PatOpaqueRecCon
  | PatOpaqueNegativeLit
  | PatOpaqueUnboxedTuple
  | PatOpaqueExtension
  deriving stock (Eq, Ord, Show, Enum, Bounded)

hsOpaqueTagName :: HsOpaqueTag -> String
hsOpaqueTagName =
  show

hsPatOpaqueTagName :: HsPatOpaqueTag -> String
hsPatOpaqueTagName =
  show
