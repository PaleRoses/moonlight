module Moonlight.Optics.Effect.LawNames
  ( LawName (..),
    lawName,
  )
where

import Data.Kind (Type)

type LawName :: Type
data LawName
  = LensGetPut
  | LensPutGet
  | LensPutPut
  | PrismPreviewReview
  | PrismReviewPreview
  | RestrictionFunctorial
  | RestrictionCompat
  | TraversalIdentity
  | TraversalCompose
  | ReadOnlyNamedExportAuditTH
  | ReadOnlyOperatorExportAuditTH
  | IndexedCompose
  deriving stock (Eq, Ord, Show)

lawName :: LawName -> String
lawName law =
  case law of
    LensGetPut -> "lens_get_put"
    LensPutGet -> "lens_put_get"
    LensPutPut -> "lens_put_put"
    PrismPreviewReview -> "prism_preview_review"
    PrismReviewPreview -> "prism_review_preview"
    RestrictionFunctorial -> "restriction_functorial"
    RestrictionCompat -> "restriction_compat"
    TraversalIdentity -> "traversal_id"
    TraversalCompose -> "traversal_compose"
    ReadOnlyNamedExportAuditTH -> "read_only_named_export_audit_th"
    ReadOnlyOperatorExportAuditTH -> "read_only_operator_export_audit_th"
    IndexedCompose -> "ix_compose"
