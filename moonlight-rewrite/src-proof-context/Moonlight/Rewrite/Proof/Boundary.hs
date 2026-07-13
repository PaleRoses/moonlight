-- | Boundary checker for required external proof manifests.
-- Owns expected obligation assembly from simplicial and restriction
-- registries, theorem-name validation, runtime-law classification, and exact
-- comparison with the manifest emitted independently by compiled Lean
-- declarations.
-- Contracts: manifests canonicalize by sort/nub, and missing, unexpected, or
-- invalid theorem names are typed obstructions rather than parser folklore.
module Moonlight.Rewrite.Proof.Boundary
  ( ProofObligation (..),
    ProofObligationClassification (..),
    ProofTheoremName (..),
    ProofTheoremNameRejection (..),
    InvalidProofTheoremName (..),
    ProofTheoremManifest,
    proofTheoremManifestTheorems,
    proofTheoremManifestIdentifiers,
    proofTheoremManifestFromIdentifiers,
    mkProofTheoremName,
    ProofBoundaryObstruction (..),
    ProofBoundaryDischarge (..),
    proofObligationRuntimeLawId,
    requiredProofObligations,
    requiredProofTheoremManifest,
    requiredRuntimeLawObligationIdentifiers,
    requiredRestrictionRuntimeLawIdentifiers,
    requiredRestrictionLeanTheoremManifest,
    requiredRestrictionManifestTheoremManifest,
    proofManifestPath,
    proofManifestHashPath,
    restrictionKernelSchemaPath,
    restrictionKernelSchemaHashPath,
    parseTheoremManifest,
    checkProofBoundary,
  )
where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (nub, sort)
import Moonlight.Core qualified as ProofManifest
import Moonlight.Sheaf.Context.Schema
  ( restrictionKernelLeanTheoremIdentifiers,
    restrictionKernelManifestTheoremIdentifiers,
    restrictionKernelRuntimeLawIdentifiers,
  )

type ProofTheoremName :: Type
newtype ProofTheoremName = ProofTheoremName
  { proofTheoremNameString :: String
  }
  deriving stock (Eq, Ord, Show)

type ProofTheoremNameRejection :: Type
data ProofTheoremNameRejection
  = EmptyProofTheoremName
  | ProofTheoremNameContainsWhitespace
  deriving stock (Eq, Ord, Show)

type InvalidProofTheoremName :: Type
data InvalidProofTheoremName = InvalidProofTheoremName
  { invalidProofTheoremNameIdentifier :: !String,
    invalidProofTheoremNameRejection :: !ProofTheoremNameRejection
  }
  deriving stock (Eq, Ord, Show)

type ProofTheoremManifest :: Type
newtype ProofTheoremManifest = ProofTheoremManifest
  { proofTheoremManifestTheorems :: [ProofTheoremName]
  }
  deriving stock (Eq, Ord, Show)

proofTheoremManifestIdentifiers :: ProofTheoremManifest -> [String]
proofTheoremManifestIdentifiers =
  fmap proofTheoremNameString . proofTheoremManifestTheorems

proofTheoremManifestFromIdentifiers :: [String] -> Either [InvalidProofTheoremName] ProofTheoremManifest
proofTheoremManifestFromIdentifiers =
  fmap proofTheoremManifestFromTheorems
    . collectValidated
    . fmap mkProofTheoremName

proofTheoremManifestFromTheorems :: [ProofTheoremName] -> ProofTheoremManifest
proofTheoremManifestFromTheorems =
  ProofTheoremManifest . canonicalTheoremNames

mkProofTheoremName :: String -> Either InvalidProofTheoremName ProofTheoremName
mkProofTheoremName rawName =
  case theoremNameRejection rawName of
    Nothing ->
      Right (ProofTheoremName rawName)
    Just rejection ->
      Left
        InvalidProofTheoremName
          { invalidProofTheoremNameIdentifier = rawName,
            invalidProofTheoremNameRejection = rejection
          }

theoremNameRejection :: String -> Maybe ProofTheoremNameRejection
theoremNameRejection rawName
  | null rawName = Just EmptyProofTheoremName
  | any isSpace rawName = Just ProofTheoremNameContainsWhitespace
  | otherwise = Nothing

type ProofObligationClassification :: Type
data ProofObligationClassification
  = ProofOnlyObligation
  | RuntimeLawObligation !String
  deriving stock (Eq, Ord, Show)

type ProofObligation :: Type
data ProofObligation = ProofObligation
  { proofObligationTheorem :: !ProofTheoremName,
    proofObligationClassification :: !ProofObligationClassification
  }
  deriving stock (Eq, Ord, Show)

proofObligationRuntimeLawId :: ProofObligation -> Maybe String
proofObligationRuntimeLawId obligation =
  case proofObligationClassification obligation of
    ProofOnlyObligation ->
      Nothing
    RuntimeLawObligation runtimeLawId ->
      Just runtimeLawId

type ProofBoundaryObstruction :: Type
data ProofBoundaryObstruction
  = ProofBoundaryManifestParseFailed !ProofManifest.ProofManifestError
  | ProofBoundaryInvalidObligations ![InvalidProofTheoremName]
  | ProofBoundaryInvalidManifestTheorems ![InvalidProofTheoremName]
  | ProofBoundaryMissingTheorems ![ProofTheoremName]
  | ProofBoundaryUnexpectedTheorems ![ProofTheoremName]
  deriving stock (Eq, Show)

type ProofBoundaryDischarge :: Type
data ProofBoundaryDischarge = ProofBoundaryDischarge
  { pbdObligations :: ![ProofObligation],
    pbdManifestTheorems :: !ProofTheoremManifest
  }
  deriving stock (Eq, Ord, Show)

requiredProofObligations :: Either ProofBoundaryObstruction [ProofObligation]
requiredProofObligations =
  first ProofBoundaryInvalidObligations $
    collectValidated
      ( fmap proofOnlyObligation proofOnlyTheoremIdentifiers
          <> fmap restrictionKernelObligation restrictionKernelManifestTheoremIdentifiers
      )

-- | The proof-only obligations expected by the e-graph mechanization boundary.
-- 'requiredProofTheoremManifest' is the canonical expected view used for exact
-- comparison; the observed manifest is emitted independently by Lean.
proofOnlyTheoremIdentifiers :: [String]
proofOnlyTheoremIdentifiers =
  [ "find_idempotent",
    "congruence_closure",
    "rebuild_restores_congruence",
    "unifier_side_projection_apex",
    "region_path_compression_preserves_equivalence",
    "contextual_equivalence_kernel_correct",
    "extract_in_class",
    "extract_optimal",
    "proof_soundness",
    "contextual_extraction_respects_scope",
    "scoped_saturation_trace_respects_scope",
    "rewrite_composition_associative_up_to_alpha",
    "rewrite_identity_left",
    "rewrite_identity_right",
    "rewrite_decoration_scope_preserved",
    "rewrite_restriction_commutes_with_composition",
    "context_restriction_exists_iff_order",
    "context_sheaf_gluing_restricts",
    "context_sheaf_gluing_unique",
    "sheaf_capability_environment_sound",
    "principal_support_contains_generator",
    "support_normalization_preserves_semantics",
    "support_family_rewrites_at_exact_support",
    "supported_fact_family_rules_at_exact_support",
    "proof_context_evidence_sound",
    "support_aware_proof_evidence_sound",
    "obstruction_report_complete",
    "obstruction_coboundary_squared_zero",
    "positive_first_cohomology_obstructs_gluing",
    "poset_cech_differential_squared_zero",
    "poset_cohomology_respects_restrictions",
    "lattice_join_commutative",
    "lattice_meet_commutative",
    "lattice_join_idempotent",
    "lattice_meet_idempotent",
    "lattice_absorption_join_meet",
    "lattice_absorption_meet_join",
    "lattice_join_associative",
    "lattice_meet_associative",
    "merge_monotonicity"
  ]

restrictionKernelObligation :: String -> Either InvalidProofTheoremName ProofObligation
restrictionKernelObligation rawName = do
  theoremName <-
    mkProofTheoremName rawName
  pure
    ProofObligation
      { proofObligationTheorem = theoremName,
        proofObligationClassification =
          if rawName `elem` requiredRestrictionRuntimeLawIdentifiers
            then RuntimeLawObligation rawName
            else ProofOnlyObligation
      }

proofOnlyObligation :: String -> Either InvalidProofTheoremName ProofObligation
proofOnlyObligation =
  fmap proofOnlyObligationFromName . mkProofTheoremName

proofOnlyObligationFromName :: ProofTheoremName -> ProofObligation
proofOnlyObligationFromName theoremName =
  ProofObligation
    { proofObligationTheorem = theoremName,
      proofObligationClassification = ProofOnlyObligation
    }

requiredProofTheoremManifest :: Either ProofBoundaryObstruction ProofTheoremManifest
requiredProofTheoremManifest =
  proofTheoremManifestFromTheorems
    . fmap proofObligationTheorem
    <$> requiredProofObligations

requiredRuntimeLawObligationIdentifiers :: Either ProofBoundaryObstruction [String]
requiredRuntimeLawObligationIdentifiers =
  foldMap (maybe [] (: []) . proofObligationRuntimeLawId)
    <$> requiredProofObligations

requiredRestrictionRuntimeLawIdentifiers :: [String]
requiredRestrictionRuntimeLawIdentifiers = restrictionKernelRuntimeLawIdentifiers

requiredRestrictionLeanTheoremManifest :: Either ProofBoundaryObstruction ProofTheoremManifest
requiredRestrictionLeanTheoremManifest =
  first ProofBoundaryInvalidObligations $
    proofTheoremManifestFromIdentifiers restrictionKernelLeanTheoremIdentifiers

requiredRestrictionManifestTheoremManifest :: Either ProofBoundaryObstruction ProofTheoremManifest
requiredRestrictionManifestTheoremManifest =
  first ProofBoundaryInvalidObligations $
    proofTheoremManifestFromIdentifiers restrictionKernelManifestTheoremIdentifiers

proofManifestPath :: FilePath
proofManifestPath = "proofs/lean/theorem-manifest.json"

proofManifestHashPath :: FilePath
proofManifestHashPath = "proofs/lean/theorem-manifest.json.sha256"

restrictionKernelSchemaPath :: FilePath
restrictionKernelSchemaPath = "proofs/lean/restriction-kernel-schema.json"

restrictionKernelSchemaHashPath :: FilePath
restrictionKernelSchemaHashPath = "proofs/lean/restriction-kernel-schema.json.sha256"

parseTheoremManifest :: String -> Either ProofBoundaryObstruction ProofTheoremManifest
parseTheoremManifest source = do
  theoremNames <-
    first
      ProofBoundaryManifestParseFailed
      (ProofManifest.parseTheoremManifestNames source)
  first
    ProofBoundaryInvalidManifestTheorems
    (proofTheoremManifestFromIdentifiers theoremNames)

checkProofBoundary :: ProofTheoremManifest -> Either ProofBoundaryObstruction ProofBoundaryDischarge
checkProofBoundary manifestTheorems = do
  obligations <- requiredProofObligations

  let requiredTheorems =
        canonicalTheoremNames (fmap proofObligationTheorem obligations)
      actualTheorems =
        proofTheoremManifestTheorems manifestTheorems
      missingTheorems =
        requiredTheorems `differenceSorted` actualTheorems
      unexpectedTheorems =
        actualTheorems `differenceSorted` requiredTheorems

  case (missingTheorems, unexpectedTheorems) of
    ([], []) ->
      Right
        ProofBoundaryDischarge
          { pbdObligations = obligations,
            pbdManifestTheorems = proofTheoremManifestFromTheorems actualTheorems
          }
    (_ : _, _) ->
      Left (ProofBoundaryMissingTheorems missingTheorems)
    ([], _ : _) ->
      Left (ProofBoundaryUnexpectedTheorems unexpectedTheorems)

canonicalTheoremNames :: [ProofTheoremName] -> [ProofTheoremName]
canonicalTheoremNames =
  sort . nub

collectValidated :: [Either invalid value] -> Either [invalid] [value]
collectValidated validatedValues =
  case partitionEithers validatedValues of
    ([], values) ->
      Right values
    (invalidValues, _) ->
      Left invalidValues

differenceSorted :: Ord value => [value] -> [value] -> [value]
differenceSorted leftValues rightValues =
  filter (`notElem` rightValues) leftValues
