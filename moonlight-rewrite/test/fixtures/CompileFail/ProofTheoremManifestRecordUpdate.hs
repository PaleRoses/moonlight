module ProofTheoremManifestRecordUpdate where

import Moonlight.Rewrite.ProofContext
  ( ProofTheoremManifest,
    proofTheoremManifestTheorems,
  )

forgeProofTheoremManifest :: ProofTheoremManifest -> ProofTheoremManifest
forgeProofTheoremManifest manifest =
  manifest
    { proofTheoremManifestTheorems =
        proofTheoremManifestTheorems manifest
    }
