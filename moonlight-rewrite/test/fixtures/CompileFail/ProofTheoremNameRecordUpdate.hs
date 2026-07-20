module ProofTheoremNameRecordUpdate where

import Moonlight.Rewrite.ProofContext (ProofTheoremName, proofTheoremNameString)

forgeProofTheoremName :: ProofTheoremName -> ProofTheoremName
forgeProofTheoremName theoremName =
  theoremName {proofTheoremNameString = proofTheoremNameString theoremName}
