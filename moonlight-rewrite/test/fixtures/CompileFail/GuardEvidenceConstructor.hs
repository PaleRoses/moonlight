module GuardEvidenceConstructor where

import Moonlight.Rewrite.System (GuardEvidence (..))

forgedGuardEvidence :: GuardEvidence
forgedGuardEvidence = GuardEvidence [] mempty
