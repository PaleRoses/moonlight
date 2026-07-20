module Moonlight.Sheaf.Obstruction.CertificationSpec
  ( tests,
  )
where

import Data.Kind (Type)

import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy (..),
    EnvironmentCacheKey (..),
    SectionCertificationAlgebra (..),
    environmentFingerprintFromCachePolicy,
    mkSectionCertificationAlgebraWithCachePolicy,
    mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy,
    regionCarrierPlanFromList,
  )
import Moonlight.Sheaf.Verdict
  ( Verdict (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    testCase,
  )

type DummyRequest :: Type -> Type
newtype DummyRequest runtime = DummyRequest
  { drKey :: Int
  }

tests :: TestTree
tests =
  testGroup
    "certification"
    [ testCase "cache-policy constructor derives the environment fingerprint and explicit kernel acceptance" testDefaultCertificationAlgebra,
      testCase "capability-aware constructor preserves custom capability and kernel verdicts" testCapabilityAwareCertificationAlgebra
    ]

testDefaultCertificationAlgebra :: Assertion
testDefaultCertificationAlgebra =
  let requestValue :: DummyRequest ()
      requestValue =
        DummyRequest 41
      algebra :: SectionCertificationAlgebra DummyRequest Int Int Int Int Int String ()
      algebra =
        mkSectionCertificationAlgebraWithCachePolicy
          "default-capability"
          (: [])
          (\request queryValue -> regionCarrierPlanFromList [drKey request + queryValue])
          (\request queryValue regionValue -> [drKey request + queryValue + regionValue])
          (\request occurrenceValue regionValue -> drKey request + occurrenceValue + regionValue)
          (\request guardValue regionValue -> drKey request + guardValue + regionValue)
          id
          (\request -> EnvironmentScoped (EnvironmentCacheKey (drKey request)))
   in do
        assertEqual "expected the default capability to be used" "default-capability" (socCapabilityEnvironment algebra requestValue 7 [5] [3])
        assertEqual "expected default acceptance to be explicit at the kernel boundary" (Accepted ()) (socKernelVerdict algebra requestValue 7)
        assertEqual "expected the cache policy fingerprint to be reflected" (Just 41) (socEnvironmentFingerprint algebra requestValue)
        assertEqual "expected the explicit cache-policy helper to round-trip the fingerprint" (Just 41) (environmentFingerprintFromCachePolicy (socQueryCachePolicy algebra requestValue))

testCapabilityAwareCertificationAlgebra :: Assertion
testCapabilityAwareCertificationAlgebra =
  let requestValue :: DummyRequest ()
      requestValue =
        DummyRequest 9
      algebra :: SectionCertificationAlgebra DummyRequest Int Int Int Int Int Int String
      algebra =
        mkSectionCertificationAlgebraWithCapabilitiesAndCachePolicy
          (: [])
          (\request queryValue -> regionCarrierPlanFromList [drKey request + queryValue])
          (\request queryValue regionValue -> [drKey request + queryValue + regionValue])
          (\request occurrenceValue regionValue -> drKey request + occurrenceValue + regionValue)
          (\request guardValue regionValue -> drKey request + guardValue + regionValue)
          (\request regionValue occurrences guards -> drKey request + regionValue + length occurrences + length guards)
          ( \request regionValue ->
              if drKey request <= regionValue
                then Accepted ()
                else Rejected "region below request"
          )
          id
          (const DoNotCache)
   in do
        assertEqual "expected the custom capability interpreter to be preserved" 17 (socCapabilityEnvironment algebra requestValue 4 [2] [1, 2, 3])
        assertEqual "expected acceptedness to survive the kernel verdict boundary" (Accepted ()) (socKernelVerdict algebra requestValue 9)
        assertEqual "expected rejection to survive the kernel verdict boundary" (Rejected "region below request") (socKernelVerdict algebra requestValue 8)
        assertEqual
          "expected rejected verdict explanation to preserve the kernel obstruction"
          (Rejected ("region below request", "blocked: region below request"))
          (fmap (\obstruction -> (obstruction, "blocked: " <> obstruction)) (socKernelVerdict algebra requestValue 8))
        assertEqual "expected the non-caching policy to clear the environment fingerprint" Nothing (socEnvironmentFingerprint algebra requestValue)
