module Laws.Registry
  ( carrierTestSuites,
    lawfulCarrierSpecs,
  )
where

import Laws.Suite (LawfulCarrierSpec)
import Test.Tasty (TestTree)
import qualified CategoricalSimplexSpec
import qualified DeltaSpec
import qualified HomotopySpec
import qualified KanSpec
import qualified NerveSpec
import qualified OrdinalSpec
import qualified PresheafSpec
import qualified SpacesSpec

additionalTestSuites :: [TestTree]
additionalTestSuites =
  [ CategoricalSimplexSpec.tests,
    DeltaSpec.tests,
    OrdinalSpec.tests,
    PresheafSpec.tests,
    HomotopySpec.tests,
    KanSpec.tests
  ]

carrierTestSuites :: [TestTree]
carrierTestSuites =
  [ NerveSpec.carrierTests,
    SpacesSpec.carrierTests
  ]
    <> additionalTestSuites

lawfulCarrierSpecs :: [LawfulCarrierSpec]
lawfulCarrierSpecs =
  [ NerveSpec.lawfulCarrierSpec,
    SpacesSpec.lawfulCarrierSpec
  ]
