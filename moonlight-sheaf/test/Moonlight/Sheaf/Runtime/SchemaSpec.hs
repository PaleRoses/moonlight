{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Sheaf.Runtime.SchemaSpec
  ( tests,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as ByteString
import Moonlight.Sheaf.Context.Schema
  ( RestrictionKernelSchema (..),
    renderRestrictionKernelSchemaJson,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "schema"
    [ testCase "RestrictionKernelSchema decodes snake-case wire keys" $
        Aeson.eitherDecodeStrict' sampleSchemaJson @?= Right sampleSchema,
      testCase "RestrictionKernelSchema encodes snake-case wire keys" $
        Aeson.toJSON sampleSchema @?= sampleSchemaValue,
      testCase "renderRestrictionKernelSchemaJson roundtrips through snake-case wire keys" $
        Aeson.eitherDecodeStrict' (ByteString.pack (renderRestrictionKernelSchemaJson sampleSchema))
          @?= Right sampleSchema
    ]

sampleSchema :: RestrictionKernelSchema
sampleSchema =
  RestrictionKernelSchema
    { restrictionKernelMorphismConstructor = "mkRestriction",
      restrictionKernelIdentityMorphism = "identity",
      restrictionKernelComposeMorphism = "compose",
      restrictionKernelCanonicalSection = "canonical",
      restrictionKernelRestrictSection = "restrict",
      restrictionKernelGlobalSectionPredicate = "isGlobal",
      restrictionKernelRuntimeLawWitnesses = ["identity-law"],
      restrictionKernelLeanTheoremWitnesses = ["lean-theorem"],
      restrictionKernelManifestSubset = ["manifest"]
    }

sampleSchemaValue :: Aeson.Value
sampleSchemaValue =
  Aeson.object
    [ "morphism_constructor" .= ("mkRestriction" :: String),
      "identity_morphism" .= ("identity" :: String),
      "compose_morphism" .= ("compose" :: String),
      "canonical_section" .= ("canonical" :: String),
      "restrict_section" .= ("restrict" :: String),
      "global_section_predicate" .= ("isGlobal" :: String),
      "runtime_law_witnesses" .= ["identity-law" :: String],
      "lean_theorem_witnesses" .= ["lean-theorem" :: String],
      "manifest_subset" .= ["manifest" :: String]
    ]

sampleSchemaJson :: ByteString.ByteString
sampleSchemaJson =
  ByteString.pack
    "{\
    \\"morphism_constructor\":\"mkRestriction\",\
    \\"identity_morphism\":\"identity\",\
    \\"compose_morphism\":\"compose\",\
    \\"canonical_section\":\"canonical\",\
    \\"restrict_section\":\"restrict\",\
    \\"global_section_predicate\":\"isGlobal\",\
    \\"runtime_law_witnesses\":[\"identity-law\"],\
    \\"lean_theorem_witnesses\":[\"lean-theorem\"],\
    \\"manifest_subset\":[\"manifest\"]\
    \}"
