{-# LANGUAGE TemplateHaskell #-}

module Moonlight.Sheaf.Context.Schema
  ( RestrictionKernelSchema (..),
    restrictionKernelSchema,
    renderRestrictionKernelSchemaJson,
    restrictionKernelRuntimeLawIdentifiers,
    restrictionKernelLeanTheoremIdentifiers,
    restrictionKernelManifestTheoremIdentifiers,
  )
where

import Data.Aeson
  ( eitherDecodeStrict',
    encode,
  )
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteStringChar8
import Language.Haskell.TH.Syntax
  ( Q,
    addDependentFile,
    lift,
    makeRelativeToProject,
    runIO,
  )
import Moonlight.Sheaf.Context.Schema.Types
  ( RestrictionKernelSchema (..),
  )

restrictionKernelRuntimeLawIdentifiers :: [String]
restrictionKernelRuntimeLawIdentifiers =
  restrictionKernelRuntimeLawWitnesses restrictionKernelSchema

restrictionKernelLeanTheoremIdentifiers :: [String]
restrictionKernelLeanTheoremIdentifiers =
  restrictionKernelLeanTheoremWitnesses restrictionKernelSchema

restrictionKernelManifestTheoremIdentifiers :: [String]
restrictionKernelManifestTheoremIdentifiers =
  restrictionKernelManifestSubset restrictionKernelSchema

restrictionKernelSchema :: RestrictionKernelSchema
restrictionKernelSchema =
  $(do
      let schemaRelativePath = "../moonlight-egraph/proofs/lean/restriction-kernel-schema.json"
          schemaDecodeFailure :: FilePath -> String -> Q a
          schemaDecodeFailure schemaPath message =
            fail ("failed to decode restriction-kernel schema from " <> schemaPath <> ": " <> message)
      schemaPath <- makeRelativeToProject schemaRelativePath
      addDependentFile schemaPath
      schemaBytes <- runIO (ByteString.readFile schemaPath)
      case eitherDecodeStrict' schemaBytes of
        Left message -> schemaDecodeFailure schemaPath message
        Right schemaValue -> lift (schemaValue :: RestrictionKernelSchema)
   )

renderRestrictionKernelSchemaJson :: RestrictionKernelSchema -> String
renderRestrictionKernelSchemaJson =
  LazyByteStringChar8.unpack . encode
