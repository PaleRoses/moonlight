{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Write.Seal
  ( SealedSource,
    sealedSourceText,
    SealOutcome (..),
    sealPatchedSourceParseCount,
    sealModulePatch,
    sealModulePatchOutcome,
  )
where

import Data.Fix (Fix)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Hs qualified as Ghc
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import Melusine.Nebula.Core (NebulaError (..))
import Melusine.Nebula.Write.Back (AppendedDefinition (..), ModulePatch (..), modulePatchHasContent, patchedModuleSource)
import Melusine.Nebula.Write.Declaration (sealDeclarationObligationsFromParsedModule)
import Melusine.Nebula.Write.Protocol (sealProtocolObligationsFromParsedModule)
import Moonlight.Core (Pattern)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    TopLevelBinding (..),
    convertModule,
    renderRoundTripEquivalent,
  )
import Moonlight.EGraph.Pure.Rewrite.Instantiate (patternFromFix)
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

type SealedSource :: Type
newtype SealedSource = SealedSource String
  deriving stock (Eq, Show)

sealedSourceText :: SealedSource -> String
sealedSourceText (SealedSource sourceText) =
  sourceText

type SealOutcome :: Type
data SealOutcome
  = SealEmpty
  | Sealed !SealedSource
  | SealRefused !PatchedSourceParseCount !NebulaError
  deriving stock (Eq, Show)

type PatchedSourceParseCount :: Type
data PatchedSourceParseCount
  = NoPatchedSourceParse
  | OnePatchedSourceParse
  deriving stock (Eq, Show)

sealPatchedSourceParseCount :: SealOutcome -> Int
sealPatchedSourceParseCount outcome =
  case outcome of
    SealEmpty ->
      0
    Sealed _ ->
      1
    SealRefused parseCount _ ->
      patchedSourceParseCountValue parseCount

patchedSourceParseCountValue :: PatchedSourceParseCount -> Int
patchedSourceParseCountValue = \case
  NoPatchedSourceParse ->
    0
  OnePatchedSourceParse ->
    1

sealModulePatch :: FilePath -> String -> ModulePatch -> Either NebulaError SealedSource
sealModulePatch path source modulePatch =
  case sealModulePatchOutcome path source modulePatch of
    SealEmpty ->
      Right (SealedSource source)
    Sealed sealedSource ->
      Right sealedSource
    SealRefused _ failure ->
      Left failure

sealModulePatchOutcome :: FilePath -> String -> ModulePatch -> SealOutcome
sealModulePatchOutcome path source modulePatch
  | modulePatchHasContent modulePatch =
      case patchedModuleSource modulePatch source of
        Left failure ->
          SealRefused NoPatchedSourceParse failure
        Right patchedSource ->
          sealPatchedSourceOutcome path patchedSource modulePatch
  | otherwise =
      SealEmpty

sealPatchedSourceOutcome :: FilePath -> String -> ModulePatch -> SealOutcome
sealPatchedSourceOutcome path patchedSource modulePatch =
  case parseHsModule path patchedSource of
    Left parseFailure ->
      SealRefused
        OnePatchedSourceParse
        (NebulaParseError parseFailure)
    Right parsedModule ->
      case sealParsedModule path parsedModule modulePatch of
        Left sealFailure ->
          SealRefused OnePatchedSourceParse sealFailure
        Right () ->
          Sealed (SealedSource patchedSource)

sealParsedModule :: FilePath -> Ghc.HsModule Ghc.GhcPs -> ModulePatch -> Either NebulaError ()
sealParsedModule path parsedModule modulePatch = do
  sealDeclarationObligationsFromParsedModule parsedModule (mpDeclarationObligations modulePatch)
  sealProtocolObligationsFromParsedModule path parsedModule (mpProtocolObligations modulePatch)
  reparsed <-
    either
      (Left . NebulaSealError path . ("patched source failed to re-convert: " <>) . show)
      Right
      (convertModule parsedModule)
  let reparsedTerms = bindingTermsByName reparsed
      plannedTerms =
        mpSpliced modulePatch
          <> fmap (\definition -> (adName definition, adTerm definition)) (mpAppendedDefinitions modulePatch)
  traverse_ (sealName reparsedTerms) plannedTerms

bindingTermsByName :: ConvertedModule -> Map String (Pattern HsExprF)
bindingTermsByName converted =
  Map.fromList
    [ (occNameString (rdrNameOcc bindingName), tlbTerm binding)
      | binding <- cmBindings converted,
        bindingName <- tlbNames binding
    ]

sealName :: Map String (Pattern HsExprF) -> (String, Fix HsExprF) -> Either NebulaError ()
sealName reparsedTerms (plannedName, plannedTerm) =
  case Map.lookup plannedName reparsedTerms of
    Nothing ->
      Left (NebulaSealError plannedName "binding is missing from the patched module")
    Just reparsedTerm
      | renderRoundTripEquivalent reparsedTerm (patternFromFix plannedTerm) ->
          Right ()
      | otherwise ->
          Left (NebulaSealError plannedName "patched binding is not round-trip equivalent to the planned term")
