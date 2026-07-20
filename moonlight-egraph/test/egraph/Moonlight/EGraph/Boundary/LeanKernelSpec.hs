{-# LANGUAGE OverloadedStrings #-}

module Moonlight.EGraph.Boundary.LeanKernelSpec
  ( tests,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    eitherDecode,
    encode,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (subsequences)
import Moonlight.Rewrite.ProofContext
  ( supportBasis,
    supportContains,
    supportGenerators,
    supportMeet,
    supportUnion,
  )
import Moonlight.Sheaf.Context.Core
  ( contextRefinesTo,
  )
import Moonlight.Sheaf.Section.Restriction.Witness
  ( ContextMorphism,
    composeContextMorphism,
    contextMorphismSource,
    contextMorphismTarget,
    mkContextMorphism,
  )
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (..))
import Moonlight.Pale.Test.Section.ResourcePath
  ( renderResourcePathError,
    resolvePackageDirectory,
  )
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd), readCreateProcessWithExitCode)
import System.Process qualified as Process
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Moonlight.FiniteLattice
  ( ContextLattice (clBottom, clTop),
    ContextLatticeLookupError,
    latticeContext
  )

eGraphPackageMarker :: FilePath
eGraphPackageMarker = "foundation/moonlight-egraph/moonlight-egraph.cabal"

type KernelLattice :: Type
data KernelLattice = KernelLattice
  { klSize :: Int,
    klTop :: Int,
    klBottom :: Int,
    klJoinTable :: [[Int]],
    klLeqTable :: [[Bool]]
  }
  deriving stock (Eq, Show)

type KernelMorphism :: Type
data KernelMorphism = KernelMorphism
  { kmSource :: Int,
    kmTarget :: Int
  }
  deriving stock (Eq, Show)

type KernelRequest :: Type
data KernelRequest
  = MkContextMorphism KernelLattice Int Int
  | IdentityContextMorphism Int
  | ComposeContextMorphism KernelLattice KernelMorphism KernelMorphism
  | PrincipalSupport Int
  | NormalizeSupport KernelLattice [Int]
  | SupportContains KernelLattice [Int] Int
  | SupportUnion KernelLattice [Int] [Int]
  | SupportMeet KernelLattice [Int] [Int]
  deriving stock (Eq, Show)

type KernelResponse :: Type
data KernelResponse
  = ResponseMorphism KernelMorphism
  | ResponseNone
  | ResponseSupport [Int]
  | ResponseContains Bool
  deriving stock (Eq, Show)

instance ToJSON KernelLattice where
  toJSON kernelLatticeValue =
    object
      [ "size" .= klSize kernelLatticeValue,
        "top" .= klTop kernelLatticeValue,
        "bottom" .= klBottom kernelLatticeValue,
        "joinTable" .= klJoinTable kernelLatticeValue,
        "leqTable" .= klLeqTable kernelLatticeValue
      ]

instance ToJSON KernelMorphism where
  toJSON kernelMorphismValue =
    object
      [ "source" .= kmSource kernelMorphismValue,
        "target" .= kmTarget kernelMorphismValue
      ]

instance ToJSON KernelRequest where
  toJSON kernelRequestValue =
    case kernelRequestValue of
      MkContextMorphism latticeValue sourceContext targetContext ->
        object
          [ "tag" .= ("mk_context_morphism" :: String),
            "lattice" .= latticeValue,
            "source" .= sourceContext,
            "target" .= targetContext
          ]
      IdentityContextMorphism contextValue ->
        object
          [ "tag" .= ("identity_context_morphism" :: String),
            "context" .= contextValue
          ]
      ComposeContextMorphism latticeValue leftMorphism rightMorphism ->
        object
          [ "tag" .= ("compose_context_morphism" :: String),
            "lattice" .= latticeValue,
            "left" .= leftMorphism,
            "right" .= rightMorphism
          ]
      PrincipalSupport contextValue ->
        object
          [ "tag" .= ("principal_support" :: String),
            "context" .= contextValue
          ]
      NormalizeSupport latticeValue generators ->
        object
          [ "tag" .= ("normalize_support" :: String),
            "lattice" .= latticeValue,
            "generators" .= generators
          ]
      SupportContains latticeValue generators contextValue ->
        object
          [ "tag" .= ("support_contains" :: String),
            "lattice" .= latticeValue,
            "generators" .= generators,
            "context" .= contextValue
          ]
      SupportUnion latticeValue leftGenerators rightGenerators ->
        object
          [ "tag" .= ("support_union" :: String),
            "lattice" .= latticeValue,
            "leftGenerators" .= leftGenerators,
            "rightGenerators" .= rightGenerators
          ]
      SupportMeet latticeValue leftGenerators rightGenerators ->
        object
          [ "tag" .= ("support_meet" :: String),
            "lattice" .= latticeValue,
            "leftGenerators" .= leftGenerators,
            "rightGenerators" .= rightGenerators
          ]

instance FromJSON KernelMorphism where
  parseJSON =
    withObject "KernelMorphism" $ \objectValue ->
      KernelMorphism
        <$> objectValue .: "source"
        <*> objectValue .: "target"

instance FromJSON KernelResponse where
  parseJSON =
    withObject "KernelResponse" $ \objectValue -> do
      tag <- objectValue .: "tag"
      case (tag :: String) of
        "morphism" ->
          ResponseMorphism
            <$> ( KernelMorphism
                    <$> objectValue .: "source"
                    <*> objectValue .: "target"
                )
        "none" -> pure ResponseNone
        "support" -> ResponseSupport <$> objectValue .: "generators"
        "contains" -> ResponseContains <$> objectValue .: "value"
        unexpectedTag ->
          fail ("unexpected kernel response tag: " <> unexpectedTag)

resolveProofRoot :: IO FilePath
resolveProofRoot =
  resolvePackageDirectory eGraphPackageMarker "proofs/lean"
    >>= either (assertFailure . renderResourcePathError) pure

ensurePhaseAKernelExecutable :: IO FilePath
ensurePhaseAKernelExecutable = do
  proofRoot <- resolveProofRoot
  maybeLake <- findExecutable "lake"
  case maybeLake of
    Nothing ->
      assertFailure "expected lake on PATH to build the Phase A Lean kernel"
    Just lakeExecutable -> do
      let executablePath = proofRoot </> ".lake/build/bin/egraph-phase-a-kernel"
      (exitCode, _, stderrOutput) <-
        readCreateProcessWithExitCode
          ((Process.proc lakeExecutable ["build", "egraph-phase-a-kernel"]) {cwd = Just proofRoot})
          ""
      case exitCode of
        ExitSuccess -> pure executablePath
        ExitFailure failureCode ->
          assertFailure
            ( "failed to build Lean Phase A kernel with exit code "
                <> show failureCode
                <> "\n"
                <> stderrOutput
            )

runKernelRequests :: [KernelRequest] -> IO [KernelResponse]
runKernelRequests requests = do
  executablePath <- ensurePhaseAKernelExecutable
  let inputPayload = LBS.unpack (encode requests)
  (exitCode, stdoutOutput, stderrOutput) <-
    readCreateProcessWithExitCode
      (Process.proc executablePath [])
      inputPayload
  case exitCode of
    ExitFailure failureCode ->
      assertFailure
        ( "Phase A Lean kernel exited with code "
            <> show failureCode
            <> "\n"
            <> stderrOutput
        )
    ExitSuccess ->
      case eitherDecode (LBS.pack stdoutOutput) of
        Left decodeError ->
          assertFailure
            ( "failed to decode Phase A Lean kernel response: "
                <> decodeError
                <> "\nstdout:\n"
                <> stdoutOutput
            )
        Right responses ->
          pure responses

scopeUniverse :: [Scope]
scopeUniverse = [minBound .. maxBound]

scopeLattice :: ContextLattice Scope
scopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid Lean kernel Scope lattice fixture: " <> show compileError)

scopeIndex :: Scope -> Int
scopeIndex = fromEnum

scopeKernelLattice :: KernelLattice
scopeKernelLattice =
  KernelLattice
    { klSize = length scopeUniverse,
      klTop = scopeIndex (clTop scopeLattice),
      klBottom = scopeIndex (clBottom scopeLattice),
      klJoinTable =
        scopeUniverse
          & fmap
            ( \leftContext ->
                scopeUniverse
                  & fmap
                    (\rightContext -> scopeIndex (max leftContext rightContext))
            ),
      klLeqTable =
        scopeUniverse
          & fmap
            ( \leftContext ->
                scopeUniverse
                  & fmap
                    (\rightContext -> leftContext <= rightContext)
            )
    }

toKernelMorphism :: Scope -> Scope -> KernelMorphism
toKernelMorphism sourceContext targetContext =
  KernelMorphism
    { kmSource = scopeIndex sourceContext,
      kmTarget = scopeIndex targetContext
    }

encodedSupportGenerators :: [Scope] -> Either (ContextLatticeLookupError Scope) [Int]
encodedSupportGenerators =
  fmap (fmap scopeIndex . supportGenerators) . supportBasis scopeLattice

supportSamples :: [[Scope]]
supportSamples =
  subsequences scopeUniverse
    <> fmap (\scopeValue -> [scopeValue, scopeValue]) scopeUniverse
    <> [[GlobalCtx, ModuleCtx, ModuleCtx], [LocalCtx, GlobalCtx, LocalCtx]]

contextKernelTests :: IO ()
contextKernelTests = do
  let pairCases =
        scopeUniverse
          >>= (\sourceContext -> scopeUniverse & fmap (\targetContext -> (sourceContext, targetContext)))
      validMorphisms =
        pairCases
          & fmap
            ( \(sourceContext, targetContext) ->
                kernelMorphismFor sourceContext targetContext
            )
          & foldr
            (\maybeMorphism morphisms -> maybe morphisms (\morphism -> morphism : morphisms) maybeMorphism)
            []
      composeCases =
        validMorphisms
          >>= (\leftMorphism -> validMorphisms & fmap (\rightMorphism -> (leftMorphism, rightMorphism)))
      requests =
        ( pairCases
            & fmap (\(sourceContext, targetContext) -> MkContextMorphism scopeKernelLattice (scopeIndex sourceContext) (scopeIndex targetContext))
        )
          <> (scopeUniverse & fmap (IdentityContextMorphism . scopeIndex))
          <> ( composeCases
                & fmap (\(leftMorphism, rightMorphism) -> ComposeContextMorphism scopeKernelLattice leftMorphism rightMorphism)
             )
      expectedResponses =
        ( pairCases
            & fmap
              ( \(sourceContext, targetContext) ->
                maybe
                  ResponseNone
                  ResponseMorphism
                  (kernelMorphismFor sourceContext targetContext)
              )
        )
          <> ( scopeUniverse
                & fmap
                  (\contextValue -> ResponseMorphism (toKernelMorphism contextValue contextValue))
             )
          <> ( composeCases
                & fmap
                  ( \(leftMorphism, rightMorphism) ->
                      let leftHaskell =
                            KernelMorphism
                              { kmSource = kmSource leftMorphism,
                                kmTarget = kmTarget leftMorphism
                              }
                          rightHaskell =
                            KernelMorphism
                              { kmSource = kmSource rightMorphism,
                                kmTarget = kmTarget rightMorphism
                              }
                          leftWitness =
                            toScopeMorphism leftHaskell
                          rightWitness =
                            toScopeMorphism rightHaskell
                       in maybe
                            ResponseNone
                            (\contextMorphism -> ResponseMorphism (toKernelMorphism (fst contextMorphism) (snd contextMorphism)))
                            (composeScopeMorphism leftWitness rightWitness)
                  )
             )
  responses <- runKernelRequests requests
  assertEqual "Lean Phase A context witness kernel must match Haskell semantics" expectedResponses responses

supportKernelTests :: IO ()
supportKernelTests = do
  let supportPairs =
        supportSamples
          >>= (\leftSupport -> supportSamples & fmap (\rightSupport -> (leftSupport, rightSupport)))
      requests =
        (scopeUniverse & fmap (PrincipalSupport . scopeIndex))
          <> (supportSamples & fmap (NormalizeSupport scopeKernelLattice . fmap scopeIndex))
          <> ( supportSamples
                >>= (\supportValue -> scopeUniverse & fmap (\contextValue -> SupportContains scopeKernelLattice (fmap scopeIndex supportValue) (scopeIndex contextValue)))
             )
          <> ( supportPairs
                & fmap
                  ( \(leftSupport, rightSupport) ->
                      SupportUnion scopeKernelLattice (fmap scopeIndex leftSupport) (fmap scopeIndex rightSupport)
                  )
             )
          <> ( supportPairs
                & fmap
                  ( \(leftSupport, rightSupport) ->
                      SupportMeet scopeKernelLattice (fmap scopeIndex leftSupport) (fmap scopeIndex rightSupport)
                  )
             )
      expectedResponses =
        sequenceA $
          (scopeUniverse & fmap (\contextValue -> Right (ResponseSupport [scopeIndex contextValue])))
            <> (supportSamples & fmap (fmap ResponseSupport . encodedSupportGenerators))
            <> ( supportSamples
                  >>= ( \supportValue ->
                          scopeUniverse
                            & fmap
                              ( \contextValue -> do
                                  supportValue' <- supportBasis scopeLattice supportValue
                                  ResponseContains <$> supportContains scopeLattice supportValue' contextValue
                              )
                      )
               )
            <> ( supportPairs
                  & fmap
                    ( \(leftSupport, rightSupport) -> do
                        leftSupport' <- supportBasis scopeLattice leftSupport
                        rightSupport' <- supportBasis scopeLattice rightSupport
                        unionSupport <- supportUnion scopeLattice leftSupport' rightSupport'
                        pure (ResponseSupport (fmap scopeIndex (supportGenerators unionSupport)))
                    )
               )
            <> ( supportPairs
                  & fmap
                    ( \(leftSupport, rightSupport) -> do
                        leftSupport' <- supportBasis scopeLattice leftSupport
                        rightSupport' <- supportBasis scopeLattice rightSupport
                        meetSupport <- supportMeet scopeLattice leftSupport' rightSupport'
                        pure (ResponseSupport (fmap scopeIndex (supportGenerators meetSupport)))
                    )
               )
  responses <- runKernelRequests requests
  case expectedResponses of
    Left supportError ->
      assertFailure ("expected support kernel fixtures to be valid, got " <> show supportError)
    Right expected ->
      assertEqual "Lean Phase A support kernel must match Haskell semantics" expected responses

toScopeMorphism :: KernelMorphism -> Maybe (ContextMorphism Scope)
toScopeMorphism kernelMorphism =
  let sourceContext = toEnumScope (kmSource kernelMorphism)
      targetContext = toEnumScope (kmTarget kernelMorphism)
   in sourceContext >>= \sourceValue ->
        targetContext >>= \targetValue ->
          either (const Nothing) id (mkContextMorphism (contextRefinesTo scopeLattice) sourceValue targetValue)

kernelMorphismFor :: Scope -> Scope -> Maybe KernelMorphism
kernelMorphismFor sourceContext targetContext =
  fmap (const (toKernelMorphism sourceContext targetContext))
    (either (const Nothing) id (mkContextMorphism (contextRefinesTo scopeLattice) sourceContext targetContext))

composeScopeMorphism :: Maybe (ContextMorphism Scope) -> Maybe (ContextMorphism Scope) -> Maybe (Scope, Scope)
composeScopeMorphism leftWitness rightWitness =
  do
    leftValue <- leftWitness
    rightValue <- rightWitness
    contextMorphismPair
      <$> either
        (const Nothing)
        id
        (composeContextMorphism (contextRefinesTo scopeLattice) leftValue rightValue)

toEnumScope :: Int -> Maybe Scope
toEnumScope contextIndex =
  scopeUniverse
    & drop contextIndex
    & (\remainingScopes -> case remainingScopes of
      scopeValue : _ -> Just scopeValue
      [] -> Nothing)

contextMorphismPair :: ContextMorphism Scope -> (Scope, Scope)
contextMorphismPair contextMorphism =
  (contextMorphismSource contextMorphism, contextMorphismTarget contextMorphism)

tests :: TestTree
tests =
  testGroup
    "LeanKernel"
    [ testCase "context witness kernel matches Haskell semantics on the three-level lattice" contextKernelTests,
      testCase "support basis kernel matches Haskell semantics on the three-level lattice" supportKernelTests
    ]
