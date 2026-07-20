{-# LANGUAGE LambdaCase #-}

module Moonlight.Surface.Laws
  ( SurfaceCapability (..),
    SurfaceBuildRefusal (..),
    SurfaceLawError,
    SurfaceRewriteRule,
    surfaceLawRuleIdBase,
    surfaceUnionCommutativityLawId,
    surfaceUnionAssociativityLawId,
    surfaceUnionIdempotenceLawId,
    surfaceTranslateUnionHoistLawId,
    surfaceTranslateComposeLawId,
    surfaceScaleComposeLawId,
    surfaceTranslateIdentityLawId,
    surfaceScaleIdentityLawId,
    surfaceScaleInterHoistLawId,
    surfaceScaleDiffHoistLawId,
    surfaceNonDegenerateScaleFactId,
    surfaceLawRules,
    surfaceEquationRule,
    surfaceUnionCommutativityRule,
    surfaceTranslateUnionHoistRule,
    surfaceTranslateComposeRule,
    surfaceScaleComposeRule,
    surfaceTranslateIdentityRule,
    surfaceScaleIdentityRule,
    surfaceScaleInterHoistRule,
    surfaceScaleDiffHoistRule,
  )
where

import Data.Kind (Type)
import Moonlight.Core (Pattern (..), mkPatternVar)
import Moonlight.EGraph.Introspection.Core.Equation
  ( ApplicativeEquationError,
    EquationError,
    EquationFront,
    applicativeEquationFront,
    equationRuleWith,
  )
import Moonlight.Rewrite.System (LawId, mkLawId)
import Moonlight.Rewrite.System (RewriteCondition (..), data GuardVar, guardHasCapability, guardHasFact)
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Surface.Language (SurfaceCapability (..), SurfaceF (..))
import Text.Read (readMaybe)

type SurfaceBuildRefusal :: Type
data SurfaceBuildRefusal
  = UnknownSurfaceSymbol !String !Int
  | SurfaceArityMismatch !String !Int
  deriving stock (Eq, Show)

type SurfaceLawError :: Type
type SurfaceLawError = EquationError (ApplicativeEquationError SurfaceBuildRefusal)

type SurfaceRewriteRule :: Type
type SurfaceRewriteRule = RawRewriteRule (RewriteCondition SurfaceCapability SurfaceF) SurfaceF

surfaceLawRuleIdBase :: Int
surfaceLawRuleIdBase =
  3000000

surfaceUnionCommutativityLawId :: LawId
surfaceUnionCommutativityLawId =
  mkLawId (surfaceLawRuleIdBase + 1)

surfaceUnionAssociativityLawId :: LawId
surfaceUnionAssociativityLawId =
  mkLawId (surfaceLawRuleIdBase + 2)

surfaceUnionIdempotenceLawId :: LawId
surfaceUnionIdempotenceLawId =
  mkLawId (surfaceLawRuleIdBase + 3)

surfaceTranslateUnionHoistLawId :: LawId
surfaceTranslateUnionHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 4)

surfaceTranslateInterHoistLawId :: LawId
surfaceTranslateInterHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 5)

surfaceTranslateDiffHoistLawId :: LawId
surfaceTranslateDiffHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 6)

surfaceRotateUnionHoistLawId :: LawId
surfaceRotateUnionHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 7)

surfaceRotateInterHoistLawId :: LawId
surfaceRotateInterHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 8)

surfaceRotateDiffHoistLawId :: LawId
surfaceRotateDiffHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 9)

surfaceScaleUnionHoistLawId :: LawId
surfaceScaleUnionHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 10)

surfaceTranslateComposeLawId :: LawId
surfaceTranslateComposeLawId =
  mkLawId (surfaceLawRuleIdBase + 11)

surfaceScaleComposeLawId :: LawId
surfaceScaleComposeLawId =
  mkLawId (surfaceLawRuleIdBase + 12)

surfaceTranslateIdentityLawId :: LawId
surfaceTranslateIdentityLawId =
  mkLawId (surfaceLawRuleIdBase + 13)

surfaceScaleIdentityLawId :: LawId
surfaceScaleIdentityLawId =
  mkLawId (surfaceLawRuleIdBase + 14)

surfaceScaleInterHoistLawId :: LawId
surfaceScaleInterHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 15)

surfaceScaleDiffHoistLawId :: LawId
surfaceScaleDiffHoistLawId =
  mkLawId (surfaceLawRuleIdBase + 16)

surfaceNonDegenerateScaleFactId :: FactId
surfaceNonDegenerateScaleFactId =
  FactId (surfaceLawRuleIdBase + 1)

surfaceLawRules :: Either SurfaceLawError [SurfaceRewriteRule]
surfaceLawRules =
  sequence
    [ surfaceUnionCommutativityRule,
      surfaceUnionAssociativityRule,
      surfaceUnionIdempotenceRule,
      surfaceTranslateUnionHoistRule,
      hoistRule surfaceTranslateInterHoistLawId "inter" "translate",
      hoistRule surfaceTranslateDiffHoistLawId "diff" "translate",
      hoistRule surfaceRotateUnionHoistLawId "union" "rotate",
      hoistRule surfaceRotateInterHoistLawId "inter" "rotate",
      hoistRule surfaceRotateDiffHoistLawId "diff" "rotate",
      hoistRule surfaceScaleUnionHoistLawId "union" "scale",
      surfaceTranslateComposeRule,
      surfaceScaleComposeRule,
      surfaceTranslateIdentityRule,
      surfaceScaleIdentityRule,
      surfaceScaleInterHoistRule,
      surfaceScaleDiffHoistRule
    ]

surfaceUnionCommutativityRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceUnionCommutativityRule =
  surfaceEquationRule surfaceUnionCommutativityLawId 0 ["a", "b"] "union a b = union b a"

surfaceUnionAssociativityRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceUnionAssociativityRule =
  surfaceEquationRule surfaceUnionAssociativityLawId 0 ["a", "b", "c"] "union (union a b) c = union a (union b c)"

surfaceUnionIdempotenceRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceUnionIdempotenceRule =
  surfaceEquationRule surfaceUnionIdempotenceLawId 0 ["a"] "union a a = a"

surfaceTranslateUnionHoistRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceTranslateUnionHoistRule =
  hoistRule surfaceTranslateUnionHoistLawId "union" "translate"

surfaceTranslateComposeRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceTranslateComposeRule =
  surfaceEquationRule surfaceTranslateComposeLawId 0 ["u", "v", "x"] "translate u (translate v x) = translate (vadd u v) x"

surfaceScaleComposeRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceScaleComposeRule =
  surfaceEquationRule surfaceScaleComposeLawId 0 ["u", "v", "x"] "scale u (scale v x) = scale (vmul u v) x"

surfaceTranslateIdentityRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceTranslateIdentityRule =
  fmap (withCapability SurfaceKnownZero 0) $
    surfaceEquationRule surfaceTranslateIdentityLawId 0 ["v", "x"] "translate v x = x"

surfaceScaleIdentityRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceScaleIdentityRule =
  fmap (withCapability SurfaceKnownOne 0) $
    surfaceEquationRule surfaceScaleIdentityLawId 0 ["v", "x"] "scale v x = x"

surfaceScaleInterHoistRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceScaleInterHoistRule =
  fmap withNonDegenerateScale $
    hoistRule surfaceScaleInterHoistLawId "inter" "scale"

surfaceScaleDiffHoistRule :: Either SurfaceLawError SurfaceRewriteRule
surfaceScaleDiffHoistRule =
  fmap withNonDegenerateScale $
    hoistRule surfaceScaleDiffHoistLawId "diff" "scale"

hoistRule :: LawId -> String -> String -> Either SurfaceLawError SurfaceRewriteRule
hoistRule lawIdValue booleanName transformName =
  surfaceEquationRule
    lawIdValue
    0
    ["v", "a", "b"]
    (booleanName <> " (" <> transformName <> " v a) (" <> transformName <> " v b) = " <> transformName <> " v (" <> booleanName <> " a b)")

withCapability :: SurfaceCapability -> Int -> SurfaceRewriteRule -> SurfaceRewriteRule
withCapability capability patternVarIndex rule =
  rule {rrCondition = Just (RewriteCondition (guardHasCapability capability [GuardVar (mkPatternVar patternVarIndex)]))}

withNonDegenerateScale :: SurfaceRewriteRule -> SurfaceRewriteRule
withNonDegenerateScale rule =
  rule {rrCondition = Just (RewriteCondition (guardHasFact surfaceNonDegenerateScaleFactId [GuardVar (mkPatternVar 0)]))}

surfaceEquationRule :: LawId -> Int -> [String] -> String -> Either SurfaceLawError SurfaceRewriteRule
surfaceEquationRule lawIdValue instantiationIndex names sourceText =
  equationRuleWith surfaceEquationFront lawIdValue instantiationIndex names [] sourceText

surfaceEquationFront :: EquationFront (ApplicativeEquationError SurfaceBuildRefusal) (Pattern SurfaceF) () SurfaceF
surfaceEquationFront =
  applicativeEquationFront buildSurfaceNode readSurfaceLiteral

buildSurfaceNode :: String -> [Pattern SurfaceF] -> Either SurfaceBuildRefusal (Pattern SurfaceF)
buildSurfaceNode symbol children =
  case (symbol, children) of
    ("vec", [xValue, yValue, zValue]) -> Right (PatternNode (SurfaceVec xValue yValue zValue))
    ("vadd", [left, right]) -> Right (PatternNode (SurfaceVAdd left right))
    ("vmul", [left, right]) -> Right (PatternNode (SurfaceVMul left right))
    ("sphere", [radius]) -> Right (PatternNode (SurfaceSphere radius))
    ("cube", [size]) -> Right (PatternNode (SurfaceCube size))
    ("cylinder", [radius, height]) -> Right (PatternNode (SurfaceCylinder radius height))
    ("translate", [vectorValue, body]) -> Right (PatternNode (SurfaceTranslate vectorValue body))
    ("rotate", [vectorValue, body]) -> Right (PatternNode (SurfaceRotate vectorValue body))
    ("scale", [vectorValue, body]) -> Right (PatternNode (SurfaceScale vectorValue body))
    ("union", [left, right]) -> Right (PatternNode (SurfaceUnion left right))
    ("inter", [left, right]) -> Right (PatternNode (SurfaceInter left right))
    ("diff", [left, right]) -> Right (PatternNode (SurfaceDiff left right))
    ("vec", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("vadd", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("vmul", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("sphere", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("cube", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("cylinder", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("translate", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("rotate", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("scale", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("union", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("inter", _) -> Left (SurfaceArityMismatch symbol (length children))
    ("diff", _) -> Left (SurfaceArityMismatch symbol (length children))
    _ -> Left (UnknownSurfaceSymbol symbol (length children))

readSurfaceLiteral :: String -> Maybe (Pattern SurfaceF)
readSurfaceLiteral token =
  PatternNode . SurfaceLit <$> readMaybe token
