{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.Equation
  ( EquationFront (..),
    EquationSides (..),
    EquationError (..),
    EquationBinderState (..),
    ApplicativeEquationError (..),
    ApplicativeToken (..),
    applicativeEquationFront,
    splitEquation,
    equationSides,
    equationRuleWith,
  )
where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Kind (Type)
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumL)
import Moonlight.Core (Pattern (..), PatternVar, RewriteRuleId (..), mkPatternVar)
import Moonlight.Rewrite.System (LawId, lawIdKey)
import Moonlight.Rewrite.System (RawRewriteRule (..))

type EquationFront :: Type -> Type -> Type -> (Type -> Type) -> Type
data EquationFront e subst binder f = EquationFront
  { efParseTerm :: !(String -> Either e (Pattern f)),
    efParseTermWithVariables :: !(Map String PatternVar -> String -> Either e (Pattern f)),
    efVariableName :: !(f (Pattern f) -> Maybe String),
    efSubstitutionPattern :: !(subst -> Pattern f),
    efEnterBinderNode :: !(LawId -> Map String subst -> EquationBinderState binder -> f (Pattern f) -> Maybe (f (Pattern f), EquationBinderState binder)),
    efRewriteBoundReference :: !(EquationBinderState binder -> f (Pattern f) -> Maybe (f (Pattern f)))
  }

type EquationSides :: Type
data EquationSides = EquationSides
  { esLhs :: !String,
    esRhs :: !String
  }
  deriving stock (Eq, Ord, Show)

type EquationError :: Type -> Type
data EquationError e
  = DuplicateEquationVariable !String
  | InvalidEquationRuleInstantiationIndex !Int
  | EquationMissingEquals !String
  | EquationAmbiguousEquals !String !Int
  | EquationParseFailure !e
  deriving stock (Eq, Show)

type EquationScanState :: Type
data EquationScanState = EquationScanState
  { essParenDepth :: !Int,
    essBracketDepth :: !Int,
    essBraceDepth :: !Int,
    essPrevious :: !(Maybe Char),
    essMatches :: ![Int]
  }

type EquationBinderState :: Type -> Type
data EquationBinderState binder = EquationBinderState
  { ebsNextBinderOffset :: !Int,
    ebsBinders :: !(Map binder binder)
  }
  deriving stock (Eq, Show)

type ApplicativeToken :: Type
data ApplicativeToken
  = ApplicativeAtom !Int !String
  | ApplicativeOpen !Int
  | ApplicativeClose !Int
  deriving stock (Eq, Ord, Show)

type ApplicativeEquationError :: Type -> Type
data ApplicativeEquationError e
  = ApplicativeUnexpectedEnd !Int
  | ApplicativeUnexpectedToken !ApplicativeToken
  | ApplicativeUnclosedParen !ApplicativeToken
  | ApplicativeNodeRefusal !ApplicativeToken !e
  deriving stock (Eq, Show)

equationSides :: String -> String -> EquationSides
equationSides lhs rhs =
  EquationSides (trim lhs) (trim rhs)

splitEquation :: String -> Either (EquationError e) EquationSides
splitEquation sourceText =
  case reverse (essMatches (foldl' scanChar initialEquationScanState (scanRows sourceText))) of
    [] ->
      Left (EquationMissingEquals sourceText)
    [index] ->
      case splitAt index sourceText of
        (lhsText, _ : rhsText) -> Right (equationSides lhsText rhsText)
        _ -> Left (EquationMissingEquals sourceText)
    matches ->
      Left (EquationAmbiguousEquals sourceText (length matches))

equationRuleWith ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Int ->
  [String] ->
  [(String, subst)] ->
  String ->
  Either (EquationError e) (RawRewriteRule cond f)
equationRuleWith front lawIdValue instantiationIndex names substitutions sourceText =
  splitEquation sourceText >>= equationRuleFromSidesWith front lawIdValue instantiationIndex names substitutions

equationRuleFromSidesWith ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Int ->
  [String] ->
  [(String, subst)] ->
  EquationSides ->
  Either (EquationError e) (RawRewriteRule cond f)
equationRuleFromSidesWith front lawIdValue instantiationIndex names substitutions sides
  | instantiationIndex < 0 || instantiationIndex >= 100 =
      Left (InvalidEquationRuleInstantiationIndex instantiationIndex)
  | otherwise =
      RawRewriteRule
        <$> pure (RewriteRuleId (lawIdKey lawIdValue * 100 + instantiationIndex))
        <*> equationPatternWith front lawIdValue names substitutions (esLhs sides)
        <*> equationPatternWith front lawIdValue names substitutions (esRhs sides)
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing

equationPatternWith ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  [String] ->
  [(String, subst)] ->
  String ->
  Either (EquationError e) (Pattern f)
equationPatternWith front lawIdValue names substitutions sourceText =
  patternVarMapFromNames names >>= \variables ->
    either (Left . EquationParseFailure) Right (efParseTermWithVariables front variables sourceText) >>= \patternValue ->
      pure (abstractEquationPattern front lawIdValue variables (Map.fromList substitutions) patternValue)

abstractEquationPattern ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Map String PatternVar ->
  Map String subst ->
  Pattern f ->
  Pattern f
abstractEquationPattern front lawIdValue variables substitutions patternValue =
  fst (rewritePattern front lawIdValue variables substitutions initialBinderRewriteState patternValue)

rewritePattern ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Map String PatternVar ->
  Map String subst ->
  EquationBinderState binder ->
  Pattern f ->
  (Pattern f, EquationBinderState binder)
rewritePattern front lawIdValue variables substitutions binderState =
  \case
    PatternVar patternVariable ->
      (PatternVar patternVariable, binderState)
    PatternNode nodeValue ->
      rewriteNode front lawIdValue variables substitutions binderState nodeValue

rewriteNode ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Map String PatternVar ->
  Map String subst ->
  EquationBinderState binder ->
  f (Pattern f) ->
  (Pattern f, EquationBinderState binder)
rewriteNode front lawIdValue variables substitutions binderState nodeValue =
  case efRewriteBoundReference front binderState nodeValue of
    Just rewrittenNode ->
      (PatternNode rewrittenNode, binderState)
    Nothing ->
      case efVariableName front nodeValue of
        Just variableName
          | Just patternVariable <- Map.lookup variableName variables ->
              (PatternVar patternVariable, binderState)
          | Just substitutionPattern <- efSubstitutionPattern front <$> Map.lookup variableName substitutions ->
              (substitutionPattern, binderState)
        _ ->
          rewriteStructuredNode front lawIdValue variables substitutions binderState nodeValue

rewriteStructuredNode ::
  (Ord binder, Traversable f) =>
  EquationFront e subst binder f ->
  LawId ->
  Map String PatternVar ->
  Map String subst ->
  EquationBinderState binder ->
  f (Pattern f) ->
  (Pattern f, EquationBinderState binder)
rewriteStructuredNode front lawIdValue variables substitutions binderState nodeValue =
  let (enteredNode, enteredState) =
        maybe
          (nodeValue, binderState)
          id
          (efEnterBinderNode front lawIdValue substitutions binderState nodeValue)
      (finalState, rewrittenNode) =
        mapAccumL
          ( \stateValue child ->
              let (rewrittenChild, childState) = rewritePattern front lawIdValue variables substitutions stateValue child
               in (childState, rewrittenChild)
          )
          enteredState
          enteredNode
   in (PatternNode rewrittenNode, finalState)

applicativeEquationFront ::
  (String -> [Pattern f] -> Either e (Pattern f)) ->
  (String -> Maybe (Pattern f)) ->
  EquationFront (ApplicativeEquationError e) (Pattern f) () f
applicativeEquationFront buildNode readLiteral =
  EquationFront
    { efParseTerm = parseApplicativeEquationTerm Map.empty buildNode readLiteral,
      efParseTermWithVariables = \variables -> parseApplicativeEquationTerm variables buildNode readLiteral,
      efVariableName = const Nothing,
      efSubstitutionPattern = id,
      efEnterBinderNode = \_ _ _ _ -> Nothing,
      efRewriteBoundReference = \_ _ -> Nothing
    }

parseApplicativeEquationTerm ::
  Map String PatternVar ->
  (String -> [Pattern f] -> Either e (Pattern f)) ->
  (String -> Maybe (Pattern f)) ->
  String ->
  Either (ApplicativeEquationError e) (Pattern f)
parseApplicativeEquationTerm variables buildNode readLiteral sourceText =
  case parseExpression variables buildNode readLiteral (tokenizeApplicative sourceText) of
    Left refusal ->
      Left refusal
    Right (patternValue, []) ->
      Right patternValue
    Right (_, tokenValue : _) ->
      Left (ApplicativeUnexpectedToken tokenValue)

parseExpression ::
  Map String PatternVar ->
  (String -> [Pattern f] -> Either e (Pattern f)) ->
  (String -> Maybe (Pattern f)) ->
  [ApplicativeToken] ->
  Either (ApplicativeEquationError e) (Pattern f, [ApplicativeToken])
parseExpression variables buildNode readLiteral =
  \case
    [] ->
      Left (ApplicativeUnexpectedEnd 0)
    ApplicativeClose position : _ ->
      Left (ApplicativeUnexpectedToken (ApplicativeClose position))
    ApplicativeOpen position : rest ->
      parseExpression variables buildNode readLiteral rest >>= \(patternValue, remaining) ->
        case remaining of
          ApplicativeClose _ : afterClose ->
            Right (patternValue, afterClose)
          tokenValue : _ ->
            Left (ApplicativeUnexpectedToken tokenValue)
          [] ->
            Left (ApplicativeUnclosedParen (ApplicativeOpen position))
    ApplicativeAtom position symbol : rest ->
      parseChildren variables buildNode readLiteral rest >>= \(children, remaining) ->
        case (Map.lookup symbol variables, children, readLiteral symbol) of
          (Just patternVariable, [], _) ->
            Right (PatternVar patternVariable, remaining)
          (_, [], Just literalPattern) ->
            Right (literalPattern, remaining)
          _ ->
            either
              (Left . ApplicativeNodeRefusal (ApplicativeAtom position symbol))
              (\patternValue -> Right (patternValue, remaining))
              (buildNode symbol children)

parseAtom ::
  Map String PatternVar ->
  (String -> [Pattern f] -> Either e (Pattern f)) ->
  (String -> Maybe (Pattern f)) ->
  [ApplicativeToken] ->
  Either (ApplicativeEquationError e) (Pattern f, [ApplicativeToken])
parseAtom variables buildNode readLiteral =
  \case
    [] ->
      Left (ApplicativeUnexpectedEnd 0)
    ApplicativeClose position : _ ->
      Left (ApplicativeUnexpectedToken (ApplicativeClose position))
    ApplicativeOpen position : rest ->
      parseExpression variables buildNode readLiteral rest >>= \(patternValue, remaining) ->
        case remaining of
          ApplicativeClose _ : afterClose ->
            Right (patternValue, afterClose)
          tokenValue : _ ->
            Left (ApplicativeUnexpectedToken tokenValue)
          [] ->
            Left (ApplicativeUnclosedParen (ApplicativeOpen position))
    ApplicativeAtom position symbol : rest ->
      case (Map.lookup symbol variables, readLiteral symbol) of
        (Just patternVariable, _) ->
          Right (PatternVar patternVariable, rest)
        (_, Just literalPattern) ->
          Right (literalPattern, rest)
        _ ->
          either
            (Left . ApplicativeNodeRefusal (ApplicativeAtom position symbol))
            (\patternValue -> Right (patternValue, rest))
            (buildNode symbol [])

parseChildren ::
  Map String PatternVar ->
  (String -> [Pattern f] -> Either e (Pattern f)) ->
  (String -> Maybe (Pattern f)) ->
  [ApplicativeToken] ->
  Either (ApplicativeEquationError e) ([Pattern f], [ApplicativeToken])
parseChildren variables buildNode readLiteral tokens =
  case tokens of
    [] ->
      Right ([], [])
    ApplicativeClose _ : _ ->
      Right ([], tokens)
    _ ->
      parseAtom variables buildNode readLiteral tokens >>= \(child, remaining) ->
        fmap
          (\(children, finalTokens) -> (child : children, finalTokens))
          (parseChildren variables buildNode readLiteral remaining)

tokenizeApplicative :: String -> [ApplicativeToken]
tokenizeApplicative sourceText =
  lexApplicative sourceText

type LexemeState :: Type
data LexemeState
  = LexemeSpace !Int
  | LexemeAtom !Int !String
  deriving stock (Eq, Show)

lexApplicative :: String -> [ApplicativeToken]
lexApplicative sourceText =
  finalizeLexeme (foldl' lexChar (LexemeSpace 0, []) (zip [0 ..] sourceText))

lexChar :: (LexemeState, [ApplicativeToken]) -> (Int, Char) -> (LexemeState, [ApplicativeToken])
lexChar (lexemeState, tokens) (position, charValue)
  | isSpace charValue =
      (LexemeSpace (position + 1), closeLexeme lexemeState tokens)
  | charValue == '(' =
      (LexemeSpace (position + 1), ApplicativeOpen position : closeLexeme lexemeState tokens)
  | charValue == ')' =
      (LexemeSpace (position + 1), ApplicativeClose position : closeLexeme lexemeState tokens)
  | otherwise =
      case lexemeState of
        LexemeSpace _ ->
          (LexemeAtom position [charValue], tokens)
        LexemeAtom start text ->
          (LexemeAtom start (text <> [charValue]), tokens)

closeLexeme :: LexemeState -> [ApplicativeToken] -> [ApplicativeToken]
closeLexeme lexemeState tokens =
  case lexemeState of
    LexemeSpace _ ->
      tokens
    LexemeAtom start text ->
      ApplicativeAtom start text : tokens

finalizeLexeme :: (LexemeState, [ApplicativeToken]) -> [ApplicativeToken]
finalizeLexeme (lexemeState, tokens) =
  reverse (closeLexeme lexemeState tokens)

patternVarMapFromNames :: [String] -> Either (EquationError e) (Map String PatternVar)
patternVarMapFromNames names =
  fmap eavVariables (foldM observeName (EquationAllocatedVariables Map.empty) (zip names (mkPatternVar <$> [0 ..])))
  where
    observeName ::
      EquationAllocatedVariables ->
      (String, PatternVar) ->
      Either (EquationError e) EquationAllocatedVariables
    observeName accumulator (name, patternVariable)
      | Map.member name (eavVariables accumulator) =
          Left (DuplicateEquationVariable name)
      | otherwise =
          Right
            accumulator
              { eavVariables =
                  Map.insert name patternVariable (eavVariables accumulator)
              }

type EquationAllocatedVariables :: Type
newtype EquationAllocatedVariables = EquationAllocatedVariables
  { eavVariables :: Map String PatternVar
  }

initialEquationScanState :: EquationScanState
initialEquationScanState =
  EquationScanState
    { essParenDepth = 0,
      essBracketDepth = 0,
      essBraceDepth = 0,
      essPrevious = Nothing,
      essMatches = []
    }

initialBinderRewriteState :: EquationBinderState binder
initialBinderRewriteState =
  EquationBinderState
    { ebsNextBinderOffset = 0,
      ebsBinders = Map.empty
    }

scanRows :: String -> [(Int, Char, Maybe Char)]
scanRows sourceText =
  zip3 [0 ..] sourceText (fmap Just (drop 1 sourceText) <> [Nothing])

scanChar :: EquationScanState -> (Int, Char, Maybe Char) -> EquationScanState
scanChar stateValue (index, charValue, nextChar) =
  updateDepth charValue
    stateValue
      { essPrevious = Just charValue,
        essMatches =
          if bareEqualsAt stateValue charValue nextChar
            then index : essMatches stateValue
            else essMatches stateValue
      }

bareEqualsAt :: EquationScanState -> Char -> Maybe Char -> Bool
bareEqualsAt stateValue charValue nextChar =
  charValue == '='
    && essParenDepth stateValue == 0
    && essBracketDepth stateValue == 0
    && essBraceDepth stateValue == 0
    && maybe True (`notElem` ("=<>/" :: String)) (essPrevious stateValue)
    && maybe True (`notElem` ("=>" :: String)) nextChar

updateDepth :: Char -> EquationScanState -> EquationScanState
updateDepth =
  \case
    '(' -> \stateValue -> stateValue {essParenDepth = essParenDepth stateValue + 1}
    ')' -> \stateValue -> stateValue {essParenDepth = max 0 (essParenDepth stateValue - 1)}
    '[' -> \stateValue -> stateValue {essBracketDepth = essBracketDepth stateValue + 1}
    ']' -> \stateValue -> stateValue {essBracketDepth = max 0 (essBracketDepth stateValue - 1)}
    '{' -> \stateValue -> stateValue {essBraceDepth = essBraceDepth stateValue + 1}
    '}' -> \stateValue -> stateValue {essBraceDepth = max 0 (essBraceDepth stateValue - 1)}
    _ -> id

trim :: String -> String
trim =
  dropWhileEnd isSpace . dropWhile isSpace
