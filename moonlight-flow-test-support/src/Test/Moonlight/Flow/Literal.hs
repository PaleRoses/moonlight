{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Literal
  ( PlanLiteral (..),
    PlanLiteralError (..),
    parsePlanLiteral,
    prettyPlan,
    planLiteral,
    literalRoundTrip,
  )
where

import Data.Char (isAlpha, ord)
import Data.List qualified as List
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Moonlight.Core
  ( SlotId,
    mkAtomId,
    mkSlotId,
  )
import Moonlight.Flow.Plan.Compile.Build qualified as PlanBuild
import Moonlight.Flow.Plan.Query.Core
  ( AtomSpec,
    OutputVar,
    QueryOutput (..),
    QueryPlanDomain (StructuralQueryPlan),
    mkAtomSpec,
    mkQueryAtomId,
    mkSourceAtomId,
    mkStalkRecipe,
  )
import Moonlight.Flow.Plan.Shape.Build
  ( queryPlanToPlanShape,
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape,
    PlanStage (RawLogical),
  )
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
    pfail,
    string,
    (<++),
  )

data PlanLiteral
  = LiteralAtom !String ![Char]
  | LiteralProject ![Char] !PlanLiteral
  deriving stock (Eq, Ord, Show, Read)

data PlanLiteralError
  = PlanLiteralParseFailed !String
  | PlanLiteralUnsupportedNestedProject !PlanLiteral
  | PlanLiteralBuildFailed ![PlanBuild.QueryPlanError]
  deriving stock (Eq, Show)

data LiteralOutput = LiteralOutput

type LiteralTuple = ()

instance QueryOutput LiteralOutput Int where
  type OutputVar LiteralOutput Int = ()
  data OutputRecipe LiteralOutput Int = LiteralOutputRecipe

  mkOutputRecipe _ = LiteralOutputRecipe

  projectOutputRecipe LiteralOutputRecipe _root _bindingValues = Right LiteralOutput

parsePlanLiteral :: String -> Either PlanLiteralError PlanLiteral
parsePlanLiteral source =
  case fmap fst (filter (null . snd) (readP_to_S (skipSpaces *> parseLiteral <* skipSpaces <* eof) source)) of
    [literal] -> Right literal
    _ -> Left (PlanLiteralParseFailed source)

prettyPlan :: PlanLiteral -> String
prettyPlan literal =
  case literal of
    LiteralAtom relation slots ->
      "(atom " <> relation <> " [" <> punctuateComma slots <> "])"
    LiteralProject slots child ->
      "(project [" <> punctuateComma slots <> "] " <> prettyPlan child <> ")"

planLiteral :: String -> Either PlanLiteralError (PlanShape 'RawLogical)
planLiteral source =
  parsePlanLiteral source >>= literalToShape

literalRoundTrip :: String -> Either PlanLiteralError Bool
literalRoundTrip source = do
  parsed <- parsePlanLiteral source
  reparsed <- parsePlanLiteral (prettyPlan parsed)
  pure (parsed == reparsed)

parseLiteral :: ReadP PlanLiteral
parseLiteral = parseProject <++ parseAtom

parseAtom :: ReadP PlanLiteral
parseAtom = do
  _ <- char '('
  skipSpaces
  _ <- string "atom"
  skipSpaces
  relation <- munch1 isAlpha
  skipSpaces
  slots <- parseSlots
  skipSpaces
  _ <- char ')'
  pure (LiteralAtom relation slots)

parseProject :: ReadP PlanLiteral
parseProject = do
  _ <- char '('
  skipSpaces
  _ <- string "project"
  skipSpaces
  slots <- parseSlots
  skipSpaces
  child <- parseLiteral
  skipSpaces
  _ <- char ')'
  pure (LiteralProject slots child)

parseSlots :: ReadP [Char]
parseSlots = do
  _ <- char '['
  skipSpaces
  slots <- parseSlot `sepBy1` (skipSpaces *> char ',' <* skipSpaces)
  skipSpaces
  _ <- char ']'
  pure slots

parseSlot :: ReadP Char
parseSlot = do
  token <- munch1 isAlpha
  case token of
    [slot] -> pure slot
    _ -> pfail

literalToShape :: PlanLiteral -> Either PlanLiteralError (PlanShape 'RawLogical)
literalToShape literal =
  case literal of
    LiteralAtom relation slots -> buildShape relation slots slots
    LiteralProject outputs (LiteralAtom relation slots) -> buildShape relation slots outputs
    LiteralProject _ nested -> Left (PlanLiteralUnsupportedNestedProject nested)

buildShape :: String -> [Char] -> [Char] -> Either PlanLiteralError (PlanShape 'RawLogical)
buildShape relation atomSlots outputSlots =
  case PlanBuild.mkQueryPlan input of
    Left err -> Left (PlanLiteralBuildFailed err)
    Right plan -> Right (queryPlanToPlanShape plan)
  where
    schema = fmap slotId atomSlots
    input :: PlanBuild.QueryPlanInput () LiteralOutput () () LiteralTuple Int
    input =
      PlanBuild.QueryPlanInput
        { PlanBuild.qpiDomain = StructuralQueryPlan,
          PlanBuild.qpiCompiled = (),
          PlanBuild.qpiDigest = digestString relation,
          PlanBuild.qpiAtoms = Vector.singleton atomSpec,
          PlanBuild.qpiSchemaOrder = Just (Vector.fromList schema),
          PlanBuild.qpiRootSlot = maybe (mkSlotId 0) slotId (safeHead atomSlots),
          PlanBuild.qpiOutputs = fmap (\slot -> PlanBuild.PlanOutputBinding (slotId slot) ()) outputSlots,
          PlanBuild.qpiResidual = PlanBuild.NoQueryPlanResidual
        }
    atomSpec :: AtomSpec () LiteralTuple Int
    atomSpec =
      mkAtomSpec
        (mkQueryAtomId 0)
        (mkSourceAtomId (mkAtomId 0))
        ()
        0
        (Vector.fromList schema)
        (mkStalkRecipe (Vector.replicate (length schema) []))

slotId :: Char -> SlotId
slotId slot =
  mkSlotId (ord slot - ord 'a')

digestString :: String -> Word64
digestString =
  List.foldl' (\acc charValue -> acc * 167 + fromIntegral (ord charValue)) 2166136261

safeHead :: [value] -> Maybe value
safeHead values =
  case values of
    [] -> Nothing
    first : _ -> Just first

punctuateComma :: [Char] -> String
punctuateComma =
  foldr step ""
  where
    step slot acc =
      case acc of
        "" -> [slot]
        _ -> slot : ',' : acc
