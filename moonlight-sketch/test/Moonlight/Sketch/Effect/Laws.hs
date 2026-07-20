module Moonlight.Sketch.Effect.Laws
  ( tests,
  )
where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Moonlight.Pale.Test.LawSuite
  ( QuickCheckLawBundle,
    lawSuiteGroup,
    quickCheckLawBundle,
    quickCheckLawBundleGroup,
    quickCheckLawDefinition,
  )
import Moonlight.Sketch
  ( ConstraintId,
    PreprocessId,
    RefinementId,
    SchemaEnv,
    TransformFns (..),
    TransformId,
    addConstraint,
    addPreprocessor,
    addRefinement,
    addTransform,
    emptySchemaEnv,
    mkConstraintId,
    mkPreprocessId,
    mkRefinementId,
    mkTransformId,
  )
import Moonlight.Sketch.Arbitrary ()
import Moonlight.Sketch.Effect.Harness
  ( envMergeAssociative,
    envMergeIdentity,
    formatMatchDeterministic,
    hashCollisionResistance,
    hashDeterministic,
    hashPostNormalization,
    latticeAbsorptionJoin,
    latticeAbsorptionMeet,
    latticeBoundedJoinIdentity,
    latticeBoundedMeetIdentity,
    normalizeDeterministic,
    normalizeIdempotent,
    resolveCycleDetection,
    resolveIdempotent,
    resolvePreservesSemantics,
    schemaEqPostNormalization,
    subtypeAntisymmetric,
    subtypeReflexive,
    subtypeTransitive,
    subtypeUnknownTop,
    subtypeVoidBottom,
    validateAccumulation,
    validateEmptyOnConformance,
  )
import Moonlight.Sketch.Effect.LawNames (CommonLawName (..), SketchLawName (..))
import Test.Tasty (TestTree, localOption)
import qualified Test.Tasty.QuickCheck as QC

type SmallValue :: Type
newtype SmallValue = SmallValue {unSmallValue :: Value}
  deriving stock (Show)

instance QC.Arbitrary SmallValue where
  arbitrary = QC.sized genValue

genValue :: Int -> QC.Gen SmallValue
genValue size
  | size <= 0 =
      SmallValue
        <$> QC.oneof
          [ pure Null,
            Bool <$> QC.arbitrary,
            Number . fromInteger <$> QC.chooseInteger (-100, 100),
            String . Text.pack <$> QC.listOf (QC.elements ['a' .. 'z'])
          ]
  | otherwise =
      let child = max 0 (size `div` 2)
       in SmallValue
            <$> QC.frequency
              [ (4, unSmallValue <$> genValue 0),
                (1, Array . Vector.fromList . map unSmallValue <$> genBoundedList 4 (genValue child)),
                (1, Object . KeyMap.fromList <$> genBoundedList 4 ((,) <$> genObjectKey <*> (unSmallValue <$> genValue child)))
              ]

genObjectKey :: QC.Gen Key.Key
genObjectKey =
  Key.fromText . Text.pack <$> genBoundedList 6 (QC.elements ['a' .. 'z'])

genBoundedList :: Int -> QC.Gen a -> QC.Gen [a]
genBoundedList maxLength generator = do
  listLength <- QC.chooseInt (0, max 0 maxLength)
  QC.vectorOf listLength generator

envA :: SchemaEnv
envA =
  addRefinement (requiredRefinementId "r1") (\_ _ -> [])
    . addTransform (requiredTransformId "t1") (TransformFns (\_ -> Right) (\_ -> Right))
    $ emptySchemaEnv

envB :: SchemaEnv
envB =
  addPreprocessor (requiredPreprocessId "p1") (\_ -> id)
    . addConstraint (requiredConstraintId "c1") (\_ _ -> [])
    $ emptySchemaEnv

envC :: SchemaEnv
envC =
  addRefinement (requiredRefinementId "r2") (\_ _ -> [])
    . addConstraint (requiredConstraintId "c2") (\_ _ -> [])
    $ emptySchemaEnv

effectLawBundle :: QuickCheckLawBundle String SketchLawName
effectLawBundle =
  quickCheckLawBundle
    "effect-laws"
    [ quickCheckLawDefinition (CommonLaw NormalizeIdempotent) normalizeIdempotent,
      quickCheckLawDefinition NormalizeDeterministic normalizeDeterministic,
      quickCheckLawDefinition HashDeterministic hashDeterministic,
      quickCheckLawDefinition HashPostNormalization hashPostNormalization,
      quickCheckLawDefinition HashCollisionResistance hashCollisionResistance,
      quickCheckLawDefinition SchemaEqPostNormalization schemaEqPostNormalization,
      quickCheckLawDefinition SubtypeReflexive subtypeReflexive,
      quickCheckLawDefinition SubtypeTransitive subtypeTransitive,
      quickCheckLawDefinition SubtypeAntisymmetric subtypeAntisymmetric,
      quickCheckLawDefinition SubtypeVoidBottom subtypeVoidBottom,
      quickCheckLawDefinition SubtypeUnknownTop subtypeUnknownTop,
      quickCheckLawDefinition ResolveIdempotent resolveIdempotent,
      quickCheckLawDefinition ResolvePreservesSemantics resolvePreservesSemantics,
      quickCheckLawDefinition ResolveCycleDetection resolveCycleDetection,
      quickCheckLawDefinition FormatMatchDeterministic
        (\formatSpec value -> formatMatchDeterministic formatSpec (Text.pack value)),
      quickCheckLawDefinition ValidateEmptyOnConformance
        (\node smallValue -> validateEmptyOnConformance node (unSmallValue smallValue)),
      quickCheckLawDefinition ValidateAccumulation
        (\node smallValue -> validateAccumulation node (unSmallValue smallValue)),
      quickCheckLawDefinition (CommonLaw LatticeAbsorptionJoin) latticeAbsorptionJoin,
      quickCheckLawDefinition (CommonLaw LatticeAbsorptionMeet) latticeAbsorptionMeet,
      quickCheckLawDefinition LatticeBoundedJoinIdentity latticeBoundedJoinIdentity,
      quickCheckLawDefinition LatticeBoundedMeetIdentity latticeBoundedMeetIdentity,
      quickCheckLawDefinition EnvMergeAssociative (envMergeAssociative envA envB envC),
      quickCheckLawDefinition EnvMergeIdentity (envMergeIdentity envA)
    ]

tests :: TestTree
tests =
  localOption
    (QC.QuickCheckTests 100)
    ( localOption
        (QC.QuickCheckMaxSize 16)
        (lawSuiteGroup "sketch-effect-laws" [quickCheckLawBundleGroup "sketch" id [effectLawBundle]])
    )

requiredRefinementId :: Text -> RefinementId
requiredRefinementId =
  requiredIdentifier mkRefinementId

requiredTransformId :: Text -> TransformId
requiredTransformId =
  requiredIdentifier mkTransformId

requiredPreprocessId :: Text -> PreprocessId
requiredPreprocessId =
  requiredIdentifier mkPreprocessId

requiredConstraintId :: Text -> ConstraintId
requiredConstraintId =
  requiredIdentifier mkConstraintId

requiredIdentifier :: (Text -> Maybe identifier) -> Text -> identifier
requiredIdentifier mkIdentifier rawIdentifier =
  case mkIdentifier rawIdentifier of
    Just identifier -> identifier
    Nothing -> error "expected valid effect-law identifier"
