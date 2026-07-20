module Moonlight.Sketch.Effect.Harness
  ( normalizeIdempotent,
    normalizeDeterministic,
    hashDeterministic,
    hashPostNormalization,
    hashCollisionResistance,
    schemaEqPostNormalization,
    subtypeReflexive,
    subtypeTransitive,
    subtypeAntisymmetric,
    subtypeVoidBottom,
    subtypeUnknownTop,
    resolveIdempotent,
    resolvePreservesSemantics,
    resolveCycleDetection,
    formatMatchDeterministic,
    validateEmptyOnConformance,
    validateAccumulation,
    latticeAbsorptionJoin,
    latticeAbsorptionMeet,
    latticeBoundedJoinIdentity,
    latticeBoundedMeetIdentity,
    envMergeAssociative,
    envMergeIdentity,
  )
where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Moonlight.Algebra
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    JoinSemilattice (..),
    MeetSemilattice (..),
  )
import Moonlight.Sketch
  ( ConstraintId,
    PreprocessId,
    RefinementId,
    SchemaEnv (..),
    SchemaF (..),
    SchemaNode (..),
    SchemaRegistry (..),
    StringFormat,
    TransformId,
    cataSchema,
    detectCycles,
    emptySchemaEnv,
    isSubtype,
    matchFormat,
    mergeSchemaEnv,
    normalize,
    resolve,
    schemaEq,
    schemaHash,
    validate,
  )

normalizeIdempotent :: SchemaNode -> Bool
normalizeIdempotent node = normalize (normalize node) == normalize node

normalizeDeterministic :: SchemaNode -> SchemaNode -> Bool
normalizeDeterministic left right =
  normalize (SUnion [left, right])
    == normalize (SUnion [right, left])

hashDeterministic :: SchemaNode -> Bool
hashDeterministic node = schemaHash node == schemaHash (normalize node)

hashPostNormalization :: SchemaNode -> SchemaNode -> Bool
hashPostNormalization left right =
  if normalize left == normalize right
    then schemaHash left == schemaHash right
    else True

hashCollisionResistance :: SchemaNode -> SchemaNode -> Bool
hashCollisionResistance left right =
  if normalize left /= normalize right
    then schemaHash left /= schemaHash right
    else True

schemaEqPostNormalization :: SchemaNode -> SchemaNode -> Bool
schemaEqPostNormalization left right =
  schemaEq left right == (normalize left == normalize right)

subtypeReflexive :: SchemaNode -> Bool
subtypeReflexive node = isSubtype node node

subtypeTransitive :: SchemaNode -> SchemaNode -> SchemaNode -> Bool
subtypeTransitive first second third =
  if isSubtype first second && isSubtype second third
    then isSubtype first third
    else True

subtypeAntisymmetric :: SchemaNode -> SchemaNode -> Bool
subtypeAntisymmetric left right =
  if isSubtype left right && isSubtype right left
    then normalize left == normalize right
    else True

subtypeVoidBottom :: SchemaNode -> Bool
subtypeVoidBottom node = isSubtype SVoid node

subtypeUnknownTop :: SchemaNode -> Bool
subtypeUnknownTop node = isSubtype node SUnknown

resolveIdempotent :: SchemaRegistry -> SchemaNode -> Bool
resolveIdempotent registry node =
  resolve registry (resolve registry node)
    == resolve registry node

resolvePreservesSemantics :: SchemaRegistry -> SchemaNode -> Bool
resolvePreservesSemantics registry node =
  let resolved = resolve registry node
   in normalize resolved == normalize (resolve registry resolved)

resolveCycleDetection :: SchemaRegistry -> SchemaNode -> Bool
resolveCycleDetection registry node =
  let cycles = detectCycles registry node
      knownRefs = Map.keysSet (srSchemas registry)
   in all (`Set.member` knownRefs) cycles

formatMatchDeterministic :: StringFormat -> Text -> Bool
formatMatchDeterministic formatSpec textValue =
  let normalizedFormat =
        case normalize (SString Nothing (Just formatSpec)) of
          SString _ (Just innerFormat) -> innerFormat
          _ -> formatSpec
   in matchFormat formatSpec textValue == matchFormat normalizedFormat textValue

validateEmptyOnConformance :: SchemaNode -> Value -> Bool
validateEmptyOnConformance node value =
  case (node, value) of
    (SBool, Bool _) -> null (validate node value)
    (SNull, Null) -> null (validate node value)
    (SString Nothing Nothing, String _) -> null (validate node value)
    (SNumber Nothing, Number _) -> null (validate node value)
    (SArray SUnknown Nothing, Array _) -> null (validate node value)
    _ -> True

validateAccumulation :: SchemaNode -> Value -> Bool
validateAccumulation node value =
  let normalizedNode = normalize node
   in if containsReference node || containsReference normalizedNode
        then True
        else null (validate node value) == null (validate normalizedNode value)

latticeAbsorptionJoin :: SchemaNode -> SchemaNode -> Bool
latticeAbsorptionJoin left right =
  let absorbed = join left (meet left right)
   in isSubtype absorbed left && isSubtype left absorbed

latticeAbsorptionMeet :: SchemaNode -> SchemaNode -> Bool
latticeAbsorptionMeet left right =
  let absorbed = meet left (join left right)
   in isSubtype absorbed left && isSubtype left absorbed

latticeBoundedJoinIdentity :: SchemaNode -> Bool
latticeBoundedJoinIdentity node = normalize (join node bottom) == normalize node

latticeBoundedMeetIdentity :: SchemaNode -> Bool
latticeBoundedMeetIdentity node = normalize (meet node top) == normalize node

envMergeAssociative :: SchemaEnv -> SchemaEnv -> SchemaEnv -> Bool
envMergeAssociative first second third =
  envShape (mergeSchemaEnv first (mergeSchemaEnv second third))
    == envShape (mergeSchemaEnv (mergeSchemaEnv first second) third)

envMergeIdentity :: SchemaEnv -> Bool
envMergeIdentity env =
  envShape (mergeSchemaEnv env emptySchemaEnv) == envShape env
    && envShape (mergeSchemaEnv emptySchemaEnv env) == envShape env

envShape :: SchemaEnv -> (Set.Set RefinementId, Set.Set TransformId, Set.Set PreprocessId, Set.Set ConstraintId)
envShape env =
  ( Map.keysSet (seRefinements env),
    Map.keysSet (seTransforms env),
    Map.keysSet (sePreprocessors env),
    Map.keysSet (seConstraints env)
  )

containsReference :: SchemaNode -> Bool
containsReference =
  cataSchema
    ( \layer ->
        case layer of
          SRefF _ -> True
          SLazyF _ -> True
          _ -> or layer
    )
