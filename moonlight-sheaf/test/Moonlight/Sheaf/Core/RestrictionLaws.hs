module Moonlight.Sheaf.Core.RestrictionLaws
  ( tests,
  )
where

import Data.Maybe (mapMaybe)
import Moonlight.Category
  ( FinCat,
    FinMor,
    FinObjectId (..),
    FinObj,
    allMorphisms,
    allObjects,
    composeMor,
    finMorSourceId,
    finMorTargetId,
    sampleFinCat,
    source,
    target,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (mkSheafBasis)
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError,
    buildRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.TestFixture.SheafClassLaws
  ( SheafClassLawsFixture (..),
    sheafClassLawsTests,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  case sampleSheafClassLawsFixture of
    Left registryError ->
      testGroup
        "restriction-laws"
        [ testCase
            "sample-system index construction"
            (assertFailure ("expected index construction to succeed: " <> show registryError))
        ]
    Right fixture ->
      sheafClassLawsTests fixture

newtype RestrictionProbe = RestrictionProbe
  { unRestrictionProbe :: Int
  }
  deriving stock (Eq, Show)

restrictionProbeAlgebra :: StalkAlgebra FinMor RestrictionProbe () ()
restrictionProbeAlgebra =
  StalkAlgebra
    { saRestrictionKernel = StalkRestrictionMap . restrictByMorphismDelta,
      saMismatches =
        \left right ->
          ([() | left /= right]),
      saMerge =
        \(RestrictionProbe leftValue) (RestrictionProbe rightValue) ->
          Right (RestrictionProbe (leftValue + rightValue)),
      saRepair = const (Left ()),
      saNormalize = id
    }

sampleSheafClassLawsFixture ::
  Either
    (RestrictionIndexError FinObj)
    (SheafClassLawsFixture FinObj RestrictionProbe () FinMor ())
sampleSheafClassLawsFixture =
  fmap
    ( \restrictions ->
        SheafClassLawsFixture
          { sclfName = "sample-system",
            sclfStalkAlgebra = restrictionProbeAlgebra,
            sclfRestrictions = restrictions,
            sclfGenStalk = genRestrictionProbe,
            sclfGenTripleOfCells = genRestrictionCompositionTriple,
            sclfGenSelfCell = genRestrictionIdentityCell
          }
    )
    sampleRestrictionIndex

sampleCategory :: FinCat
sampleCategory =
  sampleFinCat

sampleCells :: [FinObj]
sampleCells =
  allObjects sampleCategory

sampleMorphisms :: [FinMor]
sampleMorphisms =
  allMorphisms sampleCategory

sampleRestrictionIndex ::
  Either
    (RestrictionIndexError FinObj)
    (RestrictionIndex FinObj FinMor)
sampleRestrictionIndex =
  buildRestrictionIndex
    (mkObjectIndex (basisCells (mkSheafBasis sampleCells)))
    ( \(morphismValue, sourceObject, targetObject) ->
        RestrictionParts
          { partKind = unitIncidenceRestriction,
            partSource = sourceObject,
            partTarget = targetObject,
            partWitness = morphismValue
          }
    )
    sampleRestrictionEntries

sampleRestrictionEntries :: [(FinMor, FinObj, FinObj)]
sampleRestrictionEntries =
  mapMaybe
    ( \morphismValue ->
        case (source sampleCategory morphismValue, target sampleCategory morphismValue) of
          (Right sourceObject, Right targetObject) -> Just (morphismValue, sourceObject, targetObject)
          _ -> Nothing
    )
    sampleMorphisms

restrictByMorphismDelta :: FinMor -> RestrictionProbe -> RestrictionProbe
restrictByMorphismDelta morphismValue (RestrictionProbe probeValue) =
  RestrictionProbe (probeValue + morphismDelta morphismValue)

morphismDelta :: FinMor -> Int
morphismDelta morphismValue =
  let FinObjectId targetIdValue = finMorTargetId morphismValue
      FinObjectId sourceIdValue = finMorSourceId morphismValue
   in targetIdValue - sourceIdValue

genRestrictionCompositionTriple :: QC.Gen (FinObj, FinObj, FinObj)
genRestrictionCompositionTriple =
  QC.elements restrictionCompositionTriples

genRestrictionIdentityCell :: QC.Gen FinObj
genRestrictionIdentityCell =
  QC.elements restrictionIdentityCells

genRestrictionProbe :: QC.Gen RestrictionProbe
genRestrictionProbe =
  RestrictionProbe <$> QC.chooseInt (-128, 128)

restrictionCompositionTriples :: [(FinObj, FinObj, FinObj)]
restrictionCompositionTriples =
  sampleMorphisms
    >>= \morphismAB ->
      sampleMorphisms
        >>= \morphismBC ->
          either
            (const [])
            ( const
                ( case (source sampleCategory morphismAB, target sampleCategory morphismAB, target sampleCategory morphismBC) of
                    (Right sourceAB, Right targetAB, Right targetBC) -> [(sourceAB, targetAB, targetBC)]
                    _ -> []
                )
            )
            (composeMor sampleCategory morphismBC morphismAB)

restrictionIdentityCells :: [FinObj]
restrictionIdentityCells =
  sampleCells
