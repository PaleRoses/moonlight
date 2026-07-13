module Moonlight.Sheaf.Section.Stalk.Geometric
  ( GeometricStalk (..),
    GeometricMismatch (..),
    GeometricRepairObstruction (..),
    GeometricRestriction (..),
    geometricStalkAlgebra,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Sheaf.Section.Stalk
  ( RepairInput (..),
    StalkAlgebra (..),
    StalkRestrictionKernel (..),
    mergeStalks,
    normalizeStalk,
    restrictStalk,
    stalkMismatches,
  )

type GeometricStalk :: Type -> Type -> Type
data GeometricStalk chart metric = GeometricStalk
  { geometricChart :: !chart,
    geometricMetric :: !metric
  }
  deriving stock (Eq, Ord, Show, Read, Functor)

type GeometricMismatch :: Type -> Type -> Type
data GeometricMismatch chartMismatch metricMismatch
  = GeometricChartMismatch chartMismatch
  | GeometricMetricMismatch metricMismatch
  deriving stock (Eq, Ord, Show, Read)

type GeometricRepairObstruction :: Type -> Type -> Type
data GeometricRepairObstruction chartObstruction metricObstruction
  = GeometricChartRepairObstruction chartObstruction
  | GeometricMetricRepairObstruction metricObstruction
  deriving stock (Eq, Ord, Show, Read)

type GeometricRestriction :: Type -> Type -> Type
data GeometricRestriction chartWitness metricWitness = GeometricRestriction
  { grChartWitness :: !chartWitness,
    grMetricWitness :: !metricWitness
  }
  deriving stock (Eq, Ord, Show, Read)

instance Bifunctor GeometricStalk where
  bimap chartMap metricMap stalkValue =
    GeometricStalk
      { geometricChart = chartMap (geometricChart stalkValue),
        geometricMetric = metricMap (geometricMetric stalkValue)
      }

geometricStalkAlgebra ::
  StalkAlgebra chartWitness chart chartMismatch chartRepairObstruction ->
  StalkAlgebra metricWitness metric metricMismatch metricRepairObstruction ->
  StalkAlgebra
    (GeometricRestriction chartWitness metricWitness)
    (GeometricStalk chart metric)
    (GeometricMismatch chartMismatch metricMismatch)
    (GeometricRepairObstruction chartRepairObstruction metricRepairObstruction)
geometricStalkAlgebra chartAlgebra metricAlgebra =
  StalkAlgebra
    { saRestrictionKernel =
        \restriction ->
          StalkRestrictionMap
            ( bimap
                (restrictStalk chartAlgebra (grChartWitness restriction))
                (restrictStalk metricAlgebra (grMetricWitness restriction))
            ),
      saMismatches =
        \leftStalk rightStalk ->
          fmap
            GeometricChartMismatch
            (stalkMismatches chartAlgebra (geometricChart leftStalk) (geometricChart rightStalk))
            <> fmap
              GeometricMetricMismatch
              (stalkMismatches metricAlgebra (geometricMetric leftStalk) (geometricMetric rightStalk)),
      saMerge =
        \leftStalk rightStalk ->
          GeometricStalk
            <$> first
              (fmap GeometricChartMismatch)
              (mergeStalks chartAlgebra (geometricChart leftStalk) (geometricChart rightStalk))
            <*> first
              (fmap GeometricMetricMismatch)
              (mergeStalks metricAlgebra (geometricMetric leftStalk) (geometricMetric rightStalk)),
      saRepair =
        \repairInput ->
          case repairInput of
            RepairMergeInput values mismatches ->
              let chartValues = fmap geometricChart values
                  metricValues = fmap geometricMetric values
               in GeometricStalk
                    <$> first
                      GeometricChartRepairObstruction
                      ( maybe
                          (Right (NonEmpty.head chartValues))
                          (saRepair chartAlgebra . RepairMergeInput chartValues)
                          ( NonEmpty.nonEmpty
                              [chartMismatch | GeometricChartMismatch chartMismatch <- NonEmpty.toList mismatches]
                          )
                      )
                    <*> first
                      GeometricMetricRepairObstruction
                      ( maybe
                          (Right (NonEmpty.head metricValues))
                          (saRepair metricAlgebra . RepairMergeInput metricValues)
                          ( NonEmpty.nonEmpty
                              [metricMismatch | GeometricMetricMismatch metricMismatch <- NonEmpty.toList mismatches]
                          )
                      )
            RepairRestrictionInput restriction restrictedValue targetValue mismatches ->
              GeometricStalk
                <$> first
                  GeometricChartRepairObstruction
                  ( maybe
                      (Right (geometricChart targetValue))
                      ( saRepair chartAlgebra
                          . RepairRestrictionInput
                            (grChartWitness restriction)
                            (geometricChart restrictedValue)
                            (geometricChart targetValue)
                      )
                      ( NonEmpty.nonEmpty
                          [chartMismatch | GeometricChartMismatch chartMismatch <- NonEmpty.toList mismatches]
                      )
                  )
                <*> first
                  GeometricMetricRepairObstruction
                  ( maybe
                      (Right (geometricMetric targetValue))
                      ( saRepair metricAlgebra
                          . RepairRestrictionInput
                            (grMetricWitness restriction)
                            (geometricMetric restrictedValue)
                            (geometricMetric targetValue)
                      )
                      ( NonEmpty.nonEmpty
                          [metricMismatch | GeometricMetricMismatch metricMismatch <- NonEmpty.toList mismatches]
                      )
                  ),
      saNormalize =
        bimap
          (normalizeStalk chartAlgebra)
          (normalizeStalk metricAlgebra)
    }
