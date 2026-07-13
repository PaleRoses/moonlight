module Main
  ( main,
  )
where

import Moonlight.Sheaf.Bench.Cochain (cochainBenchmarks)
import Moonlight.Sheaf.Bench.Cosheaf (finiteCosheafBenchmarks)
import Moonlight.Sheaf.Bench.Operation (operationBenchmarks)
import Moonlight.Sheaf.Bench.PropagationAudit (propagationAuditBenchmarks)
import Moonlight.Sheaf.Bench.PropagationToy (propagationToyBenchmarks)
import Moonlight.Sheaf.Bench.Query (queryBenchmarks)
import Moonlight.Sheaf.Bench.SitePreparation (sitePreparationBenchmarks)
import Moonlight.Sheaf.Bench.StoreDescent (storeDescentBenchmarks)
import Test.Tasty.Bench (Benchmark, defaultMain)

main :: IO ()
main =
  sequenceA sheafBenchmarkSections >>= defaultMain

sheafBenchmarkSections :: [IO Benchmark]
sheafBenchmarkSections =
  [ pure operationBenchmarks,
    pure storeDescentBenchmarks,
    pure sitePreparationBenchmarks,
    cochainBenchmarks,
    finiteCosheafBenchmarks,
    queryBenchmarks,
    pure propagationToyBenchmarks,
    pure propagationAuditBenchmarks
  ]
