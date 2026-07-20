module Test.Moonlight.Flow.Gen.Execution
  ( genTriangleProgram,
    genPathProgram,
  )
where

import Test.Moonlight.Flow.Execution.RelProgram
  ( RelProgram,
    atom,
    generatedTriangleProgram,
    program,
  )
import Test.QuickCheck
  ( Gen,
    chooseInt,
    vectorOf,
  )

genTriangleProgram :: Gen RelProgram
genTriangleProgram =
  generatedTriangleProgram <$> genRows <*> genRows <*> genRows
  where
    genRows = do
      rowCount <- chooseInt (0, 24)
      vectorOf rowCount (vectorOf 2 (chooseInt (0, 12)))

genPathProgram :: Gen RelProgram
genPathProgram = do
  leftRows <- genPairRows
  middleRows <- genPairRows
  rightRows <- genPairRows
  pure $
    program
      "generated-execution-path"
      0
      [ atom 0 [0, 1] leftRows,
        atom 1 [1, 2] middleRows,
        atom 2 [2, 3] rightRows
      ]
      Nothing
  where
    genPairRows = do
      rowCount <- chooseInt (0, 24)
      vectorOf rowCount (vectorOf 2 (chooseInt (0, 24)))
