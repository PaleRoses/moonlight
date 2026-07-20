module Moonlight.EGraph.Test.Chimera.Population
  ( tissueTermsAtDepth,
    tissueTermAtDepth,
  )
where

import Data.Fix (Fix)
import Data.List qualified as List
import Moonlight.EGraph.Test.Chimera.Core
  ( TissueF,
    bone,
    cartilage,
    chitin,
    graft,
    keratin,
    marrow,
  )

tissueTermsAtDepth :: Int -> Int -> [Fix TissueF]
tissueTermsAtDepth termDepth termCount =
  fmap
    (tissueTermAtDepth termDepth)
    (take (max 0 termCount) [0 ..])

tissueTermAtDepth :: Int -> Int -> Fix TissueF
tissueTermAtDepth termDepth termIndex =
  case max 0 termDepth of
    0 ->
      marrow (max 0 termIndex)
    graftLayerCount ->
      List.foldl'
        graft
        (marrow (max 0 termIndex))
        (cartilage : fmap tissueLeaf (take (graftLayerCount - 1) [0 ..]))

tissueLeaf :: Int -> Fix TissueF
tissueLeaf termIndex =
  case termIndex `mod` 4 of
    0 -> bone
    1 -> keratin
    2 -> chitin
    _ -> cartilage
