-- | Owner-bound compilation of section descent kernels.  Mutable kernel
-- execution is intentionally confined to the hidden implementation module.
module Moonlight.Sheaf.Section.Store.Descent.FastPath
  ( prepareAlgebraSectionDescent,
  )
where

import Moonlight.Sheaf.Section.Stalk (StalkAlgebra)
import Moonlight.Sheaf.Section.Store.Descent.FastPath.Internal
  ( compileFastEditKernel,
  )
import Moonlight.Sheaf.Section.Store.Internal
  ( AlgebraPreparedSectionDescent (AlgebraPreparedSectionDescentInternal),
    algebraPreparedFastEditKernelsInternal,
    algebraPreparedSectionDescentInternal,
    algebraPreparedStalkAlgebraInternal,
    preparedSectionDescentFastEditProgramsInternal,
  )
import Moonlight.Sheaf.Section.Store.Types
  ( PreparedSectionDescent,
    psdViews,
  )

prepareAlgebraSectionDescent ::
  PreparedSectionDescent owner cell witness ->
  StalkAlgebra witness stalk mismatch repairObstruction ->
  AlgebraPreparedSectionDescent owner cell witness stalk mismatch repairObstruction
prepareAlgebraSectionDescent preparedDescent stalkAlgebra =
  AlgebraPreparedSectionDescentInternal
    { algebraPreparedSectionDescentInternal = preparedDescent,
      algebraPreparedStalkAlgebraInternal = stalkAlgebra,
      algebraPreparedFastEditKernelsInternal =
        fmap
          (fmap (compileFastEditKernel preparedDescent stalkAlgebra))
          (preparedSectionDescentFastEditProgramsInternal (psdViews preparedDescent))
    }
