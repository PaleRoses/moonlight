module Moonlight.Optics.Boundary
  ( etaRead,
    emitDelta,
    emitDeltaWith,
    boundaryRead,
    boundaryWrite,
  )
where

import Control.Monad.Writer.Class (MonadWriter (tell))
import Moonlight.Optics.Pure.Delta (DeltaIR, DeltaOptic, singletonDelta)
import qualified Moonlight.Optics.Pure.Delta as Delta
import Moonlight.Optics.Pure.Multiplicity (ReadOptic, viewRead)
import Moonlight.Optics.Pure.Write (WriteOptic)

etaRead :: Applicative f => ReadOptic source focus -> source -> f focus
etaRead optic source =
  pure (viewRead optic source)

emitDelta :: MonadWriter (DeltaIR delta) m => DeltaOptic delta source target focus updated -> (focus -> updated) -> source -> m ()
emitDelta optic update source =
  tell (Delta.emitDelta optic update source)

emitDeltaWith :: MonadWriter (DeltaIR delta) m => (source -> target -> delta) -> WriteOptic source target focus updated -> (focus -> updated) -> source -> m ()
emitDeltaWith encoder optic update source =
  tell (singletonDelta (Delta.deltaEvent encoder optic update source))

boundaryRead :: Applicative f => (ReadOptic source focus, source) -> f focus
boundaryRead (optic, source) = etaRead optic source

boundaryWrite :: MonadWriter (DeltaIR delta) m => DeltaOptic delta source target focus updated -> (focus -> updated) -> source -> m ()
boundaryWrite = emitDelta
