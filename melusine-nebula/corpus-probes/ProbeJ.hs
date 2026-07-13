module ProbeJ where

probeJ action = do
  x <- action
  pure ((\y -> use y y) alpha)
