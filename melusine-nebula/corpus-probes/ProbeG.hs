module ProbeG where

probeG v0
  | Just value <- v0 = (\x -> use x x) value
  | otherwise = zero
