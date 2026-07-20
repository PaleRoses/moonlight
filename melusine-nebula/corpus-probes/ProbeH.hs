module ProbeH where

probeH v0 = case v0 of
  Just value -> (\x -> use x x) alpha
  Nothing -> zero
