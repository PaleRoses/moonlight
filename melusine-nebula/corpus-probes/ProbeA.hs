module ProbeA where

probeA v0 = case v0 of
  Just value -> (\x -> use x x) value
  Nothing -> zero
