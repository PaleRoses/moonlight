module ProbeE where

probeE v0 = case v0 of
  [] -> (\x -> use x x) alpha
  _ -> zero
