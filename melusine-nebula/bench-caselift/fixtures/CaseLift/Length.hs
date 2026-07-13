module CaseLift.Length where
lengthLike xs = case xs of
  [] -> 0
  y:ys -> 1 + lengthLike ys
