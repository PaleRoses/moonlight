module CaseLift.Tail where
tailKnown xs = case xs of
  [] -> []
  y:ys -> tail xs
