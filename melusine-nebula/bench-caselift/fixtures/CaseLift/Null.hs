module CaseLift.Null where
nullLike xs = case xs of
  [] -> null xs
  y:ys -> null xs
