module CaseLift.Map where
mapLike f xs = case xs of
  [] -> []
  y:ys -> f y : mapLike f ys
