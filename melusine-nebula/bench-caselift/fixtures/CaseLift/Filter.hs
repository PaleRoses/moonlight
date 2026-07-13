module CaseLift.Filter where
filterLike p xs = case xs of
  [] -> []
  y:ys -> case p y of
    True -> y : filterLike p ys
    False -> filterLike p ys
