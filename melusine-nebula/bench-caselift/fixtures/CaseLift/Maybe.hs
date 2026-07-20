module CaseLift.Maybe where
maybeLike d f m = case m of
  Nothing -> d
  Just y -> f y
