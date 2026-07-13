module ProbeK where

shareA v0 = case v0 of
  Just value -> combine value (consume (wrap alpha) (wrap alpha) (wrap alpha))
  Nothing -> zero

shareB v0 = case v0 of
  Just value -> combine value (consume (wrap beta) (wrap beta) (wrap beta))
  Nothing -> zero
