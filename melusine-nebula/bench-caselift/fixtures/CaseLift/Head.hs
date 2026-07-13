module CaseLift.Head where
headDefault xs = case xs of
  [] -> Nothing
  y:ys -> Just (head xs)
