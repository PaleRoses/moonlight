module CaseLift.Fold where
foldLike f z xs = case xs of
  [] -> z
  y:ys -> f y (foldLike f z ys)
