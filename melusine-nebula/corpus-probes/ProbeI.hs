module ProbeI where

data Box = Box { payload :: Int } | Empty { emptyTag :: Int }

probeI box = case box of
  Box { payload = value } -> combine value
  Empty {} -> (\x -> use x x) alpha
