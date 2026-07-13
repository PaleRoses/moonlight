module ProbeC where

data Box = Box { payload :: Int } | Empty { emptyTag :: Int }

probeC box = case box of
  Box { payload = value } -> (\x -> use x x) value
  Empty {} -> fallback box
