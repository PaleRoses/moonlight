module ProbeB where

data Box = Box { payload :: Int } | Empty { emptyTag :: Int }

probeB box = case box of
  Box { payload = value } -> combine value ((\x -> use x x) alpha)
  Empty {} -> fallback box
