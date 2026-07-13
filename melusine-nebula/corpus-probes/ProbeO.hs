module ProbeO where

probeO input = combine input (mapper (\y -> combine y ((\x -> use x x) alpha)))
