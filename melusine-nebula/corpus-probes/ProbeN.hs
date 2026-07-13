module ProbeN where

probeN = combine ((\y -> combine y ((\x -> use x x) alpha)) beta) gamma
