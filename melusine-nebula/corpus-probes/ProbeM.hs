module ProbeM where

probeM = \y -> \z -> combine y (combine z ((\x -> use x x) alpha))
