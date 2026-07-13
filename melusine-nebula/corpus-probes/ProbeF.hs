module ProbeF where

probeF = \y -> combine y ((\x -> use x x) alpha)
