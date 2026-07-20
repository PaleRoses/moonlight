module ProbeL where

shareC = combine gamma (shareCD alpha)

shareD = combine gamma (shareCD beta)

shareCD step = consume (wrap step) (wrap step) (wrap step)
