module Moonlight.Sketch.Pure.Validate.Algebra
  ( validateAlgebra,
    validateReference,
    unresolvedRefValidator,
    unresolvedRefMessage,
    cyclicRefMessage,
    issue,
    validateString,
    validateNumber,
    validateArrayWith,
    validateTupleWith,
    validateRecordWith,
    validateObjectWith,
  )
where

import Moonlight.Sketch.Pure.Validate.Algebra.Composite
import Moonlight.Sketch.Pure.Validate.Algebra.Core
import Moonlight.Sketch.Pure.Validate.Algebra.Primitive (validateNumber, validateString)
