module Moonlight.Geometry.Gluing.Laws
  ( propHardBooleanLowerBoundClosure,
    propHardBooleanNotExact,
    propOnionPreservesCertificate,
    propZeroSetFullAlgebra,
    propMetricDegenerateOnly,
    propProxyAdmissibility,
  )
where

import Moonlight.Geometry.Gluing.Safety
import Moonlight.Geometry.Global.Acceleration
import Moonlight.Geometry.Section.Propagation
  ( hardBooleanCertificate,
    sdfTokenDistanceCertificate,
  )
import Moonlight.Geometry.Site.Semantics
import Moonlight.Geometry.Site.Token

propHardBooleanLowerBoundClosure :: DistanceCertificate -> DistanceCertificate -> Bool
propHardBooleanLowerBoundClosure leftCertificate rightCertificate =
  case dcSemantics (hardBooleanCertificate Nothing leftCertificate rightCertificate) of
    ExactDist -> False
    _ -> True

propHardBooleanNotExact :: Bool
propHardBooleanNotExact =
  dcSemantics (hardBooleanCertificate Nothing exactCertificate exactCertificate) /= ExactDist

propOnionPreservesCertificate :: Double -> DistanceCertificate -> Bool
propOnionPreservesCertificate thickness certificate =
  sdfTokenDistanceCertificate (Onion thickness certificate) == certificate

propZeroSetFullAlgebra :: Bool
propZeroSetFullAlgebra =
  all
    (== FullBooleanAlgebra)
    [ lawfulnessForInterface ZeroSetSemantics InterfaceHardUnion,
      lawfulnessForInterface ZeroSetSemantics InterfaceHardSubtract,
      lawfulnessForInterface ZeroSetSemantics InterfaceHardIntersect
    ]

propMetricDegenerateOnly :: Bool
propMetricDegenerateOnly =
  all
    (== DegenerateOnly)
    [ lawfulnessForInterface MetricSemantics InterfaceHardUnion,
      lawfulnessForInterface MetricSemantics InterfaceHardSubtract,
      lawfulnessForInterface MetricSemantics InterfaceHardIntersect
    ]

propProxyAdmissibility :: Bool
propProxyAdmissibility =
  not (admissibleUnderSmoothParent ConservativeProxy)
    && admissibleUnderSmoothParent LipschitzProxy
