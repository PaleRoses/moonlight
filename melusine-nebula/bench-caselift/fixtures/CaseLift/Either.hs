module CaseLift.Either where
eitherKnown e = case e of
  Left x -> either (const True) (const True) e
  Right y -> either (const True) (const True) e
