{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Context.Match.State.Registry
  ( QueryRegistry,
    emptyQueryRegistry,
    registeredQueryIds,
    lookupQueryIdByFingerprint,
    registerQueryFingerprint,
    registerQueryFingerprints,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Moonlight.Core
  ( QueryId,
    mkQueryId,
  )
import Moonlight.Saturation.Context.Match.Types.Plan
  ( QueryFingerprint (..),
  )

type QueryRegistry :: Type -> Type
data QueryRegistry plan = QueryRegistry
  { qrNextId :: !Int,
    qrByFingerprint :: !(IntMap.IntMap QueryId)
  }

type role QueryRegistry nominal

emptyQueryRegistry :: QueryRegistry plan
emptyQueryRegistry =
  QueryRegistry
    { qrNextId = 0,
      qrByFingerprint = IntMap.empty
    }
{-# INLINE emptyQueryRegistry #-}

registeredQueryIds :: QueryRegistry plan -> [QueryId]
registeredQueryIds registry =
  fmap mkQueryId [0 .. qrNextId registry - 1]
{-# INLINE registeredQueryIds #-}

lookupQueryIdByFingerprint ::
  QueryFingerprint ->
  QueryRegistry plan ->
  Maybe QueryId
lookupQueryIdByFingerprint fingerprint =
  IntMap.lookup (queryFingerprintKey fingerprint) . qrByFingerprint
{-# INLINE lookupQueryIdByFingerprint #-}

registerQueryFingerprint ::
  QueryFingerprint ->
  QueryRegistry plan ->
  (QueryId, QueryRegistry plan)
registerQueryFingerprint fingerprint registry =
  case lookupQueryIdByFingerprint fingerprint registry of
    Just existingQueryId ->
      (existingQueryId, registry)
    Nothing ->
      let freshQueryId =
            mkQueryId (qrNextId registry)

          fingerprintKey =
            queryFingerprintKey fingerprint
       in ( freshQueryId,
            registry
              { qrNextId = qrNextId registry + 1,
                qrByFingerprint =
                  IntMap.insert
                    fingerprintKey
                    freshQueryId
                    (qrByFingerprint registry)
              }
          )
{-# INLINE registerQueryFingerprint #-}

registerQueryFingerprints ::
  Foldable fingerprints =>
  fingerprints QueryFingerprint ->
  QueryRegistry plan ->
  QueryRegistry plan
registerQueryFingerprints fingerprints registry =
  foldl'
    (\currentRegistry fingerprint -> snd (registerQueryFingerprint fingerprint currentRegistry))
    registry
    fingerprints
{-# INLINE registerQueryFingerprints #-}
