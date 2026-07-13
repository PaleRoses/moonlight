# moonlight-delta

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-delta` is Moonlight's checked state-change algebra: keyed patches, signed
multiplicities, invalidation scopes, frontiers, epochs, monotone operators, and bounded
repair. Operations return typed incompatibilities where a transition cannot be applied.

## Main idea

A `CellPatch value` records both endpoints of one keyed transition: absent to absent,
absent to present, present to absent, or present to present. Build cells with
`assertAbsent`, `insert`, `delete`, `replace`, or `cellFromEndpoints`. `apply` checks
the before endpoint, and `compose` checks that adjacent endpoints connect.

Use patches for concurrent updates, retries, synchronization, authorization-sensitive
changes, and reconciliation.

## Use

```haskell
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch qualified as Patch

changeTitle :: Patch.Patch Int String
changeTitle = Patch.singleton 7 (Patch.replace "Draft" "Published")

committed
  :: Either (Patch.ApplyError Int String) (Map Int String)
committed = Patch.apply changeTitle (Map.singleton 7 "Draft")
```

## Cookbook

Every recipe below type-checks against a public `moonlight-delta` surface.

### Compose and replay patches

`compose newer older` collapses two connected transitions. `replay` applies a
chronological sequence and reports the failing index.

```haskell
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch qualified as Patch

draft, reviewed, published :: Map Int String
draft = Map.singleton 7 "Draft"
reviewed = Map.singleton 7 "Review"
published = Map.singleton 7 "Published"

draftToReview, reviewToPublished :: Patch.Patch Int String
draftToReview = Patch.diff draft reviewed
reviewToPublished = Patch.diff reviewed published

publication
  :: Either (Patch.ComposeError Int String) (Patch.Patch Int String)
publication = Patch.compose reviewToPublished draftToReview

replayed
  :: Either (Patch.ReplayError Int String) (Map Int String)
replayed = Patch.replay [draftToReview, reviewToPublished] draft

history
  :: Either (Patch.ComposeError Int String) (Patch.Patch Int String)
history =
  Patch.recordMany
    [ (7, Patch.replace "Draft" "Review")
    , (7, Patch.replace "Review" "Published")
    ]
```

Use `invert` to reverse a patch. Repeated temporal keys belong in `recordMany`;
`fromList` instead means authoritative, last-wins rows.

### Apply signed multiplicities

`Signed` stores integer changes while the state retains non-negative multiplicities.

```haskell
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Signed qualified as Signed

inventory :: Map String Signed.Multiplicity
inventory = Map.singleton "rose" (Signed.Multiplicity 2)

sale :: Signed.Signed String
sale = Signed.signedFromList [("rose", -1), ("lily", 2)]

afterSale
  :: Either
       (Signed.SignedApplyError String)
       (Map String Signed.Multiplicity)
afterSale = Signed.applySignedToMap sale inventory
```

An underflow returns `SignedMultiplicityUnderflow`; a zero result removes the row.
Combine changes with `combineSigned` and reverse one with `negateSigned`.

### Bound invalidation with a scope

`Scope` distinguishes no invalidation, a finite dirty set, and global invalidation.

```haskell
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Scope qualified as Scope

dirtyPages :: Scope.Scope (Set Int)
dirtyPages = Scope.dirtyScope (Set.fromList [2, 7])

visibleDirtyPages :: Scope.Scope (Set Int)
visibleDirtyPages = Scope.restrictScope (Set.singleton 7) dirtyPages
```

`scopeKeys` returns `Nothing` for `fullScope`.

### Track two-axis progress

`ProductFrontier2` canonicalizes a two-dimensional progress skyline by removing
dominated points.

```haskell
import Moonlight.Delta.Frontier qualified as Frontier

progress :: Frontier.ProductFrontier2 Int Int
progress = Frontier.mkProductFrontier2 [(0, 3), (1, 2), (2, 1), (3, 0)]

covered :: Bool
covered = Frontier.productFrontier2Contains (2, 2) progress
```

Use `mkFrontier` or `mkUpperFrontier` for a general `PartialOrder`.

### Transport keys across an epoch

An `EpochDelta` checks a partial transport between versioned key universes. A query is
partitioned into transported, retired, and unknown keys.

```haskell
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Moonlight.Delta.Epoch qualified as Epoch

source, target :: Epoch.Endpoint IntSet
source = Epoch.Endpoint Epoch.initialVersion (IntSet.fromList [10, 20])
target =
  Epoch.Endpoint
    (Epoch.nextVersion Epoch.initialVersion)
    (IntSet.fromList [11, 30])

advance
  :: Either
       (Epoch.DeltaViolation Int)
       (Epoch.EpochDelta (IntMap Int) IntSet)
advance =
  Epoch.epochDelta
    source
    target
    (IntMap.singleton 10 11)
    (IntSet.singleton 20)
    (IntSet.singleton 10)

query
  :: Either (Epoch.DeltaViolation Int) (Epoch.Transport (IntMap Int) IntSet)
query =
  fmap
    (\deltaValue -> Epoch.transportKeys deltaValue (IntSet.fromList [10, 20, 99]))
    advance
```

Invalid versions, universes, transports, retirements, or changed keys return a
`DeltaViolation`. Here `query` partitions `10 -> 11`, retired `20`, and unknown `99`;
changed key `10` and fresh key `30` determine the dirty target set.

### Run bounded repair

A repair `Kernel` checks obstructions, turns repairable ones into corrections, and
applies those corrections under explicit fuel.

```haskell
import Data.List.NonEmpty (NonEmpty ((:|)))
import Moonlight.Repair qualified as Repair

data TooSmall = TooSmall Int
data Increment = Increment

incrementKernel :: Int -> Repair.Kernel Int TooSmall Increment
incrementKernel target =
  Repair.Kernel
    { Repair.check = \state ->
        if state >= target
          then Repair.StepConverged state
          else Repair.StepObstructed state (TooSmall target :| [])
    , Repair.residuate = \_ -> Just Increment
    , Repair.applyKernelCorrection = \state Increment -> state + 1
    }

repaired :: Repair.Result Int TooSmall
repaired = Repair.boundedRepair (incrementKernel 2) (Repair.Config 4) 0
```

The result is `ResultConverged 2 2`; the other terminal cases are `ResultStuck` and
`ResultBudgetExhausted`. Compose kernels with `sequenceRepair`, `productRepair`, or
`focusRepair`.

## Surface & boundaries

There is no umbrella library. Depend on the smallest public slice you use.

| Cabal dependency | Import | What you get |
| --- | --- | --- |
| `moonlight-delta:moonlight-delta-core` | `Moonlight.Delta.Signed`, `.Scope`, `.Frontier`, `.Monotone`, `.Normalize`, `.Support`, `.Operator`, `.Time` | Signed changes, invalidation, progress frontiers, operators, and their shared laws. |
| `moonlight-delta:moonlight-delta-patch` | `Moonlight.Delta.Patch` | Checked keyed transitions, composition, diff, inversion, and replay. |
| `moonlight-delta:moonlight-delta-epoch` | `Moonlight.Delta.Epoch` | Versioned partial key transport and view restamping. |
| `moonlight-delta:moonlight-delta-repair` | `Moonlight.Repair` | Bounded obstruction repair and kernel composition. |

## Test

```bash
cabal test moonlight-delta:moonlight-delta-test
```

## License

MIT. See `LICENSE`.
