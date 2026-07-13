# Moonlight Differential

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-differential` is the concrete mathematical basis for Moonlight
incremental maintenance. It owns the payload-parametric laws of change:
collection updates, deltas, frontiers, capabilities, traces, compaction,
dependency propagation, and maintained-view scheduling.

Flow, Sheaf, EGraph, Memory, and future runtimes use it as the lawful
maintenance layer.

## Authority

| Concept | Source |
| --- | --- |
| Core maintenance laws: finite support, signed collections, delta action algebra, time/frontiers, batches, traces, streams, locally finite orders, key-indexed arrangements, DBSP-style operators, projection work, propagation, and lifecycle | default `moonlight-differential` library over `src/Moonlight/Differential/**/*.hs` |
| Physical row blocks, generic tuple keys, signed row patches/deltas, reverse indexes, registries, row projection, and row arrangements | `index/Moonlight/Differential/Row/Block.hs`, `index/Moonlight/Differential/Row/Tuple.hs`, `index/Moonlight/Differential/Row/Patch.hs`, `index/Moonlight/Differential/Row/Delta.hs`, plus `index/Moonlight/Differential/Index/*.hs` |
| Context restriction, row cache, local facts, and bounded settlement | `runtime/Moonlight/Differential/**/*.hs` |
| Private representation constructors shared by safe production views and test-support corruption helpers | `internal/Moonlight/Differential/Internal/**/*.hs` via the private `moonlight-differential-raw` sublibrary |
| Intentional invariant-corruption helpers for validation tests | `test-support/Test/Moonlight/Differential/**/*.hs` via `moonlight-differential-test-support` |
| Generic carrier addresses, restriction keys, covers, and families | `runtime/Moonlight/Differential/Carrier/Address.hs` and `runtime/Moonlight/Differential/Carrier/Topology.hs` |
| Dense WCOJ kernels | `join/Moonlight/Differential/Join/WCOJ*.hs` |

## Quick start

A `ZSet` is a signed collection: values paired with `AdditiveGroup` weights,
with any cell whose weight reaches zero pruned on contact. `DeltaOps` is the
delta action algebra over such a collection: combine signed deltas into one,
then apply. Coalescing before applying *is* the maintenance law: the composite
acts as the two edits taken in sequence.

```haskell
import Moonlight.Differential.Algebra.ZSet (ZSet, zsetFromList, zsetToAscList)
import Moonlight.Differential.Delta (deltaApply, deltaCombine, monoidDeltaOps)

inventory :: ZSet String Int
inventory = zsetFromList [("apple", 2), ("pear", 1), ("plum", 3)]

restock, spoilage :: ZSet String Int
restock  = zsetFromList [("plum", 1), ("quince", 4)]
spoilage = zsetFromList [("pear", -1)]

maintained :: ZSet String Int
maintained =
  deltaApply monoidDeltaOps (deltaCombine monoidDeltaOps restock spoilage) inventory
```

`zsetToAscList maintained` yields `[("apple", 2), ("plum", 4), ("quince", 4)]`:
`plum` rises to weight 4, `quince` enters as fresh support, and `pear`'s `+1`
and `-1` annihilate; the dead cell is pruned rather than retained as a zero.

## Ownership invariant

The package owns **runtime-neutral maintenance algebra**. Downstream packages own
concrete interpreters.

```text
payload-parametric differential laws
  -> maintained indexes / frontiers / traces / repair obligations
  -> Flow, Sheaf, EGraph, Memory interpreters
  -> domain-specific rows, sections, facts, proofs, and payloads
```

A mechanism belongs here when it can be stated without Flow schemas/query plans,
Sheaf stalks, EGraph nodes, or Memory facts. If the operation only needs typed
keys, payloads, deltas, support cells, frontiers, and lawful
update/compose/apply operations, it is substrate. If it depends on a concrete
runtime's interpretation policy, query shape, evidence, proof object, or storage
layout, the concrete interpreter keeps that part.

## What belongs here

- Weighted or signed collection-update vocabulary.
- Delta identity, composition, application, and emptiness laws.
- Frontier, capability, pointstamp, and trace-retention laws.
- Trace compaction and summarization protocols.
- Maintained current-view protocols parameterized by payload/delta operations.
- Support and reverse-support indexes when their keys and payloads are generic.
- Packed row blocks, generic tuple keys, row patches, and row deltas when stated without Flow schemas, atom/query identity policy, or query plans.
- Generic carrier addresses, restriction keys, covers, and carrier families before Flow-specific query/carrier policy is attached.
- Contribution ownership laws: delete old contributions by support evidence,
  glue back one canonical current contribution per surviving affected witness.
- Arrangement/update-index contracts, not runtime-specific storage layouts.
- Projection work scheduling contracts and bounded settlement vocabulary.
- Projection/restriction propagation when stated as keyed dependency movement.

## Runtime boundaries

- Flow owns schema validation, query plans, dense cursor layout, carrier dispatch,
  runtime routing, atom/query row identity policy, and row/schema interpretation
  policy. Flow keeps relational patch/event vocabulary such as quotient patches
  and atom events.
- Concrete runtimes own row-specialized join wiring. Differential owns the
  generic operator join contract and the payload-neutral dense WCOJ kernels under
  `Moonlight.Differential.Join.WCOJ`.
- Flow owns query carrier constructors, derived carrier ids, evidence payloads,
  boundary digests, relational origins/scopes, and reuse/materialization policy.
  Generic carrier addresses and families live here.
- Sheaf owns stalk algebras, section stores, restriction laws, descent, and
  gluing. Differential carries maintenance.
- EGraph owns canonicalization, proof objects, extraction costs, and quotient
  epochs, except when they appear as typed payloads flowing through differential
  traces.
- Memory owns admission, belief revision policy, and rendered agent-facing views.
  Differential carries the update layer beneath them.

## Extraction law

When a downstream runtime hand-rolls maintenance logic, extract only the part
that is substrate-neutral.

1. Identify the maintained state, delta, support keys, frontier/time, and current
   view being updated.
2. Strip concrete payloads until only typed keys, payload parameters, and delta
   laws remain.
3. Move that algebra here as the canonical implementation.
4. Rewire the downstream runtime as an interpreter using direct imports.
5. Delete the local duplicate after rewiring callers directly.

## Current relation to Flow

Flow is the concrete relational interpreter. It owns schemas, query plans,
row/schema interpretation, row-specialized WCOJ execution wiring, provenance
payloads, carrier evidence, and runtime-specific repair policy. The
substrate-neutral pieces previously exposed from Flow by accident included
generic tuple keys, row patches/deltas, carrier addresses, and carrier families.
Packed row blocks now live here as `Moonlight.Differential.Row.Block`; tuple keys
now live here as `Moonlight.Differential.Row.Tuple`; signed row patches and row
deltas now live here as `Moonlight.Differential.Row.Patch` and
`Moonlight.Differential.Row.Delta`; Flow keeps atom/query row-block identity
interpretation in `Moonlight.Flow.Model.RowIdentity`. Generic carrier addresses,
restriction keys, covers, and carrier families now live here as
`Moonlight.Differential.Carrier.Address` and
`Moonlight.Differential.Carrier.Topology`; Flow keeps query carriers, derived
carrier ids, topology edges, touches, reuse, and materialization policy.
Payload-neutral WCOJ kernels live in `Moonlight.Differential.Join.WCOJ`; Flow
supplies row/schema interpretation over them.

Differential should absorb the remaining generic machinery Flow exposes by
accident: reverse support indexes, contribution repair laws, trace/frontier read shapes,
maintained current-view protocols, arrangement contracts, and
operator-scheduler laws. The end state is Flow as a high-performance
interpreter over this package.

## Law surface

Every substrate addition should come with tests for the laws it claims:

- delta identity and associativity;
- apply/compose action law;
- empty delta neutrality;
- frontier monotonicity and invalid advance rejection;
- capability transport/downgrade validity;
- trace compaction equivalence to replay under retained frontiers;
- support-index insert/delete/reindex round trips;
- maintained current-view equality against replay;
- projection/restriction propagation equivalence against full recomputation;
- quiescence and bounded-settle behavior.

A failure in these laws is a typed obstruction. Return the obstruction rather
than an empty delta.

## Scope

`moonlight-differential` is the layer where Moonlight's
differential-maintenance laws become one concrete mathematical machine shared by
interpreters.
