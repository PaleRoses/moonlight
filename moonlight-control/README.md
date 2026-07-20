# moonlight-control

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-control` is the typed control algebra for Pale Meridian schedulers,
gates, and deterministic work engines.  Its source of truth is
`Program ctx phase`; the `Control` class is the O(1) constructor interface and
all other interpreters are folds of the canonical program.

## Algebra table

| Primitive | Meaning | Cost |
|---|---|---|
| `skip` | empty program; identity for `andThen` | O(1) |
| `phase p` | one domain phase | O(1) |
| `andThen x y` | sequential composition | O(1) |
| `orElse x y` | ordered choice; no identity | O(1) |
| `upTo n x` | bounded repetition | O(1) |
| `attempt x` | speculative execution with rollback on no-progress | O(1) |
| `scoped m x` | modal context over a region | O(1) |
| `normalize` | canonicalize a program | O(n) |
| `foldProgram` | eliminate a program | O(n) |

`orElse`, `attempt`, and `upTo` intentionally have negative laws; see
`Moonlight.Control.Laws.obstructionLaws`.

## Quick start

A program is a pure value in `Program ctx phase`, assembled from the `Control`
constructors and consumed by folding. Here the phase vocabulary is `String` and
there is no modal context (`()`):

```haskell
import Moonlight.Control
import Numeric.Natural (Natural)

plan :: Program () String
plan =
  sequenceAll
    [ phase "discover"
    , attempt (phase "repair")
    , upTo 3 (phase "saturate")
    ]

-- Phases in left-to-right order: ["discover","repair","saturate"].
ordered :: [String]
ordered = programPhases plan

-- Every interpreter is a fold of the canonical carrier; this one counts leaves.
phaseCount :: Program ctx p -> Natural
phaseCount =
  foldProgram
    ProgramAlgebra
      { paSkip    = 0
      , paPhase   = const 1
      , paSeq     = (+)
      , paOr      = (+)
      , paUpTo    = \_ body -> body
      , paAttempt = id
      , paScoped  = \_ body -> body
      }
```

`plan` stays inert data until an interpreter observes it. `programPhases` and
`phaseCount` are two folds of the same `Program`, and every heavier interpreter,
including the machine, renderers, and analyzers, is a `foldProgram` of exactly this shape.

## Worked example

```haskell
import Moonlight.Control
import Moonlight.Control.Modality
import Moonlight.Control.Gate
import Moonlight.Control.Weight

program :: Program (Modality view group match trace group) String
program =
  gated myGate $
    weighted myPriorityProfile $
      sequenceAll
        [ phase "discover"
        , attempt (phase "repair")
        , upTo 3 (phase "saturate")
        ]
```

The engine receives the composed `Modality` for each phase.  A gate filters the
candidate space; a weight profile biases scheduling; the program remains a pure
value until interpreted.

## Main modules

| Module | Role |
|---|---|
| `Moonlight.Control` | front door re-export |
| `Moonlight.Control.Class` | `Control` class and law docs |
| `Moonlight.Control.Program` | canonical deep embedding and fold |
| `Moonlight.Control.Machine` | interpreter producing `Execution` / `Report` |
| `Moonlight.Control.Modality` | scoped product of `Gate` and `PriorityProfile` |
| `Moonlight.Control.Gate` | guidance/gating selectors |
| `Moonlight.Control.Weight` | priority evidence and profiles |
| `Moonlight.Control.Schedule` | scheduler vocabulary |
| `Moonlight.Control.Schedule.Round` | one-round scheduler interpreter |
| `Moonlight.Control.Engine.*` | type-stated engine, plans, reports, parallelism |
| `Moonlight.Control.Laws` | exported property bundles |
