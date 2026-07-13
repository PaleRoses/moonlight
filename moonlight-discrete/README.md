# moonlight-discrete

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-discrete` owns Moonlight's finite and discrete structural substrate:

automata over recursion-scheme carriers, Boolean/CSP/WFC constraint kernels,
probabilistic WFC candidate ordering, typed optic addressing and write/delta
boundaries, graph selectors and local topology, and repair-index closure support.

## Components

The package is split into semantic sublibraries so consumers depend on the exact
section they need. Law harnesses and probability remain explicit sections:

- `moonlight-discrete:automata`: deterministic/nondeterministic tree automata,
  top-down automata, tree languages, and tree transducers.
- `moonlight-discrete:constraint`: Boolean constraint expressions, CNF/DPLL,
  CSP arc consistency, and deterministic WFC search.
- `moonlight-discrete:constraint-probability`: probabilistic WFC compilation,
  entropy/weighted candidate ordering, and seeded pure sampling.
- `moonlight-discrete:optics`: typed optic boundaries, indexed addressing,
  restrictions, and write plans.
- `moonlight-discrete:moonlight-discrete-laws`: public automata, constraint, and
  optics law harnesses.
- `moonlight-discrete:graph-core`: private graph entity references, attributes,
  deltas, selectors, views, and graph optics.
- `moonlight-discrete:graph`: graph local topology and the public `Moonlight.Graph`
  surface.
- `moonlight-discrete:repair-index`: repair dependency closures over integer
  adjacency indexes.

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.Automata` | Broad automata surface over bottom-up/top-down automata, languages, and transducers. |
| `Moonlight.Constraint` | Broad deterministic constraint surface over Boolean/CSP/WFC kernels. |
| `Moonlight.Constraint.Pure.WFC.Probability` | Probabilistic WFC extension surface. |
| `Moonlight.Optics` | Optic addressing, multiplicity, restrictions, write plans, delta boundaries, and TH helpers. |
| `Moonlight.Graph` | Graph references, attributes, deltas, selectors, views, optics, and local topology. |
| `Moonlight.Repair.Index` | Integer repair closure and support-frontier helpers. |
| `Moonlight.Automata.Effect.*`, `Moonlight.Constraint.Effect.*`, `Moonlight.Optics.Effect.*` | Public law harness modules from `moonlight-discrete:moonlight-discrete-laws`. |

## Benchmarks

`tasty-bench` covers automata, constraints, optics, graph operations, and repair-index kernels.
