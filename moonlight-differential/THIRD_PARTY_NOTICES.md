# Third-party notices

## Acknowledgements (algorithm provenance, no derived code)

The fueled trace spine in this package — batch merging, spine insertion,
cursor seek/step over monotone key ranges, the fueled list merger, and the
batcher/builder construction discipline — is a from-scratch pure Haskell
realisation of algorithms studied in the Feldera DBSP engine. The relevant
modules (`Moonlight.Differential.Batch`, `Moonlight.Differential.Trace`,
`Moonlight.Differential.Arrangement`) carry per-function source anchors of
the form `feldera/crates/dbsp/src/...` naming the Rust implementation each
algorithm was studied from. No code is derived or translated; the anchors
are deliberate provenance and must not be stripped.

- Feldera: <https://github.com/feldera/feldera>
- Mihai Budiu, Tej Chajed, Frank McSherry, Leonid Ryzhyk, and Val Tannen.
  "DBSP: Automatic Incremental View Maintenance for Rich Query Languages."
  Proceedings of the VLDB Endowment, 16(7), 2023.
  <https://www.vldb.org/pvldb/vol16/p1601-budiu.pdf>

The incremental operator tier (delta joins, arrangements shared across
operators, semi-naive fixpoints) follows the differential dataflow lineage:

- Frank McSherry, Derek G. Murray, Rebecca Isaacs, and Michael Isard.
  "Differential dataflow." CIDR 2013.
- differential-dataflow: <https://github.com/TimelyDataflow/differential-dataflow>

The worst-case-optimal join tier implements the generic-join framework:

- Hung Q. Ngo, Ely Porat, Christopher Ré, and Atri Rudra. "Worst-case
  Optimal Join Algorithms." JACM 65(3), 2018. arXiv:1203.1952.

The stream calculus (Möbius differentiation and integration over locally
finite orders) is classical incidence-algebra mathematics:

- Gian-Carlo Rota. "On the foundations of combinatorial theory I: Theory
  of Möbius functions." Zeitschrift für Wahrscheinlichkeitstheorie und
  Verwandte Gebiete 2, 1964.

Thank you to the Feldera and TimelyDataflow authors and communities; their
engineering made this package possible.
