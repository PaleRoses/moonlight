import Lake
open Lake DSL

package "moonlight_egraph_proofs" where

lean_lib "MoonlightEGraphProofs" where
  roots := #[`EGraphKernel, `EGraphRefinement, `ContextLattice, `ContextSheaf, `SupportStratification, `TwistScoping, `ObstructionCohomology, `CoverageFromCocycles]

lean_exe "egraph-phase-a-kernel" where
  root := `PhaseAKernel

lean_exe "theorem-manifest" where
  root := `TheoremManifest
