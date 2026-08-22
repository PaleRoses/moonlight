# Changelog

`moonlight-triangulation` follows the
[Haskell Package Versioning Policy](https://pvp.haskell.org).

The serialization format carries its own version tag, independent of the package
version; any change to it is recorded here explicitly.

## 1.3.0.0

* Add the public GHC-9.14 `cell-complex` component. It interprets an admitted
  `ExactCellSet` as Homology's generic `CellComplex2D` without copying the mesh
  or introducing a second cell inventory, and the accompanying observatory
  executable derives its incidence category and normalized flag nerve.
* Keep the LLVM-optimized hot modules buildable with GHC 9.8 by restoring the
  legacy LLVM pass manager that its supported LLVM 15 toolchain requires.
* Withdraw `constrainedExtensionTriangulation`,
  `constrainedExtensionConstraintOutcomes` and
  `constrainedExtensionConstraintStats`. A constrained extension now publishes
  one `ConstraintBatchResult` through `constrainedExtensionConstraintBatch`,
  read with `constraintBatchTriangulation`, `constraintBatchOutcomes` and
  `constraintBatchStats`; `constrainedExtensionBuildStats` is unchanged. The
  batch is the same value `recoverConstraints` returns, so extension and
  standalone recovery are now read through one accessor set instead of two.
* Withdraw the aggregator modules `Moonlight.Triangulation.Handles`,
  `Moonlight.Triangulation.Handles.Iterators` and
  `Moonlight.Triangulation.Internal.DcelOperations` from the exposed module
  list. Every leaf they re-exported remains exposed under its own name; import
  the leaf.
* Add `BoundaryOrientation`, `boundaryLoopOrientation` and
  `componentBoundaryLoops`, so a face component publishes all of its boundary
  loops with each loop's orientation rather than only the outer one.
* Add `planarValuationsPerimeter`, the certified perimeter of a planar region
  taken directly from its valuations.
* Advance the C boundary to ABI version 2 with opaque exact-region and reusable
  structuring-element handles, one-call bulk authoring/projection, exact region
  union/intersection/difference/symmetric difference, point location, exact
  area plus Euler and certified perimeter observations, and existing polygonal
  Minkowski morphology with fixed-width receipts.
* Rename the former mesh Boolean symbols and Python, TypeScript, and Rust
  methods to `site_*`; they combine Delaunay site sets and are deliberately not
  compatibility aliases for the new polygonal region operations.
* Restore `-O0` for ordinary test bodies while retaining the filtered-predicate
  allocation witness at module-local `-O1`.
* Move the existing exact Overlay/Minkowski implementations from the `build`
  sublibrary into the main library and move `HintGenerator` from `dual` into
  that same apex owner. The `dual` sublibrary is now dcel-only and can compile
  concurrently with `build`; direct `:dual` consumers of `HintGenerator` must
  depend on the main library instead. No geometry type or runtime operation is
  duplicated.
* Reserve the planar DCEL bound rather than the general one when joining two
  separated triangulations. The separated seam copies two planar sources and
  then only adds, so its peak is its published result; the general reservation
  was a third again as much arena as the merge can ever reach. Measured at
  twenty thousand sites over twenty-one processes per arm, the separated join
  lanes fall 36.4% to 41.3% in allocation and 10.2% to 22.0% in elapsed time,
  with every unaffected lane byte-identical.
* Stop materializing an intermediate validated vector in `mesh_insert_many_f64`.
  Admission is unchanged — a malformed point anywhere still refuses the whole
  batch before any insertion — but the canonical coordinate is now applied
  where the point is used. Against a fifty-thousand-site mesh, allocation falls
  5.3% at a thousand added points and 26.5% at fifty thousand; wall time is
  unmoved, because geometric insertion, not admission, is the critical path.

## 1.2.0.1

* Admit GHC 9.8 as a tested compiler by spelling the package language as
  `GHC2021` plus `DerivingStrategies`, accepting `base-4.19`, and qualifying
  strict list folds through `Data.List`. This restores Hackage build and
  documentation generation without changing the API or binary format.

## 1.2.0.0

* Add exact rational planar regions and labelled common refinement with one
  provenance-bearing overlay carrier, closed 0-/1-/2-cell Boolean selection,
  and grouped polygon publication through the existing DCEL boundary owner.
* Add exact Euler characteristic and rational area plus symbolic radical length
  expressions with certified outward-rounded binary64 bounds.
* Add linear convex-polygon Minkowski convolution, general polygonal addition,
  and regularized polygonal erosion, opening, closing, offset, and inset through
  the existing CDT and overlay owners.
* Curate the exact region algebra through the main Haskell facade. The C ABI and
  language bindings remain intentionally unchanged.
* Change the binary wire format to version 6 so round trips preserve the vertex
  payload plane's optional fill, including the allocation-free unit payload
  used by geometry-only bulk construction. Version 5 is intentionally
  unsupported rather than decoded into a different resident representation.
* Map the dense-storage circle-sweep obstruction exhaustively at the C boundary
  as obstruction code 55.

## 1.1.0.0

* Add labelled bounded-face components and authoritative component boundaries
  with counter-clockwise outer loops, clockwise holes, exact collinear-vertex
  simplification, and typed pinch obstructions.
* Add exact Delaunay 2-simplex alpha filtration through
  `alphaShapeContainsFace` and the shared admitted `RadiusSquared` type.
* Replace `joinSeparatedConstrainedWith` with
  `joinSeparatedConstrained`; strict separation cannot combine coincident
  annotations, so the dead combiner and old name are gone.
* Change the binary wire format to version 5. Structural section counts now
  occupy one prefix and `decodeTriangulation` requires an explicit
  `DecodingBudget` plus `TrustedPayloadDecoders`, validating counts,
  relationships, packed-index bounds, a fixed-body lower bound, and total
  section elements before allocation while making external decoder trust
  explicit. Version 4 is intentionally unsupported.

## 1.0.1.0

* Add the geometry-only `delaunayGeometry` entrance and re-export
  `delaunayFromCoordinates` with its duplicate-payload policy from the main
  facade.
* Add one versioned C ABI over immutable geometry meshes, with typed
  obstruction witnesses and thin Python, TypeScript, and Rust bindings.

## 1.0.0.0

* Specialize the public geometry surface to binary64 and remove the ornamental
  scalar parameter from points, queries, triangulations, sessions, hierarchy
  hints, interpolation workspaces, and result types.
* Add dense coordinate and inner-face vertex projections to the existing DCEL
  surface.
* Add exact support relations and finite-set operations over triangulations,
  with local publication for sparse differences, intersections, and symmetric
  differences.
* Schedule singleton insertion and constrained extension between dense and
  locality-preserving transactions from measured workload evidence.
* Resolve dense coordinate-removal batches through one mutable identity index,
  retaining geometric descent for sparse removals.
* Strengthen constrained extension, refinement, serialization, algebra, and
  hostile-boundary validation.

## 0.1.0.0

* Initial release.
