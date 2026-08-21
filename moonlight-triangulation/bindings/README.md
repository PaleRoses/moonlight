# Foreign bindings

`moonlight-triangulation.cabal` owns the private Haskell `ffi` sublibrary and
`moonlight-triangulation-c` foreign library. Python, TypeScript, and Rust are
thin lifecycle and bulk-layout leaves over that sole ABI; none implements
triangulation, overlay, publication, valuation, or morphology.

From `compiler`, build and locate the shared library:

```bash
../scripts/safe-cabal.sh build \
  moonlight-triangulation:flib:moonlight-triangulation-c \
  --project-file=cabal.project.triangulation-dev --enable-shared -j1
cabal list-bin moonlight-triangulation:flib:moonlight-triangulation-c \
  --project-file=cabal.project.triangulation-dev --enable-shared
```

Set `MOONLIGHT_TRIANGULATION_LIBRARY` to that `.dylib`, `.so`, or `.dll`; Rust
linking also reads its directory from `MOONLIGHT_TRIANGULATION_LIB_DIR`.

## ABI version 2

Three opaque immutable carriers cross the boundary:

- `ml_mesh` retains a binary64 Delaunay mesh. Its `ml_mesh_site_*` algebra
  combines sites, not polygon interiors.
- `ml_region` retains the authoritative exact rational `PlanarRegion`.
  `ml_region_create_f64` admits every input coordinate exactly as its binary64
  dyadic value in one call, using point counts per loop and loop counts per
  component. The inverse bulk projection returns component/loop offsets and
  binary64 rendering points; it does not replace the exact retained geometry.
- `ml_structuring_element` retains one admitted origin-containing convex
  polygon for repeated offset, inset, opening, and closing calls.

Region union, intersection, difference, and symmetric difference call the one
Haskell overlay and grouped-publication path. Point location is exact. A single
valuation call returns Euler characteristic, exact reduced area as
`numerator/denominator`, and certified conventional-perimeter bounds. Morphology
returns a new region plus the existing operation/work receipt.

Every status-returning call accepts an optional `ml_obstruction` distinguishing
pointer/count/buffer failures from region layout, validation, overlay,
publication, valuation, projection, morphology, and runtime failures. Handles
remain valid until explicitly freed; every operation publishes a fresh output
handle. `ml_runtime_initialize` is idempotent and process-lifetime.

Batch related mesh edits into one `insert_many` call. Author a whole region with
one component list; there is deliberately no mutable builder or per-vertex FFI
surface.

## Python

```python
from fractions import Fraction
from moonlight_triangulation import Moonlight, PolygonComponent

moonlight = Moonlight()
left = moonlight.region([
    PolygonComponent(((0, 0), (2, 0), (2, 2), (0, 2)))
])
right = moonlight.region([
    PolygonComponent(((1, 0), (3, 0), (3, 2), (1, 2)))
])
intersection = left.intersection(right)
assert intersection.valuations.area == Fraction(2, 1)
```

## TypeScript

```typescript
import { Moonlight, RegionLocation } from "@moonlight/triangulation";

const moonlight = new Moonlight();
const region = moonlight.region([
  { outer: [[0, 0], [2, 0], [2, 2], [0, 2]] },
]);
console.log(region.valuations().area, region.locate([1, 1]) === RegionLocation.Interior);
```

## Rust

```rust
use moonlight_triangulation::{Moonlight, PolygonComponent};

let moonlight = Moonlight::initialize()?;
let region = moonlight.region(&[PolygonComponent {
    outer: vec![[0.0, 0.0], [2.0, 0.0], [2.0, 2.0], [0.0, 2.0]],
    holes: vec![],
}])?;
println!("{:?}", region.valuations()?.area);
```
