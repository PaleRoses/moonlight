import koffi from "koffi";

const STATUS_OK = 0;
const STATUS_BUFFER_TOO_SMALL = 3;
const ABI_VERSION = 2;

export type Point = readonly [x: number, y: number];
export type Triangle = readonly [first: number, second: number, third: number];

export interface PolygonComponent {
  readonly outer: readonly Point[];
  readonly holes?: readonly (readonly Point[])[];
}

export enum RegionLocation {
  Exterior = 0,
  Boundary = 1,
  Interior = 2,
}

export interface ExactRational {
  readonly numerator: bigint;
  readonly denominator: bigint;
}

export interface RegionValuations {
  readonly eulerCharacteristic: bigint;
  readonly area: ExactRational;
  readonly perimeterBounds: readonly [lower: number, upper: number];
}

export enum MinkowskiOperation {
  Addition = 0,
  Erosion = 1,
  Opening = 2,
  Closing = 3,
}

export interface MinkowskiReceipt {
  readonly operation: MinkowskiOperation;
  readonly inputComponents: bigint;
  readonly convexPieces: bigint;
  readonly generatedPieces: bigint;
  readonly generatedConvolutionEdges: bigint;
  readonly overlayPasses: bigint;
  readonly exactCrossings: bigint;
  readonly outputCells: bigint;
  readonly exactCoordinateBitGrowth: bigint;
}

type NativeHandle = object;
type NativeInteger = number | bigint;
type HandleOutput = Array<NativeHandle | null>;

interface NativeObstruction {
  code?: number;
  coordinate_error?: number;
  input_index?: NativeInteger;
  first_index?: NativeInteger;
  second_index?: NativeInteger;
  first_value?: number;
  second_value?: number;
  point_x?: number;
  point_y?: number;
  message?: string | readonly number[];
}

interface NativeMinkowskiReceipt {
  operation?: number;
  reserved?: number;
  input_components?: NativeInteger;
  convex_pieces?: NativeInteger;
  generated_pieces?: NativeInteger;
  generated_convolution_edges?: NativeInteger;
  overlay_passes?: NativeInteger;
  exact_crossings?: NativeInteger;
  output_cells?: NativeInteger;
  exact_coordinate_bit_growth?: NativeInteger;
}

type BuildOperation = (coordinates: Float64Array, pointCount: number, output: HandleOutput, obstruction: NativeObstruction) => number;
type InsertOperation = (mesh: NativeHandle, coordinates: Float64Array, pointCount: number, output: HandleOutput, obstruction: NativeObstruction) => number;
type BinaryOperation = (left: NativeHandle, right: NativeHandle, output: HandleOutput, obstruction: NativeObstruction) => number;
type CountOperation = (handle: NativeHandle, output: NativeInteger[], obstruction: NativeObstruction) => number;
type MorphologyOperation = (
  first: NativeHandle,
  second: NativeHandle,
  output: HandleOutput,
  receipt: NativeMinkowskiReceipt,
  obstruction: NativeObstruction,
) => number;

interface NativeApi {
  readonly abiVersion: () => number;
  readonly runtimeInitialize: () => number;
  readonly delaunay: BuildOperation;
  readonly insertMany: InsertOperation;
  readonly meshSiteUnion: BinaryOperation;
  readonly meshSiteIntersection: BinaryOperation;
  readonly meshSiteDifference: BinaryOperation;
  readonly meshSiteSymmetricDifference: BinaryOperation;
  readonly vertexCount: CountOperation;
  readonly triangleCount: CountOperation;
  readonly copyVertices: (mesh: NativeHandle, coordinates: Float64Array, capacity: number, written: NativeInteger[], obstruction: NativeObstruction) => number;
  readonly copyTriangles: (mesh: NativeHandle, triangles: Uint32Array, capacity: number, written: NativeInteger[], obstruction: NativeObstruction) => number;
  readonly meshFree: (mesh: NativeHandle) => void;
  readonly regionCreate: (
    coordinates: Float64Array,
    pointCount: number,
    loopPointCounts: BigUint64Array,
    loopCount: number,
    componentLoopCounts: BigUint64Array,
    componentCount: number,
    output: HandleOutput,
    obstruction: NativeObstruction,
  ) => number;
  readonly regionCounts: (
    region: NativeHandle,
    componentCount: NativeInteger[],
    loopCount: NativeInteger[],
    pointCount: NativeInteger[],
    obstruction: NativeObstruction,
  ) => number;
  readonly regionCopy: (
    region: NativeHandle,
    coordinates: Float64Array,
    pointCapacity: number,
    loopPointOffsets: BigUint64Array,
    loopOffsetCapacity: number,
    componentLoopOffsets: BigUint64Array,
    componentOffsetCapacity: number,
    obstruction: NativeObstruction,
  ) => number;
  readonly regionUnion: BinaryOperation;
  readonly regionIntersection: BinaryOperation;
  readonly regionDifference: BinaryOperation;
  readonly regionSymmetricDifference: BinaryOperation;
  readonly regionLocate: (region: NativeHandle, x: number, y: number, location: number[], obstruction: NativeObstruction) => number;
  readonly regionMeasure: (
    region: NativeHandle,
    euler: NativeInteger[],
    areaRatio: Buffer,
    areaCapacity: number,
    areaBytes: NativeInteger[],
    perimeterLower: number[],
    perimeterUpper: number[],
    obstruction: NativeObstruction,
  ) => number;
  readonly regionFree: (region: NativeHandle) => void;
  readonly structuringElementCreate: BuildOperation;
  readonly structuringElementFree: (element: NativeHandle) => void;
  readonly regionMinkowskiSum: MorphologyOperation;
  readonly regionOffset: MorphologyOperation;
  readonly regionInset: MorphologyOperation;
  readonly regionOpen: MorphologyOperation;
  readonly regionClose: MorphologyOperation;
}

interface FinalizerState {
  readonly free: (handle: NativeHandle) => void;
  readonly handle: NativeHandle;
}

const handleFinalizer = new FinalizationRegistry<FinalizerState>(({ free, handle }) => free(handle));

export class MoonlightError extends Error {
  readonly status: number;
  readonly code: number;
  readonly coordinateError: number;
  readonly inputIndex: bigint;
  readonly firstIndex: bigint;
  readonly secondIndex: bigint;
  readonly firstValue: number;
  readonly secondValue: number;
  readonly point: Point;

  constructor(status: number, obstruction: NativeObstruction) {
    super(obstructionMessage(obstruction));
    this.name = "MoonlightError";
    this.status = status;
    this.code = obstruction.code ?? 0;
    this.coordinateError = obstruction.coordinate_error ?? 0;
    this.inputIndex = toBigInt(obstruction.input_index);
    this.firstIndex = toBigInt(obstruction.first_index);
    this.secondIndex = toBigInt(obstruction.second_index);
    this.firstValue = obstruction.first_value ?? 0;
    this.secondValue = obstruction.second_value ?? 0;
    this.point = [obstruction.point_x ?? 0, obstruction.point_y ?? 0];
  }
}

export class Moonlight {
  readonly #native: NativeApi;

  constructor(libraryPath = process.env.MOONLIGHT_TRIANGULATION_LIBRARY) {
    if (libraryPath === undefined || libraryPath.length === 0) {
      throw new Error("set MOONLIGHT_TRIANGULATION_LIBRARY or pass libraryPath");
    }
    const native = createNativeApi(libraryPath);
    const initializationStatus = native.runtimeInitialize();
    if (initializationStatus !== STATUS_OK) {
      throw new Error(`Moonlight runtime initialization failed with status ${initializationStatus}`);
    }
    const abiVersion = native.abiVersion();
    if (abiVersion !== ABI_VERSION) {
      throw new Error(`unsupported Moonlight ABI version ${abiVersion}`);
    }
    this.#native = native;
  }

  delaunay(points: readonly Point[]): Mesh {
    const coordinates = flattenPoints(points);
    return new Mesh(
      this.#native,
      produceHandle((output, obstruction) =>
        this.#native.delaunay(coordinates, points.length, output, obstruction),
      ),
    );
  }

  region(components: readonly PolygonComponent[]): Region {
    const loops = components.flatMap(({ outer, holes = [] }) => [outer, ...holes]);
    const loopPointCounts = BigUint64Array.from(loops.map((loop) => BigInt(loop.length)));
    const componentLoopCounts = BigUint64Array.from(
      components.map(({ holes = [] }) => BigInt(holes.length + 1)),
    );
    const coordinates = flattenLoops(loops);
    return new Region(
      this.#native,
      produceHandle((output, obstruction) =>
        this.#native.regionCreate(
          coordinates,
          coordinates.length / 2,
          loopPointCounts,
          loops.length,
          componentLoopCounts,
          components.length,
          output,
          obstruction,
        ),
      ),
    );
  }

  structuringElement(points: readonly Point[]): StructuringElement {
    const coordinates = flattenPoints(points);
    return new StructuringElement(
      this.#native,
      produceHandle((output, obstruction) =>
        this.#native.structuringElementCreate(coordinates, points.length, output, obstruction),
      ),
    );
  }
}

abstract class OwnedHandle {
  readonly #native: NativeApi;
  readonly #free: (handle: NativeHandle) => void;
  readonly #kind: string;
  #handle: NativeHandle | null;

  protected constructor(
    native: NativeApi,
    handle: NativeHandle,
    free: (handle: NativeHandle) => void,
    kind: string,
  ) {
    this.#native = native;
    this.#free = free;
    this.#kind = kind;
    this.#handle = handle;
    handleFinalizer.register(this, { free, handle }, this);
  }

  close(): void {
    const handle = this.#handle;
    if (handle !== null) {
      handleFinalizer.unregister(this);
      this.#free(handle);
      this.#handle = null;
    }
  }

  native(): NativeApi {
    return this.#native;
  }

  nativeHandle(): NativeHandle {
    if (this.#handle === null) {
      throw new Error(`${this.#kind} is closed`);
    }
    return this.#handle;
  }

  requireSameRuntime(other: OwnedHandle): void {
    if (this.#native !== other.#native) {
      throw new Error("both values must belong to the same Moonlight runtime");
    }
  }
}

export class Mesh extends OwnedHandle {
  constructor(native: NativeApi, handle: NativeHandle) {
    super(native, handle, native.meshFree, "mesh");
  }

  vertexCount(): number {
    return this.count(this.native().vertexCount);
  }

  triangleCount(): number {
    return this.count(this.native().triangleCount);
  }

  vertices(): readonly Point[] {
    const count = this.vertexCount();
    const coordinates = new Float64Array(count * 2);
    const written: NativeInteger[] = [0];
    const obstruction: NativeObstruction = {};
    const status = this.native().copyVertices(this.nativeHandle(), coordinates, count, written, obstruction);
    checkStatus(status, obstruction);
    return Array.from({ length: toSafeNumber(written[0]) }, (_unused, index): Point => [
      coordinates[index * 2] ?? 0,
      coordinates[index * 2 + 1] ?? 0,
    ]);
  }

  triangles(): readonly Triangle[] {
    const count = this.triangleCount();
    const triangles = new Uint32Array(count * 3);
    const written: NativeInteger[] = [0];
    const obstruction: NativeObstruction = {};
    const status = this.native().copyTriangles(this.nativeHandle(), triangles, count, written, obstruction);
    checkStatus(status, obstruction);
    return Array.from({ length: toSafeNumber(written[0]) }, (_unused, index): Triangle => [
      triangles[index * 3] ?? 0,
      triangles[index * 3 + 1] ?? 0,
      triangles[index * 3 + 2] ?? 0,
    ]);
  }

  insertMany(points: readonly Point[]): Mesh {
    const coordinates = flattenPoints(points);
    return new Mesh(
      this.native(),
      produceHandle((output, obstruction) =>
        this.native().insertMany(this.nativeHandle(), coordinates, points.length, output, obstruction),
      ),
    );
  }

  siteUnion(other: Mesh): Mesh {
    return this.binary(other, this.native().meshSiteUnion);
  }

  siteIntersection(other: Mesh): Mesh {
    return this.binary(other, this.native().meshSiteIntersection);
  }

  siteDifference(other: Mesh): Mesh {
    return this.binary(other, this.native().meshSiteDifference);
  }

  siteSymmetricDifference(other: Mesh): Mesh {
    return this.binary(other, this.native().meshSiteSymmetricDifference);
  }

  private binary(other: Mesh, operation: BinaryOperation): Mesh {
    this.requireSameRuntime(other);
    return new Mesh(
      this.native(),
      produceHandle((output, obstruction) =>
        operation(this.nativeHandle(), other.nativeHandle(), output, obstruction),
      ),
    );
  }

  private count(operation: CountOperation): number {
    const output: NativeInteger[] = [0];
    const obstruction: NativeObstruction = {};
    const status = operation(this.nativeHandle(), output, obstruction);
    checkStatus(status, obstruction);
    return toSafeNumber(output[0]);
  }
}

export class Region extends OwnedHandle {
  constructor(native: NativeApi, handle: NativeHandle) {
    super(native, handle, native.regionFree, "region");
  }

  components(): readonly PolygonComponent[] {
    const [componentCount, loopCount, pointCount] = this.counts();
    const coordinates = new Float64Array(pointCount * 2);
    const loopPointOffsets = new BigUint64Array(loopCount + 1);
    const componentLoopOffsets = new BigUint64Array(componentCount + 1);
    const obstruction: NativeObstruction = {};
    const status = this.native().regionCopy(
      this.nativeHandle(),
      coordinates,
      pointCount,
      loopPointOffsets,
      loopCount + 1,
      componentLoopOffsets,
      componentCount + 1,
      obstruction,
    );
    checkStatus(status, obstruction);
    const points = Array.from({ length: pointCount }, (_unused, index): Point => [
      coordinates[index * 2] ?? 0,
      coordinates[index * 2 + 1] ?? 0,
    ]);
    const loops = Array.from({ length: loopCount }, (_unused, index) =>
      points.slice(toSafeNumber(loopPointOffsets[index]), toSafeNumber(loopPointOffsets[index + 1])),
    );
    return Array.from({ length: componentCount }, (_unused, index): PolygonComponent => {
      const start = toSafeNumber(componentLoopOffsets[index]);
      const end = toSafeNumber(componentLoopOffsets[index + 1]);
      const outer = loops[start];
      if (outer === undefined || start >= end) {
        throw new Error("Moonlight returned a component without an outer loop");
      }
      return { outer, holes: loops.slice(start + 1, end) };
    });
  }

  valuations(): RegionValuations {
    return this.measureWithCapacity(128);
  }

  locate([x, y]: Point): RegionLocation {
    const location = [0];
    const obstruction: NativeObstruction = {};
    const status = this.native().regionLocate(this.nativeHandle(), x, y, location, obstruction);
    checkStatus(status, obstruction);
    return locationCode(location[0]);
  }

  union(other: Region): Region {
    return this.binary(other, this.native().regionUnion);
  }

  intersection(other: Region): Region {
    return this.binary(other, this.native().regionIntersection);
  }

  difference(other: Region): Region {
    return this.binary(other, this.native().regionDifference);
  }

  symmetricDifference(other: Region): Region {
    return this.binary(other, this.native().regionSymmetricDifference);
  }

  minkowskiSum(other: Region): readonly [Region, MinkowskiReceipt] {
    this.requireSameRuntime(other);
    return this.morph(this.native().regionMinkowskiSum, this.nativeHandle(), other.nativeHandle());
  }

  offset(element: StructuringElement): readonly [Region, MinkowskiReceipt] {
    return this.withElement(element, this.native().regionOffset);
  }

  inset(element: StructuringElement): readonly [Region, MinkowskiReceipt] {
    return this.withElement(element, this.native().regionInset);
  }

  open(element: StructuringElement): readonly [Region, MinkowskiReceipt] {
    return this.withElement(element, this.native().regionOpen);
  }

  closeWith(element: StructuringElement): readonly [Region, MinkowskiReceipt] {
    return this.withElement(element, this.native().regionClose);
  }

  private binary(other: Region, operation: BinaryOperation): Region {
    this.requireSameRuntime(other);
    return new Region(
      this.native(),
      produceHandle((output, obstruction) =>
        operation(this.nativeHandle(), other.nativeHandle(), output, obstruction),
      ),
    );
  }

  private withElement(
    element: StructuringElement,
    operation: MorphologyOperation,
  ): readonly [Region, MinkowskiReceipt] {
    this.requireSameRuntime(element);
    return this.morph(operation, element.nativeHandle(), this.nativeHandle());
  }

  private morph(
    operation: MorphologyOperation,
    first: NativeHandle,
    second: NativeHandle,
  ): readonly [Region, MinkowskiReceipt] {
    const [handle, receipt] = produceMorphology((output, nativeReceipt, obstruction) =>
      operation(first, second, output, nativeReceipt, obstruction),
    );
    return [new Region(this.native(), handle), receipt];
  }

  private counts(): readonly [number, number, number] {
    const componentCount: NativeInteger[] = [0];
    const loopCount: NativeInteger[] = [0];
    const pointCount: NativeInteger[] = [0];
    const obstruction: NativeObstruction = {};
    const status = this.native().regionCounts(
      this.nativeHandle(),
      componentCount,
      loopCount,
      pointCount,
      obstruction,
    );
    checkStatus(status, obstruction);
    return [toSafeNumber(componentCount[0]), toSafeNumber(loopCount[0]), toSafeNumber(pointCount[0])];
  }

  private measureWithCapacity(capacity: number): RegionValuations {
    const euler: NativeInteger[] = [0];
    const areaRatio = Buffer.alloc(capacity);
    const areaBytes: NativeInteger[] = [0];
    const perimeterLower = [0];
    const perimeterUpper = [0];
    const obstruction: NativeObstruction = {};
    const status = this.native().regionMeasure(
      this.nativeHandle(),
      euler,
      areaRatio,
      capacity,
      areaBytes,
      perimeterLower,
      perimeterUpper,
      obstruction,
    );
    if (status === STATUS_BUFFER_TOO_SMALL && obstruction.code === 102) {
      return this.measureWithCapacity(toSafeNumber(areaBytes[0]) + 1);
    }
    checkStatus(status, obstruction);
    const ratio = areaRatio.subarray(0, toSafeNumber(areaBytes[0])).toString("ascii");
    const [numerator, denominator, remainder] = ratio.split("/");
    if (numerator === undefined || denominator === undefined || remainder !== undefined) {
      throw new Error("Moonlight returned a malformed exact-area ratio");
    }
    return {
      eulerCharacteristic: toBigInt(euler[0]),
      area: { numerator: BigInt(numerator), denominator: BigInt(denominator) },
      perimeterBounds: [perimeterLower[0] ?? 0, perimeterUpper[0] ?? 0],
    };
  }
}

export class StructuringElement extends OwnedHandle {
  constructor(native: NativeApi, handle: NativeHandle) {
    super(native, handle, native.structuringElementFree, "structuring element");
  }
}

function createNativeApi(libraryPath: string): NativeApi {
  const library = koffi.load(libraryPath);
  const mesh = koffi.opaque();
  const region = koffi.opaque();
  const structuringElement = koffi.opaque();
  const meshPointer = koffi.pointer(mesh);
  const regionPointer = koffi.pointer(region);
  const structuringElementPointer = koffi.pointer(structuringElement);
  const meshOutput = koffi.out(koffi.pointer(mesh, 2));
  const regionOutput = koffi.out(koffi.pointer(region, 2));
  const structuringElementOutput = koffi.out(koffi.pointer(structuringElement, 2));
  const obstruction = koffi.struct({
    code: "uint32_t",
    coordinate_error: "uint32_t",
    input_index: "uint64_t",
    first_index: "uint64_t",
    second_index: "uint64_t",
    first_value: "double",
    second_value: "double",
    point_x: "double",
    point_y: "double",
    message: koffi.array("char", 256),
  });
  const receipt = koffi.struct({
    operation: "uint32_t",
    reserved: "uint32_t",
    input_components: "uint64_t",
    convex_pieces: "uint64_t",
    generated_pieces: "uint64_t",
    generated_convolution_edges: "uint64_t",
    overlay_passes: "uint64_t",
    exact_crossings: "uint64_t",
    output_cells: "uint64_t",
    exact_coordinate_bit_growth: "uint64_t",
  });
  const obstructionOutput = koffi.out(koffi.pointer(obstruction));
  const receiptOutput = koffi.out(koffi.pointer(receipt));
  const sizeOutput = koffi.out(koffi.pointer("size_t"));
  const uint32Output = koffi.out(koffi.pointer("uint32_t"));
  const int64Output = koffi.out(koffi.pointer("int64_t"));
  const doubleOutput = koffi.out(koffi.pointer("double"));
  const sizeArray = koffi.pointer("size_t");
  return {
    abiVersion: library.func("ml_abi_version", "uint32_t", []),
    runtimeInitialize: library.func("ml_runtime_initialize", "uint32_t", []),
    delaunay: library.func("ml_delaunay_f64", "uint32_t", [koffi.pointer("double"), "size_t", meshOutput, obstructionOutput]),
    insertMany: library.func("ml_mesh_insert_many_f64", "uint32_t", [meshPointer, koffi.pointer("double"), "size_t", meshOutput, obstructionOutput]),
    meshSiteUnion: library.func("ml_mesh_site_union", "uint32_t", [meshPointer, meshPointer, meshOutput, obstructionOutput]),
    meshSiteIntersection: library.func("ml_mesh_site_intersection", "uint32_t", [meshPointer, meshPointer, meshOutput, obstructionOutput]),
    meshSiteDifference: library.func("ml_mesh_site_difference", "uint32_t", [meshPointer, meshPointer, meshOutput, obstructionOutput]),
    meshSiteSymmetricDifference: library.func("ml_mesh_site_symmetric_difference", "uint32_t", [meshPointer, meshPointer, meshOutput, obstructionOutput]),
    vertexCount: library.func("ml_mesh_vertex_count", "uint32_t", [meshPointer, sizeOutput, obstructionOutput]),
    triangleCount: library.func("ml_mesh_triangle_count", "uint32_t", [meshPointer, sizeOutput, obstructionOutput]),
    copyVertices: library.func("ml_mesh_copy_vertices_f64", "uint32_t", [meshPointer, koffi.out(koffi.pointer("double")), "size_t", sizeOutput, obstructionOutput]),
    copyTriangles: library.func("ml_mesh_copy_triangles_u32", "uint32_t", [meshPointer, koffi.out(koffi.pointer("uint32_t")), "size_t", sizeOutput, obstructionOutput]),
    meshFree: library.func("ml_mesh_free", "void", [meshPointer]),
    regionCreate: library.func("ml_region_create_f64", "uint32_t", [koffi.pointer("double"), "size_t", sizeArray, "size_t", sizeArray, "size_t", regionOutput, obstructionOutput]),
    regionCounts: library.func("ml_region_counts", "uint32_t", [regionPointer, sizeOutput, sizeOutput, sizeOutput, obstructionOutput]),
    regionCopy: library.func("ml_region_copy_f64", "uint32_t", [regionPointer, koffi.out(koffi.pointer("double")), "size_t", koffi.out(sizeArray), "size_t", koffi.out(sizeArray), "size_t", obstructionOutput]),
    regionUnion: library.func("ml_region_union", "uint32_t", [regionPointer, regionPointer, regionOutput, obstructionOutput]),
    regionIntersection: library.func("ml_region_intersection", "uint32_t", [regionPointer, regionPointer, regionOutput, obstructionOutput]),
    regionDifference: library.func("ml_region_difference", "uint32_t", [regionPointer, regionPointer, regionOutput, obstructionOutput]),
    regionSymmetricDifference: library.func("ml_region_symmetric_difference", "uint32_t", [regionPointer, regionPointer, regionOutput, obstructionOutput]),
    regionLocate: library.func("ml_region_locate_point_f64", "uint32_t", [regionPointer, "double", "double", uint32Output, obstructionOutput]),
    regionMeasure: library.func("ml_region_measure", "uint32_t", [regionPointer, int64Output, koffi.out(koffi.pointer("char")), "size_t", sizeOutput, doubleOutput, doubleOutput, obstructionOutput]),
    regionFree: library.func("ml_region_free", "void", [regionPointer]),
    structuringElementCreate: library.func("ml_structuring_element_create_f64", "uint32_t", [koffi.pointer("double"), "size_t", structuringElementOutput, obstructionOutput]),
    structuringElementFree: library.func("ml_structuring_element_free", "void", [structuringElementPointer]),
    regionMinkowskiSum: library.func("ml_region_minkowski_sum", "uint32_t", [regionPointer, regionPointer, regionOutput, receiptOutput, obstructionOutput]),
    regionOffset: library.func("ml_region_offset", "uint32_t", [structuringElementPointer, regionPointer, regionOutput, receiptOutput, obstructionOutput]),
    regionInset: library.func("ml_region_inset", "uint32_t", [structuringElementPointer, regionPointer, regionOutput, receiptOutput, obstructionOutput]),
    regionOpen: library.func("ml_region_open", "uint32_t", [structuringElementPointer, regionPointer, regionOutput, receiptOutput, obstructionOutput]),
    regionClose: library.func("ml_region_close", "uint32_t", [structuringElementPointer, regionPointer, regionOutput, receiptOutput, obstructionOutput]),
  };
}

function flattenPoints(points: readonly Point[]): Float64Array {
  return Float64Array.from(points.flatMap(([x, y]) => [x, y]));
}

function flattenLoops(loops: readonly (readonly Point[])[]): Float64Array {
  return Float64Array.from(loops.flatMap((loop) => loop.flatMap(([x, y]) => [x, y])));
}

function produceHandle(
  operation: (output: HandleOutput, obstruction: NativeObstruction) => number,
): NativeHandle {
  const output: HandleOutput = [null];
  const obstruction: NativeObstruction = {};
  checkStatus(operation(output, obstruction), obstruction);
  return requiredHandle(output[0]);
}

function produceMorphology(
  operation: (
    output: HandleOutput,
    receipt: NativeMinkowskiReceipt,
    obstruction: NativeObstruction,
  ) => number,
): readonly [NativeHandle, MinkowskiReceipt] {
  const output: HandleOutput = [null];
  const receipt: NativeMinkowskiReceipt = {};
  const obstruction: NativeObstruction = {};
  checkStatus(operation(output, receipt, obstruction), obstruction);
  return [requiredHandle(output[0]), morphologyReceipt(receipt)];
}

function morphologyReceipt(receipt: NativeMinkowskiReceipt): MinkowskiReceipt {
  return {
    operation: operationCode(receipt.operation),
    inputComponents: toBigInt(receipt.input_components),
    convexPieces: toBigInt(receipt.convex_pieces),
    generatedPieces: toBigInt(receipt.generated_pieces),
    generatedConvolutionEdges: toBigInt(receipt.generated_convolution_edges),
    overlayPasses: toBigInt(receipt.overlay_passes),
    exactCrossings: toBigInt(receipt.exact_crossings),
    outputCells: toBigInt(receipt.output_cells),
    exactCoordinateBitGrowth: toBigInt(receipt.exact_coordinate_bit_growth),
  };
}

function operationCode(value: number | undefined): MinkowskiOperation {
  if (value === undefined || value < MinkowskiOperation.Addition || value > MinkowskiOperation.Closing) {
    throw new Error(`Moonlight returned an unknown morphology operation ${value ?? "missing"}`);
  }
  return value;
}

function locationCode(value: number | undefined): RegionLocation {
  if (value === undefined || value < RegionLocation.Exterior || value > RegionLocation.Interior) {
    throw new Error(`Moonlight returned an unknown region location ${value ?? "missing"}`);
  }
  return value;
}

function checkStatus(status: number, obstruction: NativeObstruction): void {
  if (status !== STATUS_OK) {
    throw new MoonlightError(status, obstruction);
  }
}

function requiredHandle(handle: NativeHandle | null | undefined): NativeHandle {
  if (handle === null || handle === undefined) {
    throw new Error("Moonlight returned success without a handle");
  }
  return handle;
}

function toBigInt(value: NativeInteger | undefined): bigint {
  return value === undefined ? 0n : BigInt(value);
}

function toSafeNumber(value: NativeInteger | undefined): number {
  const exact = toBigInt(value);
  const projected = Number(exact);
  if (!Number.isSafeInteger(projected)) {
    throw new Error(`Moonlight count ${exact} exceeds JavaScript's safe integer range`);
  }
  return projected;
}

function obstructionMessage(obstruction: NativeObstruction): string {
  const message = obstruction.message;
  if (message === undefined) {
    return `Moonlight ABI failure ${obstruction.code ?? 0}`;
  }
  if (typeof message === "string") {
    return message.split("\0", 1)[0] ?? "";
  }
  const terminator = message.indexOf(0);
  const length = terminator === -1 ? message.length : terminator;
  return new TextDecoder().decode(Uint8Array.from(message.slice(0, length)));
}
