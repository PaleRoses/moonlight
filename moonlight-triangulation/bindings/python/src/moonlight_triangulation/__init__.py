from __future__ import annotations

import ctypes
import os
import weakref
from array import array
from collections.abc import Callable, Iterable, Sequence
from dataclasses import dataclass
from enum import IntEnum
from fractions import Fraction
from pathlib import Path
from typing import Final, Self


class _Obstruction(ctypes.Structure):
    _fields_ = [
        ("code", ctypes.c_uint32),
        ("coordinate_error", ctypes.c_uint32),
        ("input_index", ctypes.c_uint64),
        ("first_index", ctypes.c_uint64),
        ("second_index", ctypes.c_uint64),
        ("first_value", ctypes.c_double),
        ("second_value", ctypes.c_double),
        ("point_x", ctypes.c_double),
        ("point_y", ctypes.c_double),
        ("message", ctypes.c_char * 256),
    ]


class _NativeMinkowskiReceipt(ctypes.Structure):
    _fields_ = [
        ("operation", ctypes.c_uint32),
        ("reserved", ctypes.c_uint32),
        ("input_components", ctypes.c_uint64),
        ("convex_pieces", ctypes.c_uint64),
        ("generated_pieces", ctypes.c_uint64),
        ("generated_convolution_edges", ctypes.c_uint64),
        ("overlay_passes", ctypes.c_uint64),
        ("exact_crossings", ctypes.c_uint64),
        ("output_cells", ctypes.c_uint64),
        ("exact_coordinate_bit_growth", ctypes.c_uint64),
    ]


class MoonlightError(RuntimeError):
    def __init__(self, status: int, obstruction: _Obstruction) -> None:
        self.status = status
        self.code = obstruction.code
        self.coordinate_error = obstruction.coordinate_error
        self.input_index = obstruction.input_index
        self.first_index = obstruction.first_index
        self.second_index = obstruction.second_index
        self.first_value = obstruction.first_value
        self.second_value = obstruction.second_value
        self.point = (obstruction.point_x, obstruction.point_y)
        message = bytes(obstruction.message).split(b"\0", 1)[0].decode("utf-8", errors="replace")
        super().__init__(message or f"Moonlight ABI failure {status}:{self.code}")


class _NativeApi:
    _OK: Final = 0
    _ABI_VERSION: Final = 2

    def __init__(self, library_path: Path) -> None:
        library = ctypes.CDLL(str(library_path))
        handle = ctypes.c_void_p
        handle_output = ctypes.POINTER(handle)
        obstruction = ctypes.POINTER(_Obstruction)
        count_output = ctypes.POINTER(ctypes.c_size_t)
        receipt_output = ctypes.POINTER(_NativeMinkowskiReceipt)
        binary_operation = (handle, handle, handle_output, obstruction)
        morphology_operation = (handle, handle, handle_output, receipt_output, obstruction)

        _configure(library, "ml_abi_version", ())
        _configure(library, "ml_runtime_initialize", ())
        _configure(library, "ml_delaunay_f64", (ctypes.POINTER(ctypes.c_double), ctypes.c_size_t, handle_output, obstruction))
        _configure(library, "ml_mesh_insert_many_f64", (handle, ctypes.POINTER(ctypes.c_double), ctypes.c_size_t, handle_output, obstruction))
        _configure(library, "ml_mesh_site_union", binary_operation)
        _configure(library, "ml_mesh_site_intersection", binary_operation)
        _configure(library, "ml_mesh_site_difference", binary_operation)
        _configure(library, "ml_mesh_site_symmetric_difference", binary_operation)
        _configure(library, "ml_mesh_vertex_count", (handle, count_output, obstruction))
        _configure(library, "ml_mesh_triangle_count", (handle, count_output, obstruction))
        _configure(library, "ml_mesh_copy_vertices_f64", (handle, ctypes.POINTER(ctypes.c_double), ctypes.c_size_t, count_output, obstruction))
        _configure(library, "ml_mesh_copy_triangles_u32", (handle, ctypes.POINTER(ctypes.c_uint32), ctypes.c_size_t, count_output, obstruction))
        _configure(library, "ml_mesh_free", (handle,), None)

        _configure(
            library,
            "ml_region_create_f64",
            (
                ctypes.POINTER(ctypes.c_double),
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_size_t),
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_size_t),
                ctypes.c_size_t,
                handle_output,
                obstruction,
            ),
        )
        _configure(library, "ml_region_counts", (handle, count_output, count_output, count_output, obstruction))
        _configure(
            library,
            "ml_region_copy_f64",
            (
                handle,
                ctypes.POINTER(ctypes.c_double),
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_size_t),
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_size_t),
                ctypes.c_size_t,
                obstruction,
            ),
        )
        _configure(library, "ml_region_union", binary_operation)
        _configure(library, "ml_region_intersection", binary_operation)
        _configure(library, "ml_region_difference", binary_operation)
        _configure(library, "ml_region_symmetric_difference", binary_operation)
        _configure(library, "ml_region_locate_point_f64", (handle, ctypes.c_double, ctypes.c_double, ctypes.POINTER(ctypes.c_uint32), obstruction))
        _configure(
            library,
            "ml_region_measure",
            (
                handle,
                ctypes.POINTER(ctypes.c_int64),
                ctypes.POINTER(ctypes.c_char),
                ctypes.c_size_t,
                count_output,
                ctypes.POINTER(ctypes.c_double),
                ctypes.POINTER(ctypes.c_double),
                obstruction,
            ),
        )
        _configure(library, "ml_region_free", (handle,), None)
        _configure(library, "ml_structuring_element_create_f64", (ctypes.POINTER(ctypes.c_double), ctypes.c_size_t, handle_output, obstruction))
        _configure(library, "ml_structuring_element_free", (handle,), None)
        _configure(library, "ml_region_minkowski_sum", morphology_operation)
        _configure(library, "ml_region_offset", morphology_operation)
        _configure(library, "ml_region_inset", morphology_operation)
        _configure(library, "ml_region_open", morphology_operation)
        _configure(library, "ml_region_close", morphology_operation)

        status = int(library.ml_runtime_initialize())
        if status != self._OK:
            raise RuntimeError(f"Moonlight runtime initialization failed with status {status}")
        abi_version = int(library.ml_abi_version())
        if abi_version != self._ABI_VERSION:
            raise RuntimeError(f"unsupported Moonlight ABI version {abi_version}")
        self.library = library

    def check(self, status: int, obstruction: _Obstruction) -> None:
        if status != self._OK:
            raise MoonlightError(status, obstruction)


Point = tuple[float, float]
Triangle = tuple[int, int, int]
_BinaryNativeOperation = Callable[[ctypes.c_void_p, ctypes.c_void_p, object, object], int]
_MorphologyNativeOperation = Callable[[ctypes.c_void_p, ctypes.c_void_p, object, object, object], int]


@dataclass(frozen=True)
class PolygonComponent:
    outer: tuple[Point, ...]
    holes: tuple[tuple[Point, ...], ...] = ()


class RegionLocation(IntEnum):
    EXTERIOR = 0
    BOUNDARY = 1
    INTERIOR = 2


class MinkowskiOperation(IntEnum):
    ADDITION = 0
    EROSION = 1
    OPENING = 2
    CLOSING = 3


@dataclass(frozen=True)
class RegionValuations:
    euler_characteristic: int
    area: Fraction
    perimeter_bounds: tuple[float, float]


@dataclass(frozen=True)
class MinkowskiReceipt:
    operation: MinkowskiOperation
    input_components: int
    convex_pieces: int
    generated_pieces: int
    generated_convolution_edges: int
    overlay_passes: int
    exact_crossings: int
    output_cells: int
    exact_coordinate_bit_growth: int


class Moonlight:
    def __init__(self, library_path: str | os.PathLike[str] | None = None) -> None:
        configured_path = library_path or os.environ.get("MOONLIGHT_TRIANGULATION_LIBRARY")
        if configured_path is None:
            raise ValueError("set MOONLIGHT_TRIANGULATION_LIBRARY or pass library_path")
        self._native = _NativeApi(Path(configured_path))

    def delaunay(self, points: Sequence[Point]) -> Mesh:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: self._native.library.ml_delaunay_f64(
                pointer, len(points), output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def region(self, components: Sequence[PolygonComponent]) -> Region:
        loops, loop_counts, component_counts = _component_layout(components)
        coordinates, coordinate_pointer = _coordinate_buffer(
            point for loop in loops for point in loop
        )
        loop_buffer = _size_buffer(loop_counts)
        component_buffer = _size_buffer(component_counts)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: self._native.library.ml_region_create_f64(
                coordinate_pointer,
                sum(loop_counts),
                loop_buffer,
                len(loop_counts),
                component_buffer,
                len(component_counts),
                output,
                obstruction,
            ),
        )
        return Region(self._native, handle)

    def structuring_element(self, points: Sequence[Point]) -> StructuringElement:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: self._native.library.ml_structuring_element_create_f64(
                pointer, len(points), output, obstruction
            ),
        )
        return StructuringElement(self._native, handle)


class _OwnedHandle:
    __slots__ = ("_native", "_handle", "_finalizer", "_kind", "__weakref__")

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p, free: Callable[[ctypes.c_void_p], None], kind: str) -> None:
        self._native = native
        self._handle = handle
        self._kind = kind
        self._finalizer = weakref.finalize(self, free, handle)

    def close(self) -> None:
        self._finalizer()
        self._handle = ctypes.c_void_p()

    def __enter__(self) -> Self:
        return self

    def __exit__(self, _type: object, _value: object, _traceback: object) -> None:
        self.close()

    def _live_handle(self) -> ctypes.c_void_p:
        if not self._finalizer.alive:
            raise RuntimeError(f"{self._kind} is closed")
        return self._handle

    def _require_same_runtime(self, other: _OwnedHandle) -> None:
        if self._native is not other._native:
            raise ValueError("both values must belong to the same Moonlight runtime")


class Mesh(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_mesh_free, "mesh")

    @property
    def vertex_count(self) -> int:
        return self._count(self._native.library.ml_mesh_vertex_count)

    @property
    def triangle_count(self) -> int:
        return self._count(self._native.library.ml_mesh_triangle_count)

    @property
    def vertices(self) -> tuple[Point, ...]:
        count = self.vertex_count
        output = (ctypes.c_double * (count * 2))()
        written = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_mesh_copy_vertices_f64(
                self._live_handle(), output, count, ctypes.byref(written), ctypes.byref(obstruction)
            )
        )
        self._native.check(status, obstruction)
        return tuple((float(output[index * 2]), float(output[index * 2 + 1])) for index in range(written.value))

    @property
    def triangles(self) -> tuple[Triangle, ...]:
        count = self.triangle_count
        output = (ctypes.c_uint32 * (count * 3))()
        written = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_mesh_copy_triangles_u32(
                self._live_handle(), output, count, ctypes.byref(written), ctypes.byref(obstruction)
            )
        )
        self._native.check(status, obstruction)
        return tuple(
            (int(output[index * 3]), int(output[index * 3 + 1]), int(output[index * 3 + 2]))
            for index in range(written.value)
        )

    def insert_many(self, points: Sequence[Point]) -> Mesh:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: self._native.library.ml_mesh_insert_many_f64(
                self._live_handle(), pointer, len(points), output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def site_union(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_union)

    def site_intersection(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_intersection)

    def site_difference(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_difference)

    def site_symmetric_difference(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_symmetric_difference)

    def _binary(self, other: Mesh, operation: _BinaryNativeOperation) -> Mesh:
        self._require_same_runtime(other)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: operation(
                self._live_handle(), other._live_handle(), output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def _count(self, operation: Callable[[ctypes.c_void_p, object, object], int]) -> int:
        output = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(operation(self._live_handle(), ctypes.byref(output), ctypes.byref(obstruction)))
        self._native.check(status, obstruction)
        return int(output.value)


class Region(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_region_free, "region")

    @property
    def components(self) -> tuple[PolygonComponent, ...]:
        component_count, loop_count, point_count = self._counts()
        coordinates = (ctypes.c_double * (point_count * 2))()
        loop_offsets = (ctypes.c_size_t * (loop_count + 1))()
        component_offsets = (ctypes.c_size_t * (component_count + 1))()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_copy_f64(
                self._live_handle(),
                coordinates,
                point_count,
                loop_offsets,
                loop_count + 1,
                component_offsets,
                component_count + 1,
                ctypes.byref(obstruction),
            )
        )
        self._native.check(status, obstruction)
        points = tuple((float(coordinates[index * 2]), float(coordinates[index * 2 + 1])) for index in range(point_count))
        loops = tuple(
            points[int(loop_offsets[index]) : int(loop_offsets[index + 1])]
            for index in range(loop_count)
        )
        component_ranges = tuple(
            (int(component_offsets[index]), int(component_offsets[index + 1]))
            for index in range(component_count)
        )
        if any(start >= end for start, end in component_ranges):
            raise RuntimeError("Moonlight returned a component without an outer loop")
        return tuple(
            PolygonComponent(loops[start], loops[start + 1 : end])
            for start, end in component_ranges
        )

    @property
    def valuations(self) -> RegionValuations:
        return self._measure_with_capacity(128)

    def locate(self, point: Point) -> RegionLocation:
        output = ctypes.c_uint32()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_locate_point_f64(
                self._live_handle(), point[0], point[1], ctypes.byref(output), ctypes.byref(obstruction)
            )
        )
        self._native.check(status, obstruction)
        return RegionLocation(output.value)

    def union(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_union)

    def intersection(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_intersection)

    def difference(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_difference)

    def symmetric_difference(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_symmetric_difference)

    def minkowski_sum(self, other: Region) -> tuple[Region, MinkowskiReceipt]:
        self._require_same_runtime(other)
        return self._morph(
            lambda output, receipt, obstruction: self._native.library.ml_region_minkowski_sum(
                self._live_handle(), other._live_handle(), output, receipt, obstruction
            )
        )

    def offset(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_offset)

    def inset(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_inset)

    def open(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_open)

    def close_with(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_close)

    def _binary(self, other: Region, operation: _BinaryNativeOperation) -> Region:
        self._require_same_runtime(other)
        handle = _produce_handle(
            self._native,
            lambda output, obstruction: operation(
                self._live_handle(), other._live_handle(), output, obstruction
            ),
        )
        return Region(self._native, handle)

    def _with_element(
        self, element: StructuringElement, operation: _MorphologyNativeOperation
    ) -> tuple[Region, MinkowskiReceipt]:
        self._require_same_runtime(element)
        return self._morph(
            lambda output, receipt, obstruction: operation(
                element._live_handle(), self._live_handle(), output, receipt, obstruction
            )
        )

    def _morph(
        self,
        operation: Callable[[object, object, object], int],
    ) -> tuple[Region, MinkowskiReceipt]:
        handle, native_receipt = _produce_morphology(self._native, operation)
        return Region(self._native, handle), _receipt(native_receipt)

    def _counts(self) -> tuple[int, int, int]:
        component_count = ctypes.c_size_t()
        loop_count = ctypes.c_size_t()
        point_count = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_counts(
                self._live_handle(),
                ctypes.byref(component_count),
                ctypes.byref(loop_count),
                ctypes.byref(point_count),
                ctypes.byref(obstruction),
            )
        )
        self._native.check(status, obstruction)
        return int(component_count.value), int(loop_count.value), int(point_count.value)

    def _measure_with_capacity(self, capacity: int) -> RegionValuations:
        euler = ctypes.c_int64()
        area = ctypes.create_string_buffer(capacity)
        area_bytes = ctypes.c_size_t()
        lower = ctypes.c_double()
        upper = ctypes.c_double()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_measure(
                self._live_handle(),
                ctypes.byref(euler),
                area,
                capacity,
                ctypes.byref(area_bytes),
                ctypes.byref(lower),
                ctypes.byref(upper),
                ctypes.byref(obstruction),
            )
        )
        if status == 3 and obstruction.code == 102:
            return self._measure_with_capacity(int(area_bytes.value) + 1)
        self._native.check(status, obstruction)
        numerator, separator, denominator = area.value.decode("ascii").partition("/")
        if separator != "/":
            raise RuntimeError("Moonlight returned a malformed exact-area ratio")
        return RegionValuations(int(euler.value), Fraction(int(numerator), int(denominator)), (lower.value, upper.value))


class StructuringElement(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_structuring_element_free, "structuring element")


def _component_layout(
    components: Sequence[PolygonComponent],
) -> tuple[tuple[Sequence[Point], ...], tuple[int, ...], tuple[int, ...]]:
    loops = tuple(
        loop
        for component in components
        for loop in (component.outer, *component.holes)
    )
    loop_counts = tuple(len(loop) for loop in loops)
    component_counts = tuple(len(component.holes) + 1 for component in components)
    return loops, loop_counts, component_counts


def _coordinate_buffer(points: Iterable[Point]) -> tuple[array, object]:
    coordinates = array("d", (component for x, y in points for component in (x, y)))
    if coordinates.itemsize != ctypes.sizeof(ctypes.c_double):
        raise RuntimeError("Python's native double width does not match the Moonlight ABI")
    if not coordinates:
        return coordinates, None
    pointer = (ctypes.c_double * len(coordinates)).from_buffer(coordinates)
    return coordinates, pointer


def _size_buffer(values: Sequence[int]) -> object:
    return (ctypes.c_size_t * len(values))(*values)


def _produce_handle(
    native: _NativeApi,
    operation: Callable[[object, object], int],
) -> ctypes.c_void_p:
    output = ctypes.c_void_p()
    obstruction = _Obstruction()
    status = int(operation(ctypes.byref(output), ctypes.byref(obstruction)))
    native.check(status, obstruction)
    return _required_handle(output)


def _produce_morphology(
    native: _NativeApi,
    operation: Callable[[object, object, object], int],
) -> tuple[ctypes.c_void_p, _NativeMinkowskiReceipt]:
    output = ctypes.c_void_p()
    receipt = _NativeMinkowskiReceipt()
    obstruction = _Obstruction()
    status = int(operation(ctypes.byref(output), ctypes.byref(receipt), ctypes.byref(obstruction)))
    native.check(status, obstruction)
    return _required_handle(output), receipt


def _receipt(native: _NativeMinkowskiReceipt) -> MinkowskiReceipt:
    return MinkowskiReceipt(
        MinkowskiOperation(native.operation),
        int(native.input_components),
        int(native.convex_pieces),
        int(native.generated_pieces),
        int(native.generated_convolution_edges),
        int(native.overlay_passes),
        int(native.exact_crossings),
        int(native.output_cells),
        int(native.exact_coordinate_bit_growth),
    )


def _required_handle(handle: ctypes.c_void_p) -> ctypes.c_void_p:
    if not handle.value:
        raise RuntimeError("Moonlight returned success without a handle")
    return handle


def _configure(library: ctypes.CDLL, name: str, parameters: Sequence[object], result: object = ctypes.c_uint32) -> None:
    function = getattr(library, name)
    setattr(function, "argtypes", list(parameters))
    setattr(function, "restype", result)


__all__ = [
    "Mesh",
    "MinkowskiOperation",
    "MinkowskiReceipt",
    "Moonlight",
    "MoonlightError",
    "Point",
    "PolygonComponent",
    "Region",
    "RegionLocation",
    "RegionValuations",
    "StructuringElement",
    "Triangle",
]
