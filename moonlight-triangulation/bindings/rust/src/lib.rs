use std::error::Error;
use std::ffi::{c_char, c_double, c_uint};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;

const STATUS_OK: u32 = 0;
const STATUS_BUFFER_TOO_SMALL: u32 = 3;
const ABI_VERSION: u32 = 2;

#[repr(C)]
struct NativeMesh {
    _private: [u8; 0],
}

#[repr(C)]
struct NativeRegion {
    _private: [u8; 0],
}

#[repr(C)]
struct NativeStructuringElement {
    _private: [u8; 0],
}

#[repr(C)]
struct NativeObstruction {
    code: u32,
    coordinate_error: u32,
    input_index: u64,
    first_index: u64,
    second_index: u64,
    first_value: f64,
    second_value: f64,
    point_x: f64,
    point_y: f64,
    message: [c_char; 256],
}

impl Default for NativeObstruction {
    fn default() -> Self {
        Self {
            code: 0,
            coordinate_error: 0,
            input_index: u64::MAX,
            first_index: 0,
            second_index: 0,
            first_value: 0.0,
            second_value: 0.0,
            point_x: 0.0,
            point_y: 0.0,
            message: [0; 256],
        }
    }
}

#[repr(C)]
#[derive(Default)]
struct NativeMinkowskiReceipt {
    operation: u32,
    reserved: u32,
    input_components: u64,
    convex_pieces: u64,
    generated_pieces: u64,
    generated_convolution_edges: u64,
    overlay_passes: u64,
    exact_crossings: u64,
    output_cells: u64,
    exact_coordinate_bit_growth: u64,
}

#[link(name = "moonlight-triangulation-c")]
unsafe extern "C" {
    fn ml_abi_version() -> c_uint;
    fn ml_runtime_initialize() -> c_uint;
    fn ml_delaunay_f64(
        coordinates: *const c_double,
        point_count: usize,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_insert_many_f64(
        mesh: *const NativeMesh,
        coordinates: *const c_double,
        point_count: usize,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_site_union(
        left: *const NativeMesh,
        right: *const NativeMesh,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_site_intersection(
        left: *const NativeMesh,
        right: *const NativeMesh,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_site_difference(
        left: *const NativeMesh,
        right: *const NativeMesh,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_site_symmetric_difference(
        left: *const NativeMesh,
        right: *const NativeMesh,
        result: *mut *mut NativeMesh,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_vertex_count(
        mesh: *const NativeMesh,
        count: *mut usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_triangle_count(
        mesh: *const NativeMesh,
        count: *mut usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_copy_vertices_f64(
        mesh: *const NativeMesh,
        coordinates: *mut c_double,
        point_capacity: usize,
        points_written: *mut usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_copy_triangles_u32(
        mesh: *const NativeMesh,
        triangles: *mut u32,
        triangle_capacity: usize,
        triangles_written: *mut usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_mesh_free(mesh: *mut NativeMesh);

    fn ml_region_create_f64(
        coordinates: *const c_double,
        point_count: usize,
        loop_point_counts: *const usize,
        loop_count: usize,
        component_loop_counts: *const usize,
        component_count: usize,
        result: *mut *mut NativeRegion,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_counts(
        region: *const NativeRegion,
        component_count: *mut usize,
        loop_count: *mut usize,
        point_count: *mut usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_copy_f64(
        region: *const NativeRegion,
        coordinates: *mut c_double,
        point_capacity: usize,
        loop_point_offsets: *mut usize,
        loop_offset_capacity: usize,
        component_loop_offsets: *mut usize,
        component_offset_capacity: usize,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_union(
        left: *const NativeRegion,
        right: *const NativeRegion,
        result: *mut *mut NativeRegion,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_intersection(
        left: *const NativeRegion,
        right: *const NativeRegion,
        result: *mut *mut NativeRegion,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_difference(
        left: *const NativeRegion,
        right: *const NativeRegion,
        result: *mut *mut NativeRegion,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_symmetric_difference(
        left: *const NativeRegion,
        right: *const NativeRegion,
        result: *mut *mut NativeRegion,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_locate_point_f64(
        region: *const NativeRegion,
        x: c_double,
        y: c_double,
        location: *mut u32,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_measure(
        region: *const NativeRegion,
        euler_characteristic: *mut i64,
        area_ratio_utf8: *mut c_char,
        area_capacity: usize,
        area_bytes_written: *mut usize,
        perimeter_lower: *mut c_double,
        perimeter_upper: *mut c_double,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_free(region: *mut NativeRegion);

    fn ml_structuring_element_create_f64(
        coordinates: *const c_double,
        point_count: usize,
        result: *mut *mut NativeStructuringElement,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_structuring_element_free(element: *mut NativeStructuringElement);
    fn ml_region_minkowski_sum(
        left: *const NativeRegion,
        right: *const NativeRegion,
        result: *mut *mut NativeRegion,
        receipt: *mut NativeMinkowskiReceipt,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_offset(
        element: *const NativeStructuringElement,
        region: *const NativeRegion,
        result: *mut *mut NativeRegion,
        receipt: *mut NativeMinkowskiReceipt,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_inset(
        element: *const NativeStructuringElement,
        region: *const NativeRegion,
        result: *mut *mut NativeRegion,
        receipt: *mut NativeMinkowskiReceipt,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_open(
        element: *const NativeStructuringElement,
        region: *const NativeRegion,
        result: *mut *mut NativeRegion,
        receipt: *mut NativeMinkowskiReceipt,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
    fn ml_region_close(
        element: *const NativeStructuringElement,
        region: *const NativeRegion,
        result: *mut *mut NativeRegion,
        receipt: *mut NativeMinkowskiReceipt,
        obstruction: *mut NativeObstruction,
    ) -> c_uint;
}

type BinaryMeshOperation = unsafe extern "C" fn(
    *const NativeMesh,
    *const NativeMesh,
    *mut *mut NativeMesh,
    *mut NativeObstruction,
) -> c_uint;

type BinaryRegionOperation = unsafe extern "C" fn(
    *const NativeRegion,
    *const NativeRegion,
    *mut *mut NativeRegion,
    *mut NativeObstruction,
) -> c_uint;

type ElementRegionOperation = unsafe extern "C" fn(
    *const NativeStructuringElement,
    *const NativeRegion,
    *mut *mut NativeRegion,
    *mut NativeMinkowskiReceipt,
    *mut NativeObstruction,
) -> c_uint;

#[derive(Debug, Clone, PartialEq)]
pub struct MoonlightError {
    pub status: u32,
    pub code: u32,
    pub coordinate_error: u32,
    pub input_index: u64,
    pub first_index: u64,
    pub second_index: u64,
    pub first_value: f64,
    pub second_value: f64,
    pub point: [f64; 2],
    pub message: String,
}

impl Display for MoonlightError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.message)
    }
}

impl Error for MoonlightError {}

#[derive(Debug, Clone, PartialEq)]
pub struct PolygonComponent {
    pub outer: Vec<[f64; 2]>,
    pub holes: Vec<Vec<[f64; 2]>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionLocation {
    Exterior,
    Boundary,
    Interior,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExactRational {
    pub numerator: String,
    pub denominator: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegionValuations {
    pub euler_characteristic: i64,
    pub area: ExactRational,
    pub perimeter_bounds: [f64; 2],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MinkowskiOperation {
    Addition,
    Erosion,
    Opening,
    Closing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MinkowskiReceipt {
    pub operation: MinkowskiOperation,
    pub input_components: u64,
    pub convex_pieces: u64,
    pub generated_pieces: u64,
    pub generated_convolution_edges: u64,
    pub overlay_passes: u64,
    pub exact_crossings: u64,
    pub output_cells: u64,
    pub exact_coordinate_bit_growth: u64,
}

pub struct Moonlight;

impl Moonlight {
    pub fn initialize() -> Result<Self, MoonlightError> {
        let status = unsafe { ml_runtime_initialize() };
        if status != STATUS_OK {
            return Err(synthetic_error(
                status,
                "Moonlight runtime initialization failed",
                0,
                0,
            ));
        }
        let version = unsafe { ml_abi_version() };
        if version != ABI_VERSION {
            return Err(synthetic_error(
                5,
                format!("unsupported Moonlight ABI version {version}"),
                u64::from(version),
                u64::from(ABI_VERSION),
            ));
        }
        Ok(Self)
    }

    pub fn delaunay(&self, points: &[[f64; 2]]) -> Result<Mesh, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_delaunay_f64(coordinates.as_ptr(), points.len(), output, obstruction)
        })
        .map(Mesh::from_handle)
    }

    pub fn region(&self, components: &[PolygonComponent]) -> Result<Region, MoonlightError> {
        let loop_point_counts = components
            .iter()
            .flat_map(|component| std::iter::once(&component.outer).chain(component.holes.iter()))
            .map(Vec::len)
            .collect::<Vec<_>>();
        let component_loop_counts = components
            .iter()
            .map(|component| component.holes.len() + 1)
            .collect::<Vec<_>>();
        let coordinates = components
            .iter()
            .flat_map(|component| std::iter::once(&component.outer).chain(component.holes.iter()))
            .flat_map(|loop_points| loop_points.iter())
            .flat_map(|[x, y]| [*x, *y])
            .collect::<Vec<_>>();
        create_handle(|output, obstruction| unsafe {
            ml_region_create_f64(
                coordinates.as_ptr(),
                coordinates.len() / 2,
                loop_point_counts.as_ptr(),
                loop_point_counts.len(),
                component_loop_counts.as_ptr(),
                components.len(),
                output,
                obstruction,
            )
        })
        .map(Region::from_handle)
    }

    pub fn structuring_element(
        &self,
        points: &[[f64; 2]],
    ) -> Result<StructuringElement, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_structuring_element_create_f64(
                coordinates.as_ptr(),
                points.len(),
                output,
                obstruction,
            )
        })
        .map(StructuringElement::from_handle)
    }
}

pub struct Mesh {
    handle: NonNull<NativeMesh>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl Mesh {
    fn from_handle(handle: NonNull<NativeMesh>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }

    pub fn vertex_count(&self) -> Result<usize, MoonlightError> {
        self.count(ml_mesh_vertex_count)
    }

    pub fn triangle_count(&self) -> Result<usize, MoonlightError> {
        self.count(ml_mesh_triangle_count)
    }

    pub fn vertices(&self) -> Result<Vec<[f64; 2]>, MoonlightError> {
        let count = self.vertex_count()?;
        let mut coordinates = vec![0.0; count * 2];
        let mut written = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_mesh_copy_vertices_f64(
                self.handle.as_ptr(),
                coordinates.as_mut_ptr(),
                count,
                &mut written,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        coordinates.truncate(written * 2);
        coordinate_pairs(&coordinates)
    }

    pub fn triangles(&self) -> Result<Vec<[u32; 3]>, MoonlightError> {
        let count = self.triangle_count()?;
        let mut triangles = vec![0; count * 3];
        let mut written = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_mesh_copy_triangles_u32(
                self.handle.as_ptr(),
                triangles.as_mut_ptr(),
                count,
                &mut written,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        triangles.truncate(written * 3);
        triangles
            .chunks_exact(3)
            .map(|triangle| {
                Ok([
                    *triangle.first().ok_or_else(projection_shape_error)?,
                    *triangle.get(1).ok_or_else(projection_shape_error)?,
                    *triangle.get(2).ok_or_else(projection_shape_error)?,
                ])
            })
            .collect()
    }

    pub fn insert_many(&self, points: &[[f64; 2]]) -> Result<Self, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_mesh_insert_many_f64(
                self.handle.as_ptr(),
                coordinates.as_ptr(),
                points.len(),
                output,
                obstruction,
            )
        })
        .map(Self::from_handle)
    }

    pub fn site_union(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_mesh_site_union)
    }

    pub fn site_intersection(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_mesh_site_intersection)
    }

    pub fn site_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_mesh_site_difference)
    }

    pub fn site_symmetric_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_mesh_site_symmetric_difference)
    }

    fn binary(&self, other: &Self, operation: BinaryMeshOperation) -> Result<Self, MoonlightError> {
        create_handle(|output, obstruction| unsafe {
            operation(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
        .map(Self::from_handle)
    }

    fn count(
        &self,
        operation: unsafe extern "C" fn(
            *const NativeMesh,
            *mut usize,
            *mut NativeObstruction,
        ) -> c_uint,
    ) -> Result<usize, MoonlightError> {
        let mut count = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe { operation(self.handle.as_ptr(), &mut count, &mut obstruction) };
        status_result(status, obstruction)?;
        Ok(count)
    }
}

impl Drop for Mesh {
    fn drop(&mut self) {
        unsafe { ml_mesh_free(self.handle.as_ptr()) }
    }
}

pub struct Region {
    handle: NonNull<NativeRegion>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl Region {
    fn from_handle(handle: NonNull<NativeRegion>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }

    pub fn components(&self) -> Result<Vec<PolygonComponent>, MoonlightError> {
        let (component_count, loop_count, point_count) = self.counts()?;
        let mut coordinates = vec![0.0; point_count * 2];
        let mut loop_point_offsets = vec![0; loop_count + 1];
        let mut component_loop_offsets = vec![0; component_count + 1];
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_copy_f64(
                self.handle.as_ptr(),
                coordinates.as_mut_ptr(),
                point_count,
                loop_point_offsets.as_mut_ptr(),
                loop_point_offsets.len(),
                component_loop_offsets.as_mut_ptr(),
                component_loop_offsets.len(),
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        let points = coordinate_pairs(&coordinates)?;
        component_loop_offsets
            .windows(2)
            .map(|window| {
                let (start, end) = offset_window(window)?;
                if start >= end {
                    return Err(projection_shape_error());
                }
                let outer = projected_loop(&points, &loop_point_offsets, start)?;
                let holes = (start + 1..end)
                    .map(|index| projected_loop(&points, &loop_point_offsets, index))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(PolygonComponent { outer, holes })
            })
            .collect()
    }

    pub fn valuations(&self) -> Result<RegionValuations, MoonlightError> {
        self.measure_with_capacity(128)
    }

    pub fn locate(&self, [x, y]: [f64; 2]) -> Result<RegionLocation, MoonlightError> {
        let mut location = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_locate_point_f64(self.handle.as_ptr(), x, y, &mut location, &mut obstruction)
        };
        status_result(status, obstruction)?;
        region_location(location)
    }

    pub fn union(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_region_union)
    }

    pub fn intersection(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_region_intersection)
    }

    pub fn difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_region_difference)
    }

    pub fn symmetric_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(other, ml_region_symmetric_difference)
    }

    pub fn minkowski_sum(&self, other: &Self) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        produce_morphology(|output, receipt, obstruction| unsafe {
            ml_region_minkowski_sum(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                receipt,
                obstruction,
            )
        })
    }

    pub fn offset(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(element, ml_region_offset)
    }

    pub fn inset(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(element, ml_region_inset)
    }

    pub fn open(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(element, ml_region_open)
    }

    pub fn close_with(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(element, ml_region_close)
    }

    fn binary(
        &self,
        other: &Self,
        operation: BinaryRegionOperation,
    ) -> Result<Self, MoonlightError> {
        create_handle(|output, obstruction| unsafe {
            operation(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
        .map(Self::from_handle)
    }

    fn with_element(
        &self,
        element: &StructuringElement,
        operation: ElementRegionOperation,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        produce_morphology(|output, receipt, obstruction| unsafe {
            operation(
                element.handle.as_ptr(),
                self.handle.as_ptr(),
                output,
                receipt,
                obstruction,
            )
        })
    }

    fn counts(&self) -> Result<(usize, usize, usize), MoonlightError> {
        let mut component_count = 0;
        let mut loop_count = 0;
        let mut point_count = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_counts(
                self.handle.as_ptr(),
                &mut component_count,
                &mut loop_count,
                &mut point_count,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        Ok((component_count, loop_count, point_count))
    }

    fn measure_with_capacity(&self, capacity: usize) -> Result<RegionValuations, MoonlightError> {
        let mut euler_characteristic = 0;
        let mut area_ratio = vec![0; capacity];
        let mut area_bytes_written = 0;
        let mut perimeter_lower = 0.0;
        let mut perimeter_upper = 0.0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_measure(
                self.handle.as_ptr(),
                &mut euler_characteristic,
                area_ratio.as_mut_ptr(),
                capacity,
                &mut area_bytes_written,
                &mut perimeter_lower,
                &mut perimeter_upper,
                &mut obstruction,
            )
        };
        if status == STATUS_BUFFER_TOO_SMALL && obstruction.code == 102 {
            return self.measure_with_capacity(area_bytes_written + 1);
        }
        status_result(status, obstruction)?;
        let area_bytes = area_ratio
            .get(..area_bytes_written)
            .ok_or_else(projection_shape_error)?
            .iter()
            .map(|byte| *byte as u8)
            .collect::<Vec<_>>();
        let area_text = String::from_utf8(area_bytes)
            .map_err(|failure| synthetic_error(5, failure.to_string(), 0, 0))?;
        let (numerator, denominator) = area_text
            .split_once('/')
            .ok_or_else(|| synthetic_error(5, "malformed exact-area ratio", 0, 0))?;
        Ok(RegionValuations {
            euler_characteristic,
            area: ExactRational {
                numerator: numerator.to_owned(),
                denominator: denominator.to_owned(),
            },
            perimeter_bounds: [perimeter_lower, perimeter_upper],
        })
    }
}

impl Drop for Region {
    fn drop(&mut self) {
        unsafe { ml_region_free(self.handle.as_ptr()) }
    }
}

pub struct StructuringElement {
    handle: NonNull<NativeStructuringElement>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl StructuringElement {
    fn from_handle(handle: NonNull<NativeStructuringElement>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }
}

impl Drop for StructuringElement {
    fn drop(&mut self) {
        unsafe { ml_structuring_element_free(self.handle.as_ptr()) }
    }
}

fn flatten_points(points: &[[f64; 2]]) -> Vec<f64> {
    points.iter().flat_map(|[x, y]| [*x, *y]).collect()
}

fn coordinate_pairs(coordinates: &[f64]) -> Result<Vec<[f64; 2]>, MoonlightError> {
    coordinates
        .chunks_exact(2)
        .map(|point| {
            Ok([
                *point.first().ok_or_else(projection_shape_error)?,
                *point.get(1).ok_or_else(projection_shape_error)?,
            ])
        })
        .collect()
}

fn offset_window(window: &[usize]) -> Result<(usize, usize), MoonlightError> {
    Ok((
        *window.first().ok_or_else(projection_shape_error)?,
        *window.get(1).ok_or_else(projection_shape_error)?,
    ))
}

fn projected_loop(
    points: &[[f64; 2]],
    offsets: &[usize],
    index: usize,
) -> Result<Vec<[f64; 2]>, MoonlightError> {
    let start = *offsets.get(index).ok_or_else(projection_shape_error)?;
    let end = *offsets.get(index + 1).ok_or_else(projection_shape_error)?;
    points
        .get(start..end)
        .map(<[_]>::to_vec)
        .ok_or_else(projection_shape_error)
}

fn create_handle<Native>(
    operation: impl FnOnce(*mut *mut Native, *mut NativeObstruction) -> u32,
) -> Result<NonNull<Native>, MoonlightError> {
    let mut output = std::ptr::null_mut();
    let mut obstruction = NativeObstruction::default();
    let status = operation(&mut output, &mut obstruction);
    status_result(status, obstruction)?;
    NonNull::new(output).ok_or_else(success_without_handle_error)
}

fn produce_morphology(
    operation: impl FnOnce(
        *mut *mut NativeRegion,
        *mut NativeMinkowskiReceipt,
        *mut NativeObstruction,
    ) -> u32,
) -> Result<(Region, MinkowskiReceipt), MoonlightError> {
    let mut output = std::ptr::null_mut();
    let mut receipt = NativeMinkowskiReceipt::default();
    let mut obstruction = NativeObstruction::default();
    let status = operation(&mut output, &mut receipt, &mut obstruction);
    status_result(status, obstruction)?;
    let handle = NonNull::new(output).ok_or_else(success_without_handle_error)?;
    Ok((Region::from_handle(handle), minkowski_receipt(receipt)?))
}

fn minkowski_receipt(receipt: NativeMinkowskiReceipt) -> Result<MinkowskiReceipt, MoonlightError> {
    Ok(MinkowskiReceipt {
        operation: minkowski_operation(receipt.operation)?,
        input_components: receipt.input_components,
        convex_pieces: receipt.convex_pieces,
        generated_pieces: receipt.generated_pieces,
        generated_convolution_edges: receipt.generated_convolution_edges,
        overlay_passes: receipt.overlay_passes,
        exact_crossings: receipt.exact_crossings,
        output_cells: receipt.output_cells,
        exact_coordinate_bit_growth: receipt.exact_coordinate_bit_growth,
    })
}

fn minkowski_operation(code: u32) -> Result<MinkowskiOperation, MoonlightError> {
    match code {
        0 => Ok(MinkowskiOperation::Addition),
        1 => Ok(MinkowskiOperation::Erosion),
        2 => Ok(MinkowskiOperation::Opening),
        3 => Ok(MinkowskiOperation::Closing),
        _ => Err(synthetic_error(
            5,
            format!("unknown morphology operation {code}"),
            u64::from(code),
            0,
        )),
    }
}

fn region_location(code: u32) -> Result<RegionLocation, MoonlightError> {
    match code {
        0 => Ok(RegionLocation::Exterior),
        1 => Ok(RegionLocation::Boundary),
        2 => Ok(RegionLocation::Interior),
        _ => Err(synthetic_error(
            5,
            format!("unknown region location {code}"),
            u64::from(code),
            0,
        )),
    }
}

fn status_result(status: u32, obstruction: NativeObstruction) -> Result<(), MoonlightError> {
    if status == STATUS_OK {
        Ok(())
    } else {
        Err(MoonlightError::from_native(status, obstruction))
    }
}

impl MoonlightError {
    fn from_native(status: u32, obstruction: NativeObstruction) -> Self {
        let message_end = obstruction
            .message
            .iter()
            .position(|character| *character == 0)
            .unwrap_or(obstruction.message.len());
        let message_bytes = obstruction
            .message
            .get(..message_end)
            .unwrap_or_default()
            .iter()
            .map(|character| *character as u8)
            .collect::<Vec<_>>();
        Self {
            status,
            code: obstruction.code,
            coordinate_error: obstruction.coordinate_error,
            input_index: obstruction.input_index,
            first_index: obstruction.first_index,
            second_index: obstruction.second_index,
            first_value: obstruction.first_value,
            second_value: obstruction.second_value,
            point: [obstruction.point_x, obstruction.point_y],
            message: String::from_utf8_lossy(&message_bytes).into_owned(),
        }
    }
}

fn projection_shape_error() -> MoonlightError {
    synthetic_error(5, "Moonlight returned a malformed bulk projection", 0, 0)
}

fn success_without_handle_error() -> MoonlightError {
    synthetic_error(5, "Moonlight returned success without a handle", 0, 0)
}

fn synthetic_error(
    status: u32,
    message: impl Into<String>,
    first_index: u64,
    second_index: u64,
) -> MoonlightError {
    MoonlightError {
        status,
        code: 103,
        coordinate_error: 0,
        input_index: u64::MAX,
        first_index,
        second_index,
        first_value: 0.0,
        second_value: 0.0,
        point: [0.0, 0.0],
        message: message.into(),
    }
}
