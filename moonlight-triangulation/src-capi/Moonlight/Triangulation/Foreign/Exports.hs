{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Moonlight.Triangulation.Foreign.Exports where

import Data.Int (Int64)
import Data.Word (Word32)
import Foreign.C.Types (CChar, CDouble (..), CSize (..), CUInt (..))
import Foreign.Ptr (Ptr)
import Moonlight.Triangulation.Foreign.ABI
  ( CMesh
  , CMinkowskiReceipt
  , CObstruction
  , CRegion
  , CStructuringElement
  )
import qualified Moonlight.Triangulation.Foreign.ABI as ABI

delaunayF64 = ABI.delaunayF64
meshInsertManyF64 = ABI.meshInsertManyF64
meshSiteUnion = ABI.meshSiteUnion
meshSiteIntersection = ABI.meshSiteIntersection
meshSiteDifference = ABI.meshSiteDifference
meshSiteSymmetricDifference = ABI.meshSiteSymmetricDifference
meshVertexCount = ABI.meshVertexCount
meshTriangleCount = ABI.meshTriangleCount
meshCopyVerticesF64 = ABI.meshCopyVerticesF64
meshCopyTrianglesU32 = ABI.meshCopyTrianglesU32
meshFree = ABI.meshFree
regionCreateF64 = ABI.regionCreateF64
regionCounts = ABI.regionCounts
regionCopyF64 = ABI.regionCopyF64
regionUnion = ABI.regionUnion
regionIntersection = ABI.regionIntersection
regionDifference = ABI.regionDifference
regionSymmetricDifference = ABI.regionSymmetricDifference
regionLocatePointF64 = ABI.regionLocatePointF64
regionMeasure = ABI.regionMeasure
regionFree = ABI.regionFree
structuringElementCreateF64 = ABI.structuringElementCreateF64
structuringElementFree = ABI.structuringElementFree
regionMinkowskiSum = ABI.regionMinkowskiSum
regionOffset = ABI.regionOffset
regionInset = ABI.regionInset
regionOpen = ABI.regionOpen
regionClose = ABI.regionClose

foreign export ccall "ml_delaunay_f64" delaunayF64 :: Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_insert_many_f64" meshInsertManyF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_site_union" meshSiteUnion :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_site_intersection" meshSiteIntersection :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_site_difference" meshSiteDifference :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_site_symmetric_difference" meshSiteSymmetricDifference :: Ptr CMesh -> Ptr CMesh -> Ptr (Ptr CMesh) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_vertex_count" meshVertexCount :: Ptr CMesh -> Ptr CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_triangle_count" meshTriangleCount :: Ptr CMesh -> Ptr CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_copy_vertices_f64" meshCopyVerticesF64 :: Ptr CMesh -> Ptr CDouble -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_copy_triangles_u32" meshCopyTrianglesU32 :: Ptr CMesh -> Ptr Word32 -> CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_mesh_free" meshFree :: Ptr CMesh -> IO ()
foreign export ccall "ml_region_create_f64" regionCreateF64 :: Ptr CDouble -> CSize -> Ptr CSize -> CSize -> Ptr CSize -> CSize -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_counts" regionCounts :: Ptr CRegion -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_copy_f64" regionCopyF64 :: Ptr CRegion -> Ptr CDouble -> CSize -> Ptr CSize -> CSize -> Ptr CSize -> CSize -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_union" regionUnion :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_intersection" regionIntersection :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_difference" regionDifference :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_symmetric_difference" regionSymmetricDifference :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_locate_point_f64" regionLocatePointF64 :: Ptr CRegion -> CDouble -> CDouble -> Ptr CUInt -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_measure" regionMeasure :: Ptr CRegion -> Ptr Int64 -> Ptr CChar -> CSize -> Ptr CSize -> Ptr CDouble -> Ptr CDouble -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_free" regionFree :: Ptr CRegion -> IO ()
foreign export ccall "ml_structuring_element_create_f64" structuringElementCreateF64 :: Ptr CDouble -> CSize -> Ptr (Ptr CStructuringElement) -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_structuring_element_free" structuringElementFree :: Ptr CStructuringElement -> IO ()
foreign export ccall "ml_region_minkowski_sum" regionMinkowskiSum :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_offset" regionOffset :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_inset" regionInset :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_open" regionOpen :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
foreign export ccall "ml_region_close" regionClose :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
