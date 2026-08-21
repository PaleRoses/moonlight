from __future__ import annotations

import math
import os
import unittest
from fractions import Fraction
from typing import ClassVar

from moonlight_triangulation import (
    MinkowskiOperation,
    Moonlight,
    MoonlightError,
    PolygonComponent,
    RegionLocation,
)


class MoonlightBindingTest(unittest.TestCase):
    engine: ClassVar[Moonlight]

    @classmethod
    def setUpClass(cls) -> None:
        cls.engine = Moonlight(os.environ["MOONLIGHT_TRIANGULATION_LIBRARY"])

    def test_immutable_site_set_algebra_and_dense_projection(self) -> None:
        left = self.engine.delaunay([(0, 0), (2, 0), (0, 2), (2, 2)])
        right = self.engine.delaunay([(2, 0), (4, 0), (2, 2), (4, 2)])
        union = left.site_union(right)
        intersection = left.site_intersection(right)
        difference = left.site_difference(right)
        symmetric = left.site_symmetric_difference(right)
        extended = left.insert_many([(1, 1), (3, 1)])
        self.addCleanup(left.close)
        self.addCleanup(right.close)
        self.addCleanup(union.close)
        self.addCleanup(intersection.close)
        self.addCleanup(difference.close)
        self.addCleanup(symmetric.close)
        self.addCleanup(extended.close)

        self.assertEqual(left.vertex_count, 4)
        self.assertEqual(union.vertex_count, 6)
        self.assertEqual(intersection.vertex_count, 2)
        self.assertEqual(difference.vertex_count, 2)
        self.assertEqual(symmetric.vertex_count, 4)
        self.assertEqual(extended.vertex_count, 6)
        self.assertEqual(len(left.vertices), left.vertex_count)
        self.assertEqual(len(left.triangles), left.triangle_count)
        self.assertTrue(all(max(triangle) < left.vertex_count for triangle in left.triangles))

    def test_exact_region_boolean_valuation_and_morphology(self) -> None:
        left = self.engine.region([PolygonComponent(((0, 0), (2, 0), (2, 2), (0, 2)))])
        right = self.engine.region([PolygonComponent(((1, 0), (3, 0), (3, 2), (1, 2)))])
        intersection = left.intersection(right)
        difference = left.difference(right)
        symmetric = left.symmetric_difference(right)
        kernel = self.engine.structuring_element(((-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)))
        offset, receipt = left.offset(kernel)
        self.addCleanup(left.close)
        self.addCleanup(right.close)
        self.addCleanup(intersection.close)
        self.addCleanup(difference.close)
        self.addCleanup(symmetric.close)
        self.addCleanup(kernel.close)
        self.addCleanup(offset.close)

        self.assertEqual(len(left.components), 1)
        self.assertEqual(len(left.components[0].outer), 4)
        self.assertEqual(intersection.valuations.area, Fraction(2, 1))
        self.assertEqual(difference.valuations.euler_characteristic, 1)
        self.assertEqual(symmetric.valuations.euler_characteristic, 2)
        self.assertEqual(left.locate((1, 1)), RegionLocation.INTERIOR)
        self.assertEqual(left.locate((0, 1)), RegionLocation.BOUNDARY)
        self.assertEqual(left.locate((3, 1)), RegionLocation.EXTERIOR)
        self.assertEqual(offset.valuations.area, Fraction(9, 1))
        self.assertEqual(receipt.operation, MinkowskiOperation.ADDITION)
        self.assertGreaterEqual(receipt.generated_pieces, 1)

    def test_invalid_coordinate_preserves_typed_witness(self) -> None:
        with self.assertRaises(MoonlightError) as raised:
            self.engine.delaunay([(0, 0), (math.nan, 1), (1, 0)])
        self.assertEqual(raised.exception.status, 4)
        self.assertEqual(raised.exception.code, 1)
        self.assertEqual(raised.exception.coordinate_error, 1)
        self.assertEqual(raised.exception.input_index, 1)


if __name__ == "__main__":
    unittest.main()
