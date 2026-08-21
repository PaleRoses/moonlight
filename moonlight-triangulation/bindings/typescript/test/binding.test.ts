import assert from "node:assert/strict";
import test from "node:test";

import {
  MinkowskiOperation,
  Moonlight,
  MoonlightError,
  RegionLocation,
} from "../src/index.js";

test("immutable site-set algebra and dense projection", () => {
  const engine = new Moonlight();
  const left = engine.delaunay([[0, 0], [2, 0], [0, 2], [2, 2]]);
  const right = engine.delaunay([[2, 0], [4, 0], [2, 2], [4, 2]]);
  const union = left.siteUnion(right);
  const intersection = left.siteIntersection(right);
  const difference = left.siteDifference(right);
  const symmetric = left.siteSymmetricDifference(right);
  const extended = left.insertMany([[1, 1], [3, 1]]);

  assert.equal(left.vertexCount(), 4);
  assert.equal(union.vertexCount(), 6);
  assert.equal(intersection.vertexCount(), 2);
  assert.equal(difference.vertexCount(), 2);
  assert.equal(symmetric.vertexCount(), 4);
  assert.equal(extended.vertexCount(), 6);
  assert.equal(left.vertices().length, left.vertexCount());
  assert.equal(left.triangles().length, left.triangleCount());

  left.close();
  right.close();
  union.close();
  intersection.close();
  difference.close();
  symmetric.close();
  extended.close();
});

test("exact region Boolean, valuation, and morphology", () => {
  const engine = new Moonlight();
  const left = engine.region([{ outer: [[0, 0], [2, 0], [2, 2], [0, 2]] }]);
  const right = engine.region([{ outer: [[1, 0], [3, 0], [3, 2], [1, 2]] }]);
  const intersection = left.intersection(right);
  const symmetric = left.symmetricDifference(right);
  const kernel = engine.structuringElement([[-0.5, -0.5], [0.5, -0.5], [0.5, 0.5], [-0.5, 0.5]]);
  const [offset, receipt] = left.offset(kernel);

  assert.equal(left.components().length, 1);
  assert.equal(left.components()[0]?.outer.length, 4);
  assert.deepEqual(intersection.valuations().area, { numerator: 2n, denominator: 1n });
  assert.equal(symmetric.valuations().eulerCharacteristic, 2n);
  assert.equal(left.locate([1, 1]), RegionLocation.Interior);
  assert.equal(left.locate([0, 1]), RegionLocation.Boundary);
  assert.equal(left.locate([3, 1]), RegionLocation.Exterior);
  assert.deepEqual(offset.valuations().area, { numerator: 9n, denominator: 1n });
  assert.equal(receipt.operation, MinkowskiOperation.Addition);
  assert(receipt.generatedPieces >= 1n);

  left.close();
  right.close();
  intersection.close();
  symmetric.close();
  kernel.close();
  offset.close();
});

test("invalid coordinate preserves typed witness", () => {
  const engine = new Moonlight();
  assert.throws(
    () => engine.delaunay([[0, 0], [Number.NaN, 1], [1, 0]]),
    (failure: unknown) =>
      failure instanceof MoonlightError &&
      failure.status === 4 &&
      failure.code === 1 &&
      failure.coordinateError === 1 &&
      failure.inputIndex === 1n,
  );
});
