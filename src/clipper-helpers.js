// clipper-lib glue for 2D polygon offsetting and booleans. Rings cross the
// F#/JS boundary as arrays of {X, Y} points in millimetres.
import ClipperLib from 'clipper-lib';

const SCALE = 1000; // micron precision

const toC = (rings) =>
  rings.map((r) => r.map((p) => ({ X: Math.round(p.X * SCALE), Y: Math.round(p.Y * SCALE) })));
const fromC = (paths) => paths.map((p) => p.map((pt) => ({ X: pt.X / SCALE, Y: pt.Y / SCALE })));

function unionSelf(cPaths) {
  const c = new ClipperLib.Clipper();
  c.AddPaths(cPaths, ClipperLib.PolyType.ptSubject, true);
  const out = new ClipperLib.Paths();
  c.Execute(
    ClipperLib.ClipType.ctUnion,
    out,
    ClipperLib.PolyFillType.pftNonZero,
    ClipperLib.PolyFillType.pftNonZero
  );
  return out;
}

/** Union the rings, then offset outward by delta mm with round joins. */
export function offsetUnion(rings, delta) {
  if (rings.length === 0) return [];
  const co = new ClipperLib.ClipperOffset(2, 0.02 * SCALE); // 0.02mm arc tolerance
  co.AddPaths(unionSelf(toC(rings)), ClipperLib.JoinType.jtRound, ClipperLib.EndType.etClosedPolygon);
  const out = new ClipperLib.Paths();
  co.Execute(out, delta * SCALE);
  return fromC(out);
}

/** Boolean op between two ring sets: op is "union" or "difference". */
export function combine(a, b, op) {
  const c = new ClipperLib.Clipper();
  c.AddPaths(toC(a), ClipperLib.PolyType.ptSubject, true);
  c.AddPaths(toC(b), ClipperLib.PolyType.ptClip, true);
  const out = new ClipperLib.Paths();
  c.Execute(
    op === 'union' ? ClipperLib.ClipType.ctUnion : ClipperLib.ClipType.ctDifference,
    out,
    ClipperLib.PolyFillType.pftNonZero,
    ClipperLib.PolyFillType.pftNonZero
  );
  return fromC(out);
}
