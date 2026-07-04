module SvgExtrude.Clipper

open Fable.Core
open SvgExtrude.Types
open SvgExtrude.PathParser

// Bindings over clipper-helpers.js. Rings cross the boundary as arrays of
// {X, Y} objects, which is exactly Fable's representation of Pt.

/// Union the rings, then offset outward by delta mm (round joins).
[<Import("offsetUnion", "./clipper-helpers.js")>]
let offsetUnion (rings: Ring array) (delta: float) : Ring array = jsNative

/// Boolean op between two ring sets: "union" or "difference".
[<Import("combine", "./clipper-helpers.js")>]
let combine (a: Ring array) (b: Ring array) (op: string) : Ring array = jsNative

/// Classify a clipper result (mixed outers + holes) into fillable shapes.
let toShapes (rings: Ring array) : Shape list =
    let subs =
        rings
        |> Array.toList
        |> List.map (fun r -> { Points = r; Closed = true })
    fst (Rings.toShapes subs)
