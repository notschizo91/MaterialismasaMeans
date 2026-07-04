module SvgExtrude.TextShapes

open Fable.Core
open Fable.Core.JsInterop
open SvgExtrude.Types
open SvgExtrude.PathParser

// Bindings over text-helpers.js (opentype.js). Fonts are opaque JS objects.

[<Import("loadDefaultFont", "./text-helpers.js")>]
let loadDefaultFont () : JS.Promise<obj> = jsNative

[<Import("parseFontBuffer", "./text-helpers.js")>]
let parseFontBuffer (buffer: obj) : obj = jsNative

[<Import("fontName", "./text-helpers.js")>]
let fontName (font: obj) : string = jsNative

/// One opentype command list (mm units, y-down) per visible glyph.
[<Import("layoutText", "./text-helpers.js")>]
let layoutText (font: obj) (text: string) (sizeMm: float) (letterSpacingMm: float) (wordGapMul: float) : obj array array = jsNative

/// opentype.js path commands -> flattened subpaths (y-down mm coordinates).
let commandsToSubpaths (tol: float) (commands: obj array) : Subpath list =
    let subs = ResizeArray<Subpath>()
    let mutable pts = ResizeArray<Pt>()
    let mutable cur = { X = 0.0; Y = 0.0 }
    let flush closed =
        if pts.Count > 1 then subs.Add { Points = pts.ToArray(); Closed = closed }
        pts <- ResizeArray<Pt>()
    for cmd in commands do
        let t: string = cmd?``type``
        match t with
        | "M" ->
            flush false
            cur <- { X = cmd?x; Y = cmd?y }
            pts.Add cur
        | "L" ->
            if pts.Count = 0 then pts.Add cur
            cur <- { X = cmd?x; Y = cmd?y }
            pts.Add cur
        | "C" ->
            if pts.Count = 0 then pts.Add cur
            let c1 = { X = cmd?x1; Y = cmd?y1 }
            let c2 = { X = cmd?x2; Y = cmd?y2 }
            let p = { X = cmd?x; Y = cmd?y }
            flattenCubic tol 0 cur c1 c2 p pts
            cur <- p
        | "Q" ->
            if pts.Count = 0 then pts.Add cur
            let q = { X = cmd?x1; Y = cmd?y1 }
            let p = { X = cmd?x; Y = cmd?y }
            flattenQuad tol cur q p pts
            cur <- p
        | "Z" -> flush true
        | _ -> ()
    flush false
    List.ofSeq subs

/// One glyph's commands -> fillable shapes with holes (y-down mm).
/// Holes smaller than minHoleArea mm² are filled in (dropped).
let glyphShapes (tol: float) (minHoleArea: float) (commands: obj array) : Shape list =
    let shapes, _ = Rings.toShapes (commandsToSubpaths tol commands)
    shapes
    |> List.map (fun s ->
        { s with Holes = s.Holes |> List.filter (fun h -> abs (Rings.signedArea h) >= minHoleArea) })
