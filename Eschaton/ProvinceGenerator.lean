/-
  Province Generator - Generates provinces using Voronoi tessellation

  Uses Delaunay triangulation to create Voronoi cells that completely tile
  the map rectangle with no gaps.
-/

import Linalg
import Tincture
import Eschaton.Widget.ProvinceMap

open Linalg
open Tincture (Color)

namespace Eschaton

/-- Float modulo operation -/
private def fmod (a b : Float) : Float :=
  a - b * (a / b).floor

/-- Configuration for province generation -/
structure ProvinceGenConfig where
  /-- Number of provinces to generate -/
  numProvinces : Nat := 24
  /-- Random seed -/
  seed : Nat := 12345
  /-- Bounding box in normalized coordinates (0-1) -/
  bounds : AABB2D := AABB2D.fromMinMax Vec2.zero Vec2.one
  /-- Minimum distance between province centers (as fraction of bounds) -/
  minDistance : Float := 0.08
  /-- Margin from edges to avoid edge cases -/
  edgeMargin : Float := 0.02
  /-- Number of Lloyd relaxation iterations to apply to seed points -/
  lloydRelaxations : Nat := 0
deriving Inhabited

/-- Simple linear congruential generator for deterministic randomness -/
private structure LCG where
  state : UInt64
deriving Inhabited

private def LCG.new (seed : Nat) : LCG :=
  { state := seed.toUInt64 }

private def LCG.next (rng : LCG) : LCG × Float :=
  -- LCG parameters (same as glibc)
  let a : UInt64 := 1103515245
  let c : UInt64 := 12345
  let m : UInt64 := 2147483648  -- 2^31
  let newState := (a * rng.state + c) % m
  let value := newState.toFloat / m.toFloat
  ({ state := newState }, value)

private def LCG.nextInRange (rng : LCG) (min max : Float) : LCG × Float :=
  let (rng', t) := rng.next
  (rng', min + t * (max - min))

/-- Generate random seed points with minimum distance constraint (Poisson disk-like) -/
private def generateSeedPoints (config : ProvinceGenConfig) : Array Vec2 := Id.run do
  let mut rng := LCG.new config.seed
  let mut points : Array Vec2 := #[]

  let minX := config.bounds.min.x + config.edgeMargin
  let maxX := config.bounds.max.x - config.edgeMargin
  let minY := config.bounds.min.y + config.edgeMargin
  let maxY := config.bounds.max.y - config.edgeMargin

  let minDistSq := config.minDistance * config.minDistance

  -- Try to place the requested number of points
  let mut attempts := 0
  let maxAttempts := config.numProvinces * 100

  while points.size < config.numProvinces && attempts < maxAttempts do
    let (rng', x) := rng.nextInRange minX maxX
    rng := rng'
    let (rng', y) := rng.nextInRange minY maxY
    rng := rng'

    let candidate := Vec2.mk x y

    -- Check minimum distance to all existing points
    let mut tooClose := false
    for p in points do
      if candidate.distanceSquared p < minDistSq then
        tooClose := true
        break

    if !tooClose then
      points := points.push candidate

    attempts := attempts + 1

  return points

/-- Generate a palette of distinct colors -/
private def generateColorPalette (rng : LCG) (count : Nat) : Array Color := Id.run do
  let mut rng := rng
  let mut colors : Array Color := #[]

  -- Use golden ratio for hue distribution to get well-spaced colors
  let goldenRatio := 0.618033988749895
  let (rng', startHue) := rng.next
  rng := rng'

  for i in [:count] do
    let hue := fmod (startHue + i.toFloat * goldenRatio) 1.0

    -- Vary saturation and lightness slightly
    let (rng', satVar) := rng.nextInRange 0.4 0.7
    rng := rng'
    let (rng', lightVar) := rng.nextInRange 0.35 0.55
    rng := rng'

    -- Convert HSL to RGB
    let color := hslToRgb hue satVar lightVar
    colors := colors.push color

  return colors
where
  /-- Convert HSL to RGB -/
  hslToRgb (h s l : Float) : Color :=
    let c := (1.0 - Float.abs (2.0 * l - 1.0)) * s
    let x := c * (1.0 - Float.abs (fmod (h * 6.0) 2.0 - 1.0))
    let m := l - c / 2.0

    let (r', g', b') :=
      if h < 1.0/6.0 then (c, x, 0.0)
      else if h < 2.0/6.0 then (x, c, 0.0)
      else if h < 3.0/6.0 then (0.0, c, x)
      else if h < 4.0/6.0 then (0.0, x, c)
      else if h < 5.0/6.0 then (x, 0.0, c)
      else (c, 0.0, x)

    Color.rgb (r' + m) (g' + m) (b' + m)

/-- Default province names -/
private def defaultProvinceNames : Array String := #[
  "Aethoria", "Valdris", "Korheim", "Thessaly", "Nordmark",
  "Ashvale", "Brightmoor", "Coldwater", "Duskwood", "Easthollow",
  "Fernwick", "Goldcrest", "Highbridge", "Ironholt", "Jadepeak",
  "Kingsland", "Lakeshore", "Mistral", "Northwind", "Oakdale",
  "Pinegrove", "Queensreach", "Riverdale", "Silverton", "Thornfield",
  "Underwood", "Verdantia", "Westmarch", "Yarrowmere", "Zephyria",
  "Amber Coast", "Bluehaven", "Crystalford", "Dawnshire", "Evergreen",
  "Frostholm", "Graystone", "Harborview", "Ivywood", "Jotunheim"
]

/-- Generate provinces using Voronoi tessellation -/
def generateProvinces (config : ProvinceGenConfig)
    (names : Option (Array String) := none)
    (colors : Option (Array Color) := none) : Array Widget.Province := Id.run do

  -- Generate or use provided seed points
  let seedPoints := generateSeedPoints config

  let points :=
    if config.lloydRelaxations == 0 then
      some seedPoints
    else
      Voronoi.lloydRelaxation seedPoints config.bounds config.lloydRelaxations

  match points with
  | none => #[]
  | some sites =>
    -- Generate Voronoi diagram
    let voronoiResult := Voronoi.generate sites config.bounds

    match voronoiResult with
    | none =>
      -- Fallback: return empty array if triangulation fails
      #[]
    | some polygons =>
      let mut provinces : Array Widget.Province := #[]

      -- Get names
      let provinceNames := names.getD defaultProvinceNames

      -- Get colors
      let rng := LCG.new (config.seed + 1000)
      let provinceColors := colors.getD (generateColorPalette rng seedPoints.size)

      for i in [:polygons.size] do
        let polygon := polygons[i]!

        -- Skip degenerate polygons
        if polygon.vertices.size < 3 then continue

        let name := provinceNames[i % provinceNames.size]!
        let color := provinceColors[i % provinceColors.size]!

        -- Use Province.create to pre-tessellate geometry at load time
        let province := Widget.Province.create
          i name polygon color (Color.rgba 0.15 0.15 0.15 1.0)

        provinces := provinces.push province

      return provinces

/-- Quick province generation with default settings -/
def generateDefaultProvinces (numProvinces : Nat := 24) (seed : Nat := 42)
    (lloydRelaxations : Nat := 0) : Array Widget.Province :=
  -- Scale minDistance inversely with sqrt of province count to maintain good spacing
  let scaleFactor := Float.sqrt (24.0 / numProvinces.toFloat)
  generateProvinces {
    numProvinces := numProvinces
    seed := seed
    bounds := AABB2D.fromMinMax Vec2.zero Vec2.one
    minDistance := 0.12 * scaleFactor
    edgeMargin := 0.03
    lloydRelaxations := lloydRelaxations
  }

end Eschaton
