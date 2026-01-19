/-
  Galaxy Hit Testing
  Inverse transform and hit testing for star selection.
-/
import Eschaton.Widget.Galaxy.State

namespace Eschaton.Widget.Galaxy

/-- Transform parameters for converting between screen and world coordinates. -/
structure TransformParams where
  /-- Screen width in pixels -/
  screenWidth : Float
  /-- Screen height in pixels -/
  screenHeight : Float
  /-- Current pan X offset -/
  panX : Float
  /-- Current pan Y offset -/
  panY : Float
  /-- Current zoom level -/
  zoom : Float
  deriving Inhabited

/-- Create transform parameters from view state and screen size. -/
def TransformParams.fromViewState (state : GalaxyViewState)
    (screenWidth screenHeight : Float) : TransformParams :=
  { screenWidth, screenHeight, panX := state.panX, panY := state.panY, zoom := state.zoom }

/-- Apply the forward transform to convert from normalized (0-1) to screen coordinates.
    This matches the transform in Galaxy.lean's galaxySpec. -/
def transformToScreen (params : TransformParams) (normX normY : Float) : Float × Float :=
  let centerX := params.screenWidth / 2.0
  let centerY := params.screenHeight / 2.0
  -- First convert normalized to base screen position
  let baseX := normX * params.screenWidth
  let baseY := normY * params.screenHeight
  -- Then apply zoom (from center) and pan
  let screenX := centerX + (baseX - centerX) * params.zoom + params.panX
  let screenY := centerY + (baseY - centerY) * params.zoom + params.panY
  (screenX, screenY)

/-- Apply the inverse transform to convert from screen coordinates to normalized (0-1).
    This is the inverse of transformToScreen. -/
def transformFromScreen (params : TransformParams) (screenX screenY : Float) : Float × Float :=
  let centerX := params.screenWidth / 2.0
  let centerY := params.screenHeight / 2.0
  -- Inverse of: screenX = centerX + (baseX - centerX) * zoom + panX
  -- => baseX = (screenX - panX - centerX) / zoom + centerX
  let baseX := (screenX - params.panX - centerX) / params.zoom + centerX
  let baseY := (screenY - params.panY - centerY) / params.zoom + centerY
  -- Convert base screen to normalized
  let normX := baseX / params.screenWidth
  let normY := baseY / params.screenHeight
  (normX, normY)

/-- A minimal star record for hit testing (to avoid circular imports). -/
structure StarHitInfo where
  /-- Normalized X position (0-1) -/
  x : Float
  /-- Normalized Y position (0-1) -/
  y : Float
  /-- Base size in pixels -/
  size : Float
  deriving Inhabited

/-- Find the star at the given screen coordinates, if any.
    Returns the index of the closest star within hit radius, or none.
    The hit radius is scaled based on zoom to maintain visual consistency. -/
def starAtPoint (stars : Array StarHitInfo) (params : TransformParams)
    (screenX screenY : Float) (hitRadiusMultiplier : Float := 1.5) : Option Nat := Id.run do
  let mut closest : Option (Nat × Float) := none
  for i in [:stars.size] do
    if h : i < stars.size then
      let star := stars[i]
      let (starScreenX, starScreenY) := transformToScreen params star.x star.y
      -- Hit radius is the star's visual size (scaled by zoom) times a multiplier
      let hitRadius := star.size * params.zoom * hitRadiusMultiplier
      let dx := screenX - starScreenX
      let dy := screenY - starScreenY
      let distSq := dx * dx + dy * dy
      let hitRadiusSq := hitRadius * hitRadius
      if distSq < hitRadiusSq then
        match closest with
        | none => closest := some (i, distSq)
        | some (_, prevDistSq) =>
          if distSq < prevDistSq then
            closest := some (i, distSq)
  closest.map (·.fst)

/-- Find all stars within a rectangular region on screen.
    Returns indices of stars whose centers fall within the region. -/
def starsInRect (stars : Array StarHitInfo) (params : TransformParams)
    (x1 y1 x2 y2 : Float) : Array Nat := Id.run do
  let minX := if x1 < x2 then x1 else x2
  let maxX := if x1 > x2 then x1 else x2
  let minY := if y1 < y2 then y1 else y2
  let maxY := if y1 > y2 then y1 else y2
  let mut result : Array Nat := #[]
  for i in [:stars.size] do
    if h : i < stars.size then
      let star := stars[i]
      let (starScreenX, starScreenY) := transformToScreen params star.x star.y
      if starScreenX >= minX && starScreenX <= maxX &&
         starScreenY >= minY && starScreenY <= maxY then
        result := result.push i
  result

end Eschaton.Widget.Galaxy
