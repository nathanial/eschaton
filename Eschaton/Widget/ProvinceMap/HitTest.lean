/-
  Province Map Hit Testing
  Inverse transform and hit testing for province selection.
-/
import Linalg
import Eschaton.Widget.ProvinceMap.State

namespace Eschaton.Widget.ProvinceMap

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
def TransformParams.fromViewState (state : ProvinceMapViewState)
    (screenWidth screenHeight : Float) : TransformParams :=
  { screenWidth, screenHeight, panX := state.panX, panY := state.panY, zoom := state.zoom }

/-- Apply the forward transform to convert from normalized (0-1) to screen coordinates.
    This matches the transform in ProvinceMap.lean's spec. -/
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

/-- A province polygon for hit testing. -/
structure ProvinceHitInfo where
  /-- The polygon defining the province boundary in normalized 0-1 coordinates -/
  polygon : Linalg.Polygon2D
  deriving Inhabited

/-- Find the province at the given screen coordinates, if any.
    Returns the index of the province containing the point, or none.
    Uses Polygon2D.containsPoint for accurate polygon hit testing. -/
def provinceAtPoint (provinces : Array ProvinceHitInfo) (params : TransformParams)
    (screenX screenY : Float) : Option Nat := Id.run do
  -- Transform screen coordinates to normalized space
  let (normX, normY) := transformFromScreen params screenX screenY
  let point := Linalg.Vec2.mk normX normY

  -- Check each province polygon
  for i in [:provinces.size] do
    if h : i < provinces.size then
      let province := provinces[i]
      if province.polygon.containsPoint point then
        return some i
  return none

/-- Find all provinces within a rectangular region on screen.
    Returns indices of provinces whose centroids fall within the region. -/
def provincesInRect (provinces : Array ProvinceHitInfo) (params : TransformParams)
    (x1 y1 x2 y2 : Float) : Array Nat := Id.run do
  let minX := if x1 < x2 then x1 else x2
  let maxX := if x1 > x2 then x1 else x2
  let minY := if y1 < y2 then y1 else y2
  let maxY := if y1 > y2 then y1 else y2
  let mut result : Array Nat := #[]
  for i in [:provinces.size] do
    if h : i < provinces.size then
      let province := provinces[i]
      let centroid := province.polygon.centroid
      let (screenX, screenY) := transformToScreen params centroid.x centroid.y
      if screenX >= minX && screenX <= maxX &&
         screenY >= minY && screenY <= maxY then
        result := result.push i
  result

end Eschaton.Widget.ProvinceMap
