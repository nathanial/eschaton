/-
  Province Map Widget
  An EU4-style province map widget showing provinces as filled polygons.
  Uses polygon rendering with solid fill colors and borders.
-/
import Afferent
import Afferent.Arbor
import Linalg
import Tincture
import Eschaton.Widget.ProvinceMap.State
import Eschaton.Widget.ProvinceMap.HitTest

open Afferent.Arbor
open Tincture (Color)

namespace Eschaton.Widget

/-- Clamp a float to be at most the given maximum. -/
private def clampMax (x max : Float) : Float := if x > max then max else x

/-- Brighten a color by a percentage (0.0 - 1.0). -/
private def brighten (color : Color) (amount : Float) : Color :=
  Color.rgba
    (clampMax (color.r + (1.0 - color.r) * amount) 1.0)
    (clampMax (color.g + (1.0 - color.g) * amount) 1.0)
    (clampMax (color.b + (1.0 - color.b) * amount) 1.0)
    color.a

/-- A province in the map. -/
structure Province where
  /-- Unique identifier for the province -/
  id : Nat
  /-- Display name for the province -/
  name : String
  /-- Polygon vertices in normalized 0-1 coordinates -/
  polygon : Linalg.Polygon2D
  /-- Fill color for the province -/
  fillColor : Color
  /-- Border color (defaults to dark gray) -/
  borderColor : Color := Color.rgba 0.2 0.2 0.2 1.0
  deriving Inhabited

/-- Static configuration for the province map (excludes view state). -/
structure ProvinceMapStaticConfig where
  /-- Array of provinces to display -/
  provinces : Array Province
  /-- Background color (ocean/empty space) -/
  backgroundColor : Color := Color.rgb 0.1 0.15 0.2
  /-- Border width in pixels -/
  borderWidth : Float := 1.5
  /-- Optional font for province labels -/
  labelFont : Option FontId := none
  /-- Label text color -/
  labelColor : Color := Color.rgba 0.9 0.9 0.9 0.9
  deriving Inhabited

/-- Province map widget spec using polygon rendering with reactive view state. -/
def provinceMapSpecWithState (staticConfig : ProvinceMapStaticConfig)
    (viewState : ProvinceMap.ProvinceMapViewState) : Afferent.Arbor.CustomSpec := {
  skipCache := false  -- No animation, can cache when state unchanged
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect
    -- Screen center for zoom origin
    let centerX := rect.width / 2.0
    let centerY := rect.height / 2.0

    -- Transform a normalized position (0-1) to screen coords with zoom and pan
    let transformToScreen (normX normY : Float) : Float × Float :=
      let baseX := normX * rect.width
      let baseY := normY * rect.height
      let screenX := centerX + (baseX - centerX) * viewState.zoom + viewState.panX
      let screenY := centerY + (baseY - centerY) * viewState.zoom + viewState.panY
      (screenX, screenY)

    Afferent.Arbor.RenderM.build do
      RenderM.pushTranslate rect.x rect.y

      -- Fill background (ocean color, not affected by pan/zoom)
      RenderM.fillRect' 0 0 rect.width rect.height staticConfig.backgroundColor

      -- Draw province fills first
      for i in [:staticConfig.provinces.size] do
        if h : i < staticConfig.provinces.size then
          let province := staticConfig.provinces[i]
          let isHovered := viewState.hoveredProvince == some i
          let isSelected := viewState.selectedProvince == some i

          -- Transform polygon vertices to screen space
          let screenVerts := province.polygon.vertices.map fun v =>
            let (sx, sy) := transformToScreen v.x v.y
            Point.mk sx sy

          -- Determine fill color based on hover/selection state
          let fillColor :=
            if isSelected then brighten province.fillColor 0.3
            else if isHovered then brighten province.fillColor 0.15
            else province.fillColor

          -- Fill polygon
          RenderM.emit (.fillPolygon screenVerts fillColor)

      -- Draw province borders (on top of fills for crisp edges)
      for i in [:staticConfig.provinces.size] do
        if h : i < staticConfig.provinces.size then
          let province := staticConfig.provinces[i]
          let isSelected := viewState.selectedProvince == some i

          -- Transform polygon vertices to screen space
          let screenVerts := province.polygon.vertices.map fun v =>
            let (sx, sy) := transformToScreen v.x v.y
            Point.mk sx sy

          -- Double border width for selected provinces
          let borderWidth :=
            if isSelected then staticConfig.borderWidth * 2.0
            else staticConfig.borderWidth

          -- Stroke polygon border
          RenderM.emit (.strokePolygon screenVerts province.borderColor borderWidth)

      -- Draw labels if font provided (centered on province centroids)
      if let some font := staticConfig.labelFont then
        for i in [:staticConfig.provinces.size] do
          if h : i < staticConfig.provinces.size then
            let province := staticConfig.provinces[i]
            let centroid := province.polygon.centroid
            let (cx, cy) := transformToScreen centroid.x centroid.y

            -- Highlight label for selected province
            let labelColor :=
              if viewState.selectedProvince == some i then
                Color.rgba 1.0 1.0 0.8 1.0
              else
                staticConfig.labelColor

            RenderM.fillText province.name cx cy font labelColor

      RenderM.popTransform  -- pop rect translate
  draw := none
}

/-- Convert a Province to ProvinceHitInfo for hit testing. -/
def Province.toHitInfo (province : Province) : ProvinceMap.ProvinceHitInfo :=
  { polygon := province.polygon }

/-- Convert an array of Provinces to ProvinceHitInfo array. -/
def toProvinceHitInfoArray (provinces : Array Province) : Array ProvinceMap.ProvinceHitInfo :=
  provinces.map Province.toHitInfo

end Eschaton.Widget
