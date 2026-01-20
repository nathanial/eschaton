/-
  Province Map Widget
  An EU4-style province map widget showing provinces as filled polygons.
  Uses pre-tessellated batched polygon rendering for high performance.
-/
import Afferent
import Afferent.Arbor
import Afferent.Render.Tessellation
import Linalg
import Tincture
import Eschaton.Widget.ProvinceMap.State
import Eschaton.Widget.ProvinceMap.HitTest

open Afferent.Arbor
open Afferent.Tessellation (TessellatedPolygon TessellatedBatch StrokeBatch tessellatePolygonForCache)
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

/-- Convert Linalg.Vec2 array to Afferent.Point array. -/
private def vec2ArrayToPoints (verts : Array Linalg.Vec2) : Array Afferent.Point :=
  verts.map fun v => { x := v.x, y := v.y }

/-- A province in the map with pre-tessellated geometry. -/
structure Province where
  /-- Unique identifier for the province -/
  id : Nat
  /-- Display name for the province -/
  name : String
  /-- Polygon vertices in normalized 0-1 coordinates (for hit testing) -/
  polygon : Linalg.Polygon2D
  /-- Pre-tessellated polygon geometry (cached at load time) -/
  tessellated : TessellatedPolygon
  /-- Fill color for the province -/
  fillColor : Color
  /-- Border color (defaults to dark gray) -/
  borderColor : Color := Color.rgba 0.2 0.2 0.2 1.0
  deriving Inhabited

namespace Province

/-- Create a province with pre-tessellated geometry. -/
def create (id : Nat) (name : String) (polygon : Linalg.Polygon2D) (fillColor : Color)
    (borderColor : Color := Color.rgba 0.2 0.2 0.2 1.0) : Province :=
  let points := vec2ArrayToPoints polygon.vertices
  let tessellated := tessellatePolygonForCache points
  { id, name, polygon, tessellated, fillColor, borderColor }

end Province

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

/-- Province map widget spec using batched polygon rendering with reactive view state.
    Uses pre-tessellated geometry for high performance (1-2 draw calls instead of ~4000). -/
def provinceMapSpecWithState (staticConfig : ProvinceMapStaticConfig)
    (viewState : ProvinceMap.ProvinceMapViewState) : Afferent.Arbor.CustomSpec := {
  skipCache := false  -- No animation, can cache when state unchanged
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect
    -- Screen center for zoom origin (relative to content rect)
    let centerX := rect.width / 2.0
    let centerY := rect.height / 2.0

    Afferent.Arbor.RenderM.build do
      -- Fill background (ocean color, not affected by pan/zoom)
      -- Use pushTranslate for the background rect since it uses local coordinates
      RenderM.pushTranslate rect.x rect.y
      RenderM.fillRect' 0 0 rect.width rect.height staticConfig.backgroundColor
      RenderM.popTransform

      -- Build tessellated fill batch from all provinces
      -- Batch uses absolute screen coordinates (rect offset is added in addPolygon)
      let fillBatch := Id.run do
        let mut batch := TessellatedBatch.withCapacity staticConfig.provinces.size
        for i in [:staticConfig.provinces.size] do
          if h : i < staticConfig.provinces.size then
            let province := staticConfig.provinces[i]
            let isHovered := viewState.hoveredProvince == some i
            let isSelected := viewState.selectedProvince == some i

            -- Determine fill color based on hover/selection state
            let fillColor :=
              if isSelected then brighten province.fillColor 0.3
              else if isHovered then brighten province.fillColor 0.15
              else province.fillColor

            -- Add province to batch with transform and color
            -- Pass rect.x, rect.y so positions are in absolute screen coordinates
            batch := batch.addPolygon province.tessellated
              rect.x rect.y viewState.panX viewState.panY viewState.zoom
              centerX centerY rect.width rect.height
              (Afferent.Color.rgba fillColor.r fillColor.g fillColor.b fillColor.a)
        batch

      -- Emit single draw call for all province fills
      if !fillBatch.isEmpty then
        RenderM.fillTessellatedBatch fillBatch.vertices fillBatch.indices fillBatch.vertexCount

      -- Build stroke batch from all province borders
      -- Stroke batch also uses absolute screen coordinates
      let strokeBatch := Id.run do
        let mut batch := StrokeBatch.withCapacity staticConfig.provinces.size
        for i in [:staticConfig.provinces.size] do
          if h : i < staticConfig.provinces.size then
            let province := staticConfig.provinces[i]
            -- Add province border to batch (with rect offset)
            batch := batch.addPolygonBorder province.tessellated
              rect.x rect.y viewState.panX viewState.panY viewState.zoom
              centerX centerY rect.width rect.height
              (Afferent.Color.rgba province.borderColor.r province.borderColor.g province.borderColor.b province.borderColor.a)
        batch

      -- Emit single draw call for all province borders (using existing line batch)
      if !strokeBatch.isEmpty then
        RenderM.strokeLineBatch strokeBatch.data strokeBatch.lineCount staticConfig.borderWidth

      -- Draw labels if font provided (centered on province centroids)
      -- Labels use pushTranslate since fillText expects local coordinates
      if let some font := staticConfig.labelFont then
        RenderM.pushTranslate rect.x rect.y

        -- Transform a normalized position (0-1) to local coords with zoom and pan
        let transformToLocal (normX normY : Float) : Float × Float :=
          let baseX := normX * rect.width
          let baseY := normY * rect.height
          let localX := centerX + (baseX - centerX) * viewState.zoom + viewState.panX
          let localY := centerY + (baseY - centerY) * viewState.zoom + viewState.panY
          (localX, localY)

        for i in [:staticConfig.provinces.size] do
          if h : i < staticConfig.provinces.size then
            let province := staticConfig.provinces[i]
            let centroid := province.polygon.centroid
            let (cx, cy) := transformToLocal centroid.x centroid.y

            -- Highlight label for selected province
            let labelColor :=
              if viewState.selectedProvince == some i then
                Color.rgba 1.0 1.0 0.8 1.0
              else
                staticConfig.labelColor

            -- Create a bounding rect centered on the centroid
            let labelRect := Rect.mk' (cx - 100) (cy - 20) 200 40
            RenderM.fillTextBlock province.name labelRect font labelColor .center .middle

        RenderM.popTransform
  draw := none
}

/-- Convert a Province to ProvinceHitInfo for hit testing. -/
def Province.toHitInfo (province : Province) : ProvinceMap.ProvinceHitInfo :=
  { polygon := province.polygon }

/-- Convert an array of Provinces to ProvinceHitInfo array. -/
def toProvinceHitInfoArray (provinces : Array Province) : Array ProvinceMap.ProvinceHitInfo :=
  provinces.map Province.toHitInfo

end Eschaton.Widget
