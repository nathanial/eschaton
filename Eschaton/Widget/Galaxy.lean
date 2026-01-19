/-
  Galaxy Widget
  A simple galaxy map widget showing star systems connected by hyperlanes.
  Uses GPU shader-based star rendering with animated glow effects.
-/
import Afferent
import Afferent.Arbor
import Afferent.Shader.DSL
import Tincture
import Eschaton.Widget.Star
import Eschaton.Widget.Galaxy.State
import Eschaton.Widget.Galaxy.HitTest

open Afferent.Arbor
open Afferent.Shader
open Tincture (Color)

namespace Eschaton.Widget

/-- Clamp a float to be at most the given maximum. -/
private def clampMax (x max : Float) : Float := if x > max then max else x

/-- A star system in the galaxy. -/
structure StarSystem where
  name : String
  x : Float        -- normalized 0-1
  y : Float        -- normalized 0-1
  color : Color
  size : Float     -- radius in pixels
  deriving Inhabited

/-- A hyperlane connecting two star systems. -/
structure Hyperlane where
  source : Nat     -- index into systems array
  target : Nat     -- index into systems array
  deriving Inhabited

/-- Configuration for the galaxy widget (legacy, includes view state). -/
structure GalaxyConfig where
  systems : Array StarSystem
  hyperlanes : Array Hyperlane
  backgroundColor : Color := Tincture.Color.rgb 0.02 0.02 0.05
  hyperlaneColor : Color := Tincture.Color.rgba 0.3 0.4 0.6 0.5
  hyperlaneWidth : Float := 1.5
  labelFont : Option FontId := none
  labelColor : Color := Tincture.Color.rgba 0.7 0.7 0.8 0.8
  time : Float := 0.0  -- Animation time in seconds
  panX : Float := 0.0  -- Viewport pan offset X (in pixels)
  panY : Float := 0.0  -- Viewport pan offset Y (in pixels)
  zoom : Float := 1.0  -- Zoom level (1.0 = 100%)
  deriving Inhabited

/-- Static configuration for the galaxy (excludes view state). -/
structure GalaxyStaticConfig where
  systems : Array StarSystem
  hyperlanes : Array Hyperlane
  backgroundColor : Color := Tincture.Color.rgb 0.02 0.02 0.05
  hyperlaneColor : Color := Tincture.Color.rgba 0.3 0.4 0.6 0.5
  hyperlaneWidth : Float := 1.5
  labelFont : Option FontId := none
  labelColor : Color := Tincture.Color.rgba 0.7 0.7 0.8 0.8
  deriving Inhabited

/-- Convert a static config to a full config with default view state. -/
def GalaxyStaticConfig.toConfig (static : GalaxyStaticConfig) (viewState : Galaxy.GalaxyViewState)
    (time : Float) : GalaxyConfig :=
  { systems := static.systems
    hyperlanes := static.hyperlanes
    backgroundColor := static.backgroundColor
    hyperlaneColor := static.hyperlaneColor
    hyperlaneWidth := static.hyperlaneWidth
    labelFont := static.labelFont
    labelColor := static.labelColor
    time := time
    panX := viewState.panX
    panY := viewState.panY
    zoom := viewState.zoom
  }

/-- Galaxy widget spec using GPU shader-based star rendering. -/
def galaxySpec (config : GalaxyConfig) : Afferent.Arbor.CustomSpec := {
  skipCache := true  -- Animation requires fresh render each frame
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect
    -- Screen center for zoom origin
    let centerX := rect.width / 2.0
    let centerY := rect.height / 2.0

    -- Transform a position with zoom (from center) and pan
    let transform (baseX baseY : Float) : Float × Float :=
      let zoomedX := centerX + (baseX - centerX) * config.zoom + config.panX
      let zoomedY := centerY + (baseY - centerY) * config.zoom + config.panY
      (zoomedX, zoomedY)

    Afferent.Arbor.RenderM.build do
      RenderM.pushTranslate rect.x rect.y

      -- Fill background (stays fixed, not affected by pan/zoom)
      RenderM.fillRect' 0 0 rect.width rect.height config.backgroundColor

      -- Draw hyperlanes (apply zoom and pan)
      for lane in config.hyperlanes do
        if h₁ : lane.source < config.systems.size then
          if h₂ : lane.target < config.systems.size then
            let sourceSys := config.systems[lane.source]
            let targetSys := config.systems[lane.target]
            let (x1, y1) := transform (sourceSys.x * rect.width) (sourceSys.y * rect.height)
            let (x2, y2) := transform (targetSys.x * rect.width) (targetSys.y * rect.height)
            RenderM.emit (.strokeLine ⟨x1, y1⟩ ⟨x2, y2⟩ config.hyperlaneColor config.hyperlaneWidth)

      -- Draw star systems using GPU shader (apply zoom and pan)
      for sys in config.systems do
        let (cx, cy) := transform (sys.x * rect.width) (sys.y * rect.height)
        -- Scale star size with zoom
        let scaledSize := sys.size * config.zoom

        -- 8 floats: center(2), size(1), time(1), color(4)
        let params : Array Float := #[
          cx, cy,                                       -- center (with zoom and pan applied)
          scaledSize,                                   -- size (scaled with zoom)
          config.time,                                  -- time (raw seconds)
          sys.color.r, sys.color.g, sys.color.b, sys.color.a  -- color
        ]

        RenderM.drawFragment starFragment.hash starFragment.primitive.toUInt32
          params starFragment.instanceCount.toUInt32

      -- Draw labels if font provided (apply zoom and pan)
      if let some font := config.labelFont then
        for sys in config.systems do
          let (cx, cy) := transform (sys.x * rect.width) (sys.y * rect.height)
          let scaledSize := sys.size * config.zoom
          -- Position label below the star (account for glow radius)
          let labelY := cy + scaledSize * 0.9 + 12
          RenderM.fillText sys.name cx labelY font config.labelColor

      RenderM.popTransform  -- pop rect translate
  draw := none
}

/-- Create a galaxy widget that renders a star map.
    - `config`: Galaxy configuration with systems and hyperlanes -/
def galaxyWidget (config : GalaxyConfig) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := galaxySpec config) (style := { width := .percent 1.0, height := .percent 1.0 })

/-- Galaxy widget spec using GPU shader-based star rendering with reactive view state.
    This version takes separate static config, view state, and time parameters
    for use with the FRP-based reactive galaxy widget. -/
def galaxySpecWithState (staticConfig : GalaxyStaticConfig) (viewState : Galaxy.GalaxyViewState)
    (time : Float) : Afferent.Arbor.CustomSpec := {
  skipCache := true  -- Animation requires fresh render each frame
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect
    -- Screen center for zoom origin
    let centerX := rect.width / 2.0
    let centerY := rect.height / 2.0

    -- Transform a position with zoom (from center) and pan
    let transform (baseX baseY : Float) : Float × Float :=
      let zoomedX := centerX + (baseX - centerX) * viewState.zoom + viewState.panX
      let zoomedY := centerY + (baseY - centerY) * viewState.zoom + viewState.panY
      (zoomedX, zoomedY)

    Afferent.Arbor.RenderM.build do
      RenderM.pushTranslate rect.x rect.y

      -- Fill background (stays fixed, not affected by pan/zoom)
      RenderM.fillRect' 0 0 rect.width rect.height staticConfig.backgroundColor

      -- Draw hyperlanes (apply zoom and pan)
      for lane in staticConfig.hyperlanes do
        if h₁ : lane.source < staticConfig.systems.size then
          if h₂ : lane.target < staticConfig.systems.size then
            let sourceSys := staticConfig.systems[lane.source]
            let targetSys := staticConfig.systems[lane.target]
            let (x1, y1) := transform (sourceSys.x * rect.width) (sourceSys.y * rect.height)
            let (x2, y2) := transform (targetSys.x * rect.width) (targetSys.y * rect.height)
            RenderM.emit (.strokeLine ⟨x1, y1⟩ ⟨x2, y2⟩ staticConfig.hyperlaneColor staticConfig.hyperlaneWidth)

      -- Draw star systems using GPU shader (apply zoom and pan)
      for i in [:staticConfig.systems.size] do
        if h : i < staticConfig.systems.size then
          let sys := staticConfig.systems[i]
          let (cx, cy) := transform (sys.x * rect.width) (sys.y * rect.height)
          -- Scale star size with zoom
          let baseScaledSize := sys.size * viewState.zoom

          -- Modify appearance based on hover/selection state
          let isHovered := viewState.hoveredStar == some i
          let isSelected := viewState.selectedStar == some i

          -- Size boost for hovered/selected stars
          let scaledSize :=
            if isSelected then baseScaledSize * 1.3
            else if isHovered then baseScaledSize * 1.15
            else baseScaledSize

          -- Color modification for highlight effect
          let starColor :=
            if isSelected then
              -- Brighten selected star
              Color.rgba
                (clampMax (sys.color.r * 1.3) 1.0)
                (clampMax (sys.color.g * 1.3) 1.0)
                (clampMax (sys.color.b * 1.3) 1.0)
                sys.color.a
            else if isHovered then
              -- Slight brighten for hovered star
              Color.rgba
                (clampMax (sys.color.r * 1.15) 1.0)
                (clampMax (sys.color.g * 1.15) 1.0)
                (clampMax (sys.color.b * 1.15) 1.0)
                sys.color.a
            else
              sys.color

          -- 8 floats: center(2), size(1), time(1), color(4)
          let params : Array Float := #[
            cx, cy,                                       -- center (with zoom and pan applied)
            scaledSize,                                   -- size (scaled with zoom)
            time,                                         -- time (raw seconds)
            starColor.r, starColor.g, starColor.b, starColor.a  -- color
          ]

          RenderM.drawFragment starFragment.hash starFragment.primitive.toUInt32
            params starFragment.instanceCount.toUInt32

          -- Draw selection ring around selected star
          if isSelected then
            let ringRadius := scaledSize * 1.5
            RenderM.emit (.strokeCircle ⟨cx, cy⟩ ringRadius (Color.rgba 1.0 1.0 0.5 0.6) 2.0)

      -- Draw labels if font provided (apply zoom and pan)
      if let some font := staticConfig.labelFont then
        for i in [:staticConfig.systems.size] do
          if h : i < staticConfig.systems.size then
            let sys := staticConfig.systems[i]
            let (cx, cy) := transform (sys.x * rect.width) (sys.y * rect.height)
            let scaledSize := sys.size * viewState.zoom
            -- Position label below the star (account for glow radius)
            let labelY := cy + scaledSize * 0.9 + 12

            -- Highlight label for selected star
            let labelColor :=
              if viewState.selectedStar == some i then
                Color.rgba 1.0 1.0 0.8 1.0
              else
                staticConfig.labelColor

            RenderM.fillText sys.name cx labelY font labelColor

      RenderM.popTransform  -- pop rect translate
  draw := none
}

/-- Convert a legacy GalaxyConfig to a static config. -/
def GalaxyConfig.toStatic (config : GalaxyConfig) : GalaxyStaticConfig :=
  { systems := config.systems
    hyperlanes := config.hyperlanes
    backgroundColor := config.backgroundColor
    hyperlaneColor := config.hyperlaneColor
    hyperlaneWidth := config.hyperlaneWidth
    labelFont := config.labelFont
    labelColor := config.labelColor
  }

/-- Convert a StarSystem to StarHitInfo for hit testing. -/
def StarSystem.toHitInfo (sys : StarSystem) : Galaxy.StarHitInfo :=
  { x := sys.x, y := sys.y, size := sys.size }

/-- Convert an array of StarSystems to StarHitInfo array. -/
def toStarHitInfoArray (systems : Array StarSystem) : Array Galaxy.StarHitInfo :=
  systems.map StarSystem.toHitInfo

end Eschaton.Widget
