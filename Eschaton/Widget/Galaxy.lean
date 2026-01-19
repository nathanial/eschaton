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

open Afferent.Arbor
open Afferent.Shader
open Tincture (Color)

namespace Eschaton.Widget

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

/-- Configuration for the galaxy widget. -/
structure GalaxyConfig where
  systems : Array StarSystem
  hyperlanes : Array Hyperlane
  backgroundColor : Color := Tincture.Color.rgb 0.02 0.02 0.05
  hyperlaneColor : Color := Tincture.Color.rgba 0.3 0.4 0.6 0.5
  hyperlaneWidth : Float := 1.5
  labelFont : Option FontId := none
  labelColor : Color := Tincture.Color.rgba 0.7 0.7 0.8 0.8
  time : Float := 0.0  -- Animation time in seconds
  deriving Inhabited

/-- Galaxy widget spec using GPU shader-based star rendering. -/
def galaxySpec (config : GalaxyConfig) : Afferent.Arbor.CustomSpec := {
  skipCache := true  -- Animation requires fresh render each frame
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect

    Afferent.Arbor.RenderM.build do
      RenderM.pushTranslate rect.x rect.y

      -- Fill background
      RenderM.fillRect' 0 0 rect.width rect.height config.backgroundColor

      -- Draw hyperlanes
      for lane in config.hyperlanes do
        if h₁ : lane.source < config.systems.size then
          if h₂ : lane.target < config.systems.size then
            let sourceSys := config.systems[lane.source]
            let targetSys := config.systems[lane.target]
            let x1 := sourceSys.x * rect.width
            let y1 := sourceSys.y * rect.height
            let x2 := targetSys.x * rect.width
            let y2 := targetSys.y * rect.height
            RenderM.emit (.strokeLine ⟨x1, y1⟩ ⟨x2, y2⟩ config.hyperlaneColor config.hyperlaneWidth)

      -- Draw star systems using GPU shader (glow layers first, then cores)
      for sys in config.systems do
        let cx := sys.x * rect.width
        let cy := sys.y * rect.height

        -- 8 floats: center(2), size(1), time(1), color(4)
        let params : Array Float := #[
          cx, cy,                                       -- center
          sys.size,                                     -- size
          config.time,                                  -- time (raw seconds)
          sys.color.r, sys.color.g, sys.color.b, sys.color.a  -- color
        ]

        RenderM.drawFragment starFragment.hash starFragment.primitive.toUInt32
          params starFragment.instanceCount.toUInt32

      -- Draw labels if font provided
      if let some font := config.labelFont then
        for sys in config.systems do
          let cx := sys.x * rect.width
          let cy := sys.y * rect.height
          -- Position label below the star (account for glow radius)
          let labelY := cy + sys.size * 0.9 + 12
          RenderM.fillText sys.name cx labelY font config.labelColor

      RenderM.popTransform
  draw := none
}

/-- Create a galaxy widget that renders a star map.
    - `config`: Galaxy configuration with systems and hyperlanes -/
def galaxyWidget (config : GalaxyConfig) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := galaxySpec config) (style := { width := .percent 1.0, height := .percent 1.0 })

end Eschaton.Widget
