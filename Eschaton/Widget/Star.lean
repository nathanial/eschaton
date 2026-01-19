/-
  Star Widget
  GPU shader-based star rendering with bright core and smooth radial glow.
  Uses the QuadShader DSL to render per-pixel gradients in the fragment shader.
-/
import Afferent
import Afferent.Arbor
import Afferent.Shader.DSL
import Tincture

open Afferent.Arbor
open Afferent.Shader
open Afferent.Shader.DSL hiding center size time color

namespace Eschaton.Widget

-- Local aliases for DSL parameter accessors (avoid Arbor name conflicts)
private def pCenter : ShaderExpr .float2 := param "center" .float2
private def pSize : ShaderExpr .float := param "size" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Star shader definition using QuadShader for smooth per-pixel radial glow.
    Unlike the previous CircleShader with 5 discrete circles, this renders
    a smooth gradient in the fragment shader for each pixel.
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def starShader : QuadShader := {
  name := "star"
  instanceCount := 1
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  -- Vertex shader: compute quad bounds (runs once per instance)
  vertex := {
    position := pCenter - vec2 pSize pSize
    size := vec2 (pSize * 2.0) (pSize * 2.0)
  }
  -- Fragment shader: compute per-pixel color with radial gradient (runs once per pixel)
  pixel :=
    -- Distance from center (0 at center, 1 at edge of quad)
    let dist := radialDistance

    -- Bright core: very bright near center, drops off quickly
    let coreIntensity := 1.0 - smoothstep 0.0 0.15 dist

    -- Soft glow: gradual falloff from center to edge
    let glowIntensity := 1.0 - smoothstep 0.0 1.0 dist

    -- Combine: core adds bright center on top of overall glow
    let combined := glowIntensity * 0.5 + coreIntensity * 0.5

    -- Pulse: subtle brightness oscillation
    let pulse := 0.85 + 0.15 * sin (pTime * twoPi * 0.5)

    -- Final alpha with smooth circular mask
    let circularMask := 1.0 - smoothstep 0.9 1.0 dist
    let finalAlpha := combined * pulse * pColor.w * circularMask

    {
      color := vec4 pColor.x pColor.y pColor.z finalAlpha
    }
}

/-- Compiled star fragment. -/
def starFragment : ShaderFragment := starShader.compile

/-- Register the star fragment in the global registry at module load time. -/
initialize starFragmentRegistration : Unit ← do
  registerFragment starFragment

/-- Star spec for custom widget rendering.
    Draws a star with bright core and glowing corona using GPU shader.
    - `t`: Current time in seconds for animation
    - `color`: Star color (RGB + alpha)
    - `size`: Overall star size in pixels -/
def starSpec (t : Float) (color : Tincture.Color) (size : Float) : CustomSpec := {
  skipCache := true  -- Animation requires fresh render each frame
  measure := fun _ _ => (size * 2, size * 2)  -- Size includes glow
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + rect.width / 2
    let cy := rect.y + rect.height / 2

    -- 8 floats: center(2), size(1), time(1), color(4)
    let params : Array Float := #[
      cx, cy,                                 -- center
      size,                                   -- size
      t,                                      -- time (raw seconds)
      color.r, color.g, color.b, color.a      -- color
    ]

    RenderM.build do
      RenderM.drawFragment starFragment.hash starFragment.primitive.toUInt32
        params starFragment.instanceCount.toUInt32
  draw := none
}

/-- Create a star widget with animated glow effect.
    - `t`: Current time in seconds for animation
    - `color`: Star color
    - `size`: Star size in pixels -/
def starWidget (t : Float) (color : Tincture.Color) (size : Float) : WidgetBuilder := do
  let totalSize := size * 2  -- Include glow radius
  custom (spec := starSpec t color size) (style := { width := .length totalSize, height := .length totalSize })

end Eschaton.Widget
