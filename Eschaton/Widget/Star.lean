/-
  Star Widget
  GPU shader-based star rendering with bright core and glowing corona effect.
  Uses the Shader DSL to generate Metal shader code.
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

/-- Star shader definition using the DSL.
    Computes 5 circles: bright core (idx 0) + 4 glow layers (idx 1-4).
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def starShader : CircleShader := {
  name := "star"
  instanceCount := 5
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let isCore := eqU idx 0

    -- Layer radii: core is 0.15x, glows are 0.3x, 0.5x, 0.7x, 0.9x of size
    let radiusFactor := cond isCore 0.15
      (cond (eqU idx 1) 0.3
      (cond (eqU idx 2) 0.5
      (cond (eqU idx 3) 0.7 0.9)))

    -- Alpha: core is 1.0, glows fade outward (0.4, 0.25, 0.15, 0.08)
    let baseAlpha := cond isCore 1.0
      (cond (eqU idx 1) 0.4
      (cond (eqU idx 2) 0.25
      (cond (eqU idx 3) 0.15 0.08)))

    -- Pulse: subtle brightness oscillation using sin(time)
    let pulse := 0.85 + 0.15 * sin (pTime * twoPi * 0.5)
    let finalAlpha := baseAlpha * pulse * pColor.w

    {
      center := pCenter
      radius := pSize * radiusFactor
      strokeWidth := .litFloat 0.0  -- filled circles
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
