/-
  Starfield Widget
  A reusable Afferent Arbor widget that renders an animated warp speed effect
  with stars rushing outward from a central vanishing point.
-/
import Afferent
import Afferent.Arbor
import Afferent.Shader.DSL

open Afferent CanvasM
open Afferent.Shader
open Afferent.Shader.DSL hiding size time

namespace Eschaton.Widget

/-- Configuration for the starfield widget. -/
structure StarfieldConfig where
  starCount : Nat := 500
  seed : Float := 12345.0
  warpSpeed : Float := 0.02  -- How fast stars rush past (0.1 = slow cruise, 1.0 = ludicrous speed)
  deriving Inhabited

-- Local aliases for DSL parameter accessors
private def pWidth : ShaderExpr .float := param "width" .float
private def pHeight : ShaderExpr .float := param "height" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pSeed : ShaderExpr .float := param "seed" .float
private def pWarpSpeed : ShaderExpr .float := param "warpSpeed" .float

/-- Simple hash function for pseudo-random number generation in shader.
    Returns a value in [0, 1). -/
private def hash (n : ShaderExpr .float) : ShaderExpr .float :=
  fract (sin (n * 12.9898 + 78.233) * 43758.5453)

/-- Warp speed star shader - stars rush outward from center.
    Each star has a fixed angle and cycles through depth (far to near).
    Parameters: width(1), height(1), time(1), seed(1), warpSpeed(1) = 5 floats. -/
def warpStarShader (starCount : Nat) : CircleShader := {
  name := "warpStars"
  instanceCount := starCount
  params := [
    ⟨"width", .float⟩,
    ⟨"height", .float⟩,
    ⟨"time", .float⟩,
    ⟨"seed", .float⟩,
    ⟨"warpSpeed", .float⟩
  ]
  body :=
    let idxF := toFloat idx
    let seedOffset := pSeed * 0.0001

    -- Screen center (vanishing point)
    let centerX := pWidth * 0.5
    let centerY := pHeight * 0.5

    -- Each star has a fixed angle (direction it travels)
    let angle := hash (idxF * 1.0 + seedOffset) * twoPi

    -- Each star has a unique speed variation (some faster, some slower)
    let speedVariation := 0.6 + hash (idxF * 3.0 + seedOffset + 100.0) * 0.8  -- 0.6-1.4x

    -- Phase offset so stars don't all start at the same depth
    let phaseOffset := hash (idxF * 5.0 + seedOffset + 200.0)

    -- Depth cycles from 1.0 (far/center) to 0.0 (near/edge), then wraps
    -- Using fract creates seamless looping
    let depth := 1.0 - fract (pTime * pWarpSpeed * speedVariation + phaseOffset)

    -- Distance from center increases as depth decreases (star approaches)
    -- Calculate max distance to edge based on angle (handles widescreen)
    let halfW := pWidth * 0.5
    let halfH := pHeight * 0.5
    let cosA := abs (cos angle)
    let sinA := abs (sin angle)
    -- Distance to edge: min of horizontal and vertical intersections
    -- Add small epsilon to avoid division by zero
    let distToVertEdge := halfW / (cosA + 0.001)
    let distToHorizEdge := halfH / (sinA + 0.001)
    -- Extend 20% beyond edge so stars exit off-screen before respawning
    let edgeDist := cond (lt distToVertEdge distToHorizEdge) distToVertEdge distToHorizEdge
    let maxRadius := edgeDist * 1.2

    -- Use quadratic curve so stars accelerate as they get closer
    let normalizedDist := (1.0 - depth) * (1.0 - depth)
    let distance := normalizedDist * maxRadius

    -- Star position
    let starX := centerX + cos angle * distance
    let starY := centerY + sin angle * distance

    -- Star size: tiny when far, larger when close
    let minSize := 0.3
    let maxSize := 4.0
    let starSize := minSize + normalizedDist * (maxSize - minSize)

    -- Brightness: dim when far, bright when close
    -- Fade in at spawn (depth near 1.0) so stars don't pop in abruptly
    let fadeIn := smoothstep 1.0 0.85 depth      -- Fade in as star spawns
    let distanceBrightness := 0.3 + normalizedDist * 0.7
    let brightness := distanceBrightness * fadeIn

    -- Star color: slight blue-white tint, brighter stars get whiter
    let temp := hash (idxF * 7.0 + seedOffset + 300.0)
    let hue := cond (lt temp 0.2) 0.6           -- Blue tint for some
              (cond (lt temp 0.8) 0.0           -- White (most)
                                  0.12)         -- Slight yellow for others
    let saturation := cond (lt temp 0.2) (0.3 * (1.0 - normalizedDist))  -- Blue fades to white
                     (cond (lt temp 0.8) 0.0
                                         (0.2 * (1.0 - normalizedDist)))
    let rgb := hsvToRgb hue saturation 1.0

    let r := rgb.x * brightness
    let g := rgb.y * brightness
    let b := rgb.z * brightness

    {
      center := vec2 starX starY
      radius := starSize
      strokeWidth := .litFloat 0.0
      color := vec4 r g b 1.0
    }
}

/-- Glow shader for bright rushing stars.
    Creates halos around stars as they get closer. -/
def warpGlowShader (glowCount : Nat) : CircleShader := {
  name := "warpGlow"
  instanceCount := glowCount
  params := [
    ⟨"width", .float⟩,
    ⟨"height", .float⟩,
    ⟨"time", .float⟩,
    ⟨"seed", .float⟩,
    ⟨"warpSpeed", .float⟩
  ]
  body :=
    let idxF := toFloat idx
    let seedOffset := pSeed * 0.0001

    -- Map to a subset of stars (spread across indices)
    let baseStarIdx := idxF * 5.0  -- Every 5th star gets glow consideration

    let centerX := pWidth * 0.5
    let centerY := pHeight * 0.5

    -- Same calculations as main shader
    let angle := hash (baseStarIdx * 1.0 + seedOffset) * twoPi
    let speedVariation := 0.6 + hash (baseStarIdx * 3.0 + seedOffset + 100.0) * 0.8
    let phaseOffset := hash (baseStarIdx * 5.0 + seedOffset + 200.0)
    let depth := 1.0 - fract (pTime * pWarpSpeed * speedVariation + phaseOffset)
    let normalizedDist := (1.0 - depth) * (1.0 - depth)

    -- Calculate max distance to edge based on angle (handles widescreen)
    let halfW := pWidth * 0.5
    let halfH := pHeight * 0.5
    let cosA := abs (cos angle)
    let sinA := abs (sin angle)
    let distToVertEdge := halfW / (cosA + 0.001)
    let distToHorizEdge := halfH / (sinA + 0.001)
    -- Extend 20% beyond edge (must match main shader)
    let edgeDist := cond (lt distToVertEdge distToHorizEdge) distToVertEdge distToHorizEdge
    let maxRadius := edgeDist * 1.2

    let distance := normalizedDist * maxRadius

    let starX := centerX + cos angle * distance
    let starY := centerY + sin angle * distance

    -- Glow only appears when star is close (normalizedDist > 0.3)
    let glowThreshold := 0.3
    let isClose := cond (lt normalizedDist glowThreshold) 0.0 1.0
    let glowIntensity := (normalizedDist - glowThreshold) / (1.0 - glowThreshold)

    -- Glow size and alpha
    let glowRadius := cond (lt normalizedDist glowThreshold) 0.0
                          (8.0 + glowIntensity * 12.0)  -- 8-20px

    let glowAlpha := glowIntensity * 0.15 * isClose

    -- White-blue glow
    let temp := hash (baseStarIdx * 7.0 + seedOffset + 300.0)
    let hue := cond (lt temp 0.3) 0.6 0.0  -- Some blue, mostly white
    let saturation := cond (lt temp 0.3) 0.3 0.0
    let rgb := hsvToRgb hue saturation 1.0

    {
      center := vec2 starX starY
      radius := glowRadius
      strokeWidth := .litFloat 0.0
      color := vec4 rgb.x rgb.y rgb.z glowAlpha
    }
}

/-- Compiled warp star fragment for 500 stars. -/
def warpStarFragment500 : ShaderFragment := (warpStarShader 500).compile

/-- Compiled warp glow fragment for 100 potential glows. -/
def warpGlowFragment100 : ShaderFragment := (warpGlowShader 100).compile

/-- Register the warp fragments in the global registry at module load time. -/
initialize warpFragmentRegistration : Unit ← do
  registerFragment warpStarFragment500
  registerFragment warpGlowFragment100

/-- Starfield widget spec using GPU shader fragments.
    Renders warp speed effect with stars rushing from center. -/
def starfieldSpec (config : StarfieldConfig) (t : Float) : Afferent.Arbor.CustomSpec := {
  skipCache := true  -- Ensure animation updates each frame
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect

    -- Parameters: width, height, time, seed, warpSpeed
    let params : Array Float := #[
      rect.width,
      rect.height,
      t,
      config.seed,
      config.warpSpeed
    ]

    Afferent.Arbor.RenderM.build do
      Afferent.Arbor.RenderM.pushTranslate rect.x rect.y

      -- Draw glow layer first (behind stars)
      Afferent.Arbor.RenderM.drawFragment warpGlowFragment100.hash warpGlowFragment100.primitive.toUInt32
        params warpGlowFragment100.instanceCount.toUInt32

      -- Draw main stars on top
      Afferent.Arbor.RenderM.drawFragment warpStarFragment500.hash warpStarFragment500.primitive.toUInt32
        params warpStarFragment500.instanceCount.toUInt32

      Afferent.Arbor.RenderM.popTransform
  draw := none
}

/-- Create a starfield widget that renders an animated warp speed effect.
    - `config`: Starfield configuration
    - `t`: Current time in seconds for animation -/
def starfieldWidget (config : StarfieldConfig := {}) (t : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := starfieldSpec config t) (style := { width := .percent 1.0, height := .percent 1.0 })

end Eschaton.Widget
