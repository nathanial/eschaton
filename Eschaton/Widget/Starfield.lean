/-
  Starfield Widget
  A reusable Afferent Arbor widget that renders an animated starfield background
  with varied star sizes, realistic colors, glow effects, and natural twinkling.
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
  twinkleSpeed : Float := 0.1  -- Slow, subtle twinkle
  glowEnabled : Bool := true
  deriving Inhabited

-- Local aliases for DSL parameter accessors
private def pWidth : ShaderExpr .float := param "width" .float
private def pHeight : ShaderExpr .float := param "height" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pSeed : ShaderExpr .float := param "seed" .float
private def pTwinkleSpeed : ShaderExpr .float := param "twinkleSpeed" .float

/-- Simple hash function for pseudo-random number generation in shader.
    Returns a value in [0, 1). -/
private def hash (n : ShaderExpr .float) : ShaderExpr .float :=
  fract (sin (n * 12.9898 + 78.233) * 43758.5453)

/-- Alternative hash for more variation. -/
private def hash2 (n : ShaderExpr .float) : ShaderExpr .float :=
  fract (sin (n * 43.758 + 23.421) * 28462.3719)

/-- Main star shader using circles with varied sizes and colors.
    Parameters: width(1), height(1), time(1), seed(1), twinkleSpeed(1) = 5 floats. -/
def starShader (starCount : Nat) : CircleShader := {
  name := "stars"
  instanceCount := starCount
  params := [
    ⟨"width", .float⟩,
    ⟨"height", .float⟩,
    ⟨"time", .float⟩,
    ⟨"seed", .float⟩,
    ⟨"twinkleSpeed", .float⟩
  ]
  body :=
    let idxF := toFloat idx
    let seedOffset := pSeed * 0.0001

    -- Position
    let starX := hash (idxF * 1.0 + seedOffset) * pWidth
    let starY := hash (idxF * 2.0 + seedOffset + 100.0) * pHeight

    -- Star tier determines size (based on hash)
    -- Tiny (70%): 0.5-1.0, Small (20%): 1.0-2.0, Medium (8%): 2.0-3.5, Bright (2%): 3.5-5.0
    let tier := hash (idxF * 7.0 + seedOffset + 300.0)
    let radius := cond (lt tier 0.70) (0.5 + tier * 0.7)           -- Tiny: 0.5-0.99
                 (cond (lt tier 0.90) (1.0 + (tier - 0.70) * 5.0)   -- Small: 1.0-2.0
                 (cond (lt tier 0.98) (2.0 + (tier - 0.90) * 18.75) -- Medium: 2.0-3.5
                                      (3.5 + (tier - 0.98) * 75.0))) -- Bright: 3.5-5.0

    -- Star color based on "temperature" hash
    -- Blue giants (hue ~0.6), White (no saturation), Yellow (~0.12), Orange/Red (~0.06)
    let temp := hash (idxF * 11.0 + seedOffset + 400.0)
    let hue := cond (lt temp 0.15) (0.58 + temp * 0.13)     -- Blue-ish (0.58-0.60)
              (cond (lt temp 0.70) 0.0                       -- White (unused hue)
              (cond (lt temp 0.85) 0.12                      -- Yellow
                                   0.06))                    -- Orange/red
    let saturation := cond (lt temp 0.15) 0.5               -- Blue stars
                     (cond (lt temp 0.70) 0.0               -- White stars
                     (cond (lt temp 0.85) 0.35              -- Yellow stars
                                          0.45))            -- Orange/red stars

    -- Convert HSV to RGB (v=1.0 for stars, brightness controlled separately)
    let rgb := hsvToRgb hue saturation 1.0

    -- Multi-frequency twinkle for organic feel
    -- Primary oscillation (slower)
    let rate1 := 0.4 + hash (idxF * 13.0 + seedOffset + 500.0) * 0.6  -- 0.4-1.0x
    let phase1 := hash (idxF * 17.0 + seedOffset + 600.0) * twoPi
    let twinkle1 := oscillate (pTime * pTwinkleSpeed * rate1 + phase1) 1.0

    -- Secondary oscillation (faster, smaller amplitude)
    let rate2 := 1.2 + hash (idxF * 19.0 + seedOffset + 700.0) * 1.5  -- 1.2-2.7x
    let phase2 := hash (idxF * 23.0 + seedOffset + 800.0) * twoPi
    let twinkle2 := oscillate (pTime * pTwinkleSpeed * rate2 + phase2) 1.0

    -- Combine: primary dominates, secondary adds texture
    let twinkle := twinkle1 * 0.7 + twinkle2 * 0.3

    -- Occasional flash: some stars spike bright when twinkle peaks
    -- Use a threshold on combined twinkle to create rare bright moments
    let flashThreshold := 0.85
    let isFlashing := cond (lt twinkle flashThreshold) 0.0 1.0
    let flashBoost := isFlashing * (twinkle - flashThreshold) * 3.0  -- Boost when above threshold

    -- Lower base brightness so twinkle is more dramatic
    let baseBrightness := 0.25 + hash (idxF * 3.0 + seedOffset + 200.0) * 0.2  -- 0.25-0.45
    let sizeBrightness := cond (lt tier 0.70) 0.6           -- Tiny stars dimmer
                         (cond (lt tier 0.90) 0.8           -- Small stars
                         (cond (lt tier 0.98) 1.0           -- Medium stars
                                              1.0))          -- Bright stars

    -- Brightness swings from low base up to bright peak
    let brightness := (baseBrightness + twinkle * 0.6 + flashBoost) * sizeBrightness

    -- Size pulsing: stars grow slightly when brighter (bloom effect)
    let sizeMultiplier := 1.0 + twinkle * 0.25 + flashBoost * 0.5

    -- Final color
    let r := rgb.x * brightness
    let g := rgb.y * brightness
    let b := rgb.z * brightness

    {
      center := vec2 starX starY
      radius := radius * sizeMultiplier
      strokeWidth := .litFloat 0.0  -- Filled circles
      color := vec4 r g b 1.0
    }
}

/-- Glow shader for the ~50 brightest stars.
    Creates a larger, semi-transparent filled halo around bright stars. -/
def starGlowShader (glowCount : Nat) : CircleShader := {
  name := "starGlow"
  instanceCount := glowCount
  params := [
    ⟨"width", .float⟩,
    ⟨"height", .float⟩,
    ⟨"time", .float⟩,
    ⟨"seed", .float⟩,
    ⟨"twinkleSpeed", .float⟩
  ]
  body :=
    -- Map glow index to star indices spread across the range
    let idxF := toFloat idx
    let seedOffset := pSeed * 0.0001

    -- Spread glow instances across all stars (not just high indices)
    let baseStarIdx := idxF * 10.0  -- Maps 0-49 to stars 0, 10, 20, ...

    -- Must match main shader's position calculation
    let starX := hash (baseStarIdx * 1.0 + seedOffset) * pWidth
    let starY := hash (baseStarIdx * 2.0 + seedOffset + 100.0) * pHeight

    -- Get tier (same formula as main shader)
    let tier := hash (baseStarIdx * 7.0 + seedOffset + 300.0)

    -- Only medium and bright stars (tier >= 0.90) get glow
    -- For non-bright stars, set radius to 0 so nothing renders
    let isBright := cond (lt tier 0.90) 0.0
                   (cond (lt tier 0.98) 0.6
                                        1.0)

    -- Base radius for glow (0 for non-bright, larger for bright)
    -- Medium stars: ~8px glow, Bright stars: ~12px glow
    let baseRadius := cond (lt tier 0.90) 0.0
                     (cond (lt tier 0.98) 8.0
                                          12.0)

    -- Color (match star color for glow)
    let temp := hash (baseStarIdx * 11.0 + seedOffset + 400.0)
    let hue := cond (lt temp 0.15) (0.58 + temp * 0.13)
              (cond (lt temp 0.70) 0.0
              (cond (lt temp 0.85) 0.12
                                   0.06))
    let saturation := cond (lt temp 0.15) 0.5
                     (cond (lt temp 0.70) 0.0
                     (cond (lt temp 0.85) 0.35
                                          0.45))
    let rgb := hsvToRgb hue saturation 1.0

    -- Twinkle for glow (sync with star - same multi-frequency formula)
    let rate1 := 0.4 + hash (baseStarIdx * 13.0 + seedOffset + 500.0) * 0.6
    let phase1 := hash (baseStarIdx * 17.0 + seedOffset + 600.0) * twoPi
    let twinkle1 := oscillate (pTime * pTwinkleSpeed * rate1 + phase1) 1.0

    let rate2 := 1.2 + hash (baseStarIdx * 19.0 + seedOffset + 700.0) * 1.5
    let phase2 := hash (baseStarIdx * 23.0 + seedOffset + 800.0) * twoPi
    let twinkle2 := oscillate (pTime * pTwinkleSpeed * rate2 + phase2) 1.0

    let twinkle := twinkle1 * 0.7 + twinkle2 * 0.3

    -- Flash boost for glow (syncs with star flash)
    let flashThreshold := 0.85
    let isFlashing := cond (lt twinkle flashThreshold) 0.0 1.0
    let flashBoost := isFlashing * (twinkle - flashThreshold) * 2.0

    -- Glow alpha (pulses with twinkle, brighter during flash)
    let glowAlpha := (0.04 + twinkle * 0.10 + flashBoost * 0.15) * isBright

    {
      center := vec2 starX starY
      radius := baseRadius * (1.0 + twinkle * 0.3 + flashBoost * 0.5)  -- Glow grows with brightness
      strokeWidth := .litFloat 0.0  -- Filled circle, not a ring
      color := vec4 rgb.x rgb.y rgb.z glowAlpha
    }
}

/-- Compiled star fragment for 500 stars. -/
def starFragment500 : ShaderFragment := (starShader 500).compile

/-- Compiled glow fragment for 50 glowing stars. -/
def starGlowFragment50 : ShaderFragment := (starGlowShader 50).compile

/-- Register the star fragments in the global registry at module load time. -/
initialize starfieldFragmentRegistration : Unit ← do
  registerFragment starFragment500
  registerFragment starGlowFragment50

/-- Starfield widget spec using GPU shader fragments.
    Renders glow layer first (behind), then main stars.
    Passes only 5 floats to GPU; the shader computes all star positions, sizes, and colors. -/
def starfieldSpec (config : StarfieldConfig) (t : Float) : Afferent.Arbor.CustomSpec := {
  skipCache := true  -- Ensure animation updates each frame
  measure := fun _ _ => (0, 0)  -- Use layout-provided size
  collect := fun layout =>
    let rect := layout.contentRect

    -- Parameters: width, height, time, seed, twinkleSpeed
    let params : Array Float := #[
      rect.width,
      rect.height,
      t,
      config.seed,
      config.twinkleSpeed
    ]

    Afferent.Arbor.RenderM.build do
      -- Translate to content rect position
      Afferent.Arbor.RenderM.pushTranslate rect.x rect.y

      -- Draw glow layer first (behind stars) if enabled
      if config.glowEnabled then
        Afferent.Arbor.RenderM.drawFragment starGlowFragment50.hash starGlowFragment50.primitive.toUInt32
          params starGlowFragment50.instanceCount.toUInt32

      -- Draw main stars on top
      Afferent.Arbor.RenderM.drawFragment starFragment500.hash starFragment500.primitive.toUInt32
        params starFragment500.instanceCount.toUInt32

      Afferent.Arbor.RenderM.popTransform
  draw := none
}

/-- Create a starfield widget that renders an animated twinkling starfield.
    - `config`: Starfield configuration
    - `t`: Current time in seconds for animation -/
def starfieldWidget (config : StarfieldConfig := {}) (t : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := starfieldSpec config t) (style := { width := .percent 1.0, height := .percent 1.0 })

end Eschaton.Widget
