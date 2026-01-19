/-
  Starfield Widget
  A reusable Afferent Arbor widget that renders an animated starfield background.
-/
import Afferent
import Afferent.Arbor
import Trellis

open Afferent CanvasM

namespace Eschaton.Widget

/-- Configuration for the starfield widget. -/
structure StarfieldConfig where
  starCount : Nat := 500
  seed : Nat := 12345
  twinkleSpeed : Float := 2.0
  deriving Inhabited

/-- Generate star positions with brightness values using LCG random.
    Stars are generated in normalized [0,1] coordinates to be scaled at render time. -/
def generateStars (count : Nat) (seed : Nat := 12345) : Array (Float × Float × Float) := Id.run do
  let mut stars : Array (Float × Float × Float) := #[]
  let mut s := seed
  for _ in [:count] do
    -- Simple LCG random
    s := (s * 1103515245 + 12345) % (2^31)
    let x := s.toFloat / 2147483648.0
    s := (s * 1103515245 + 12345) % (2^31)
    let y := s.toFloat / 2147483648.0
    s := (s * 1103515245 + 12345) % (2^31)
    let brightness := 0.3 + (s.toFloat / 2147483648.0) * 0.7
    stars := stars.push (x, y, brightness)
  stars

/-- Cached star positions for a given config. -/
structure StarfieldState where
  config : StarfieldConfig
  stars : Array (Float × Float × Float)
  deriving Inhabited

/-- Create a starfield state from config. -/
def StarfieldState.create (config : StarfieldConfig) : StarfieldState :=
  { config
  , stars := generateStars config.starCount config.seed }

/-- Helper to draw within a layout's content rect with clipping. -/
def withContentRect (layout : Trellis.ComputedLayout) (draw : Float → Float → CanvasM Unit) : CanvasM Unit := do
  let rect := layout.contentRect
  CanvasM.save
  CanvasM.setBaseTransform (Transform.translate rect.x rect.y)
  CanvasM.resetTransform
  CanvasM.clip (Afferent.Rect.mk' 0 0 rect.width rect.height)
  draw rect.width rect.height
  CanvasM.restore

/-- Create a starfield widget that renders an animated twinkling starfield.
    - `state`: Pre-generated star positions
    - `t`: Current time in seconds for animation -/
def starfieldWidget (state : StarfieldState) (t : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)  -- Use layout-provided size
    collect := fun _ => #[]       -- No abstract commands
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        for (nx, ny, brightness) in state.stars do
          -- Scale normalized coords to content size
          let x := nx * w
          let y := ny * h
          -- Subtle twinkling effect
          let twinkle := 0.8 + 0.2 * Float.sin (t * state.config.twinkleSpeed + x * 0.1 + y * 0.1)
          let finalBrightness := brightness * twinkle
          let starColor := Color.rgba finalBrightness finalBrightness (finalBrightness * 1.1) 1.0
          let size := 1.0 + brightness * 1.5
          setFillColor starColor
          fillRectXYWH (x - size/2) (y - size/2) size size
    )
  }) (style := { width := .percent 1.0, height := .percent 1.0 })

end Eschaton.Widget
