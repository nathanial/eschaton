/-
  Eschaton - A Stellaris-inspired grand strategy game
  Main entry point with window setup and game loop
-/
import Afferent
import Eschaton

open Afferent Afferent.FFI
open Linalg

namespace Eschaton

/-- Generate star positions for the background. -/
def generateStars (count : Nat) (width height : Float) (seed : Nat := 12345) : Array (Float × Float × Float) := Id.run do
  let mut stars : Array (Float × Float × Float) := #[]
  let mut s := seed
  for _ in [:count] do
    -- Simple LCG random
    s := (s * 1103515245 + 12345) % (2^31)
    let x := (s.toFloat / 2147483648.0) * width
    s := (s * 1103515245 + 12345) % (2^31)
    let y := (s.toFloat / 2147483648.0) * height
    s := (s * 1103515245 + 12345) % (2^31)
    let brightness := 0.3 + (s.toFloat / 2147483648.0) * 0.7
    stars := stars.push (x, y, brightness)
  stars

/-- Game state. -/
structure GameState where
  lastTime : Nat
  stars : Array (Float × Float × Float)
  screenWidth : Float
  screenHeight : Float

def GameState.create (width height : Float) : IO GameState := do
  let now ← IO.monoMsNow
  let stars := generateStars 500 width height
  pure {
    lastTime := now
    stars := stars
    screenWidth := width
    screenHeight := height
  }

end Eschaton

def main : IO Unit := do
  IO.println "Eschaton"
  IO.println "========"
  IO.println "A grand strategy game"
  IO.println ""

  -- Initialize FFI
  FFI.init

  -- Get screen scale for Retina displays
  let screenScale ← FFI.getScreenScale

  -- Window dimensions
  let baseWidth : Float := 1280.0
  let baseHeight : Float := 720.0
  let physWidth := (baseWidth * screenScale).toUInt32
  let physHeight := (baseHeight * screenScale).toUInt32

  IO.println s!"Screen scale: {screenScale}"
  IO.println s!"Window size: {physWidth}x{physHeight}"

  -- Create window
  let mut canvas ← Canvas.create physWidth physHeight "Eschaton"

  -- Load fonts
  let titleFont ← Afferent.Font.load "/System/Library/Fonts/Helvetica.ttc" (48 * screenScale).toUInt32
  let subtitleFont ← Afferent.Font.load "/System/Library/Fonts/Helvetica.ttc" (24 * screenScale).toUInt32
  let debugFont ← Afferent.Font.load "/System/Library/Fonts/Monaco.ttf" (14 * screenScale).toUInt32

  -- Initialize game state
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale
  let mut state ← Eschaton.GameState.create physWidthF physHeightF

  let startTime ← IO.monoMsNow
  let mut frameCount : Nat := 0
  let mut displayFps : Float := 0.0
  let mut fpsAccumulator : Float := 0.0

  -- Main game loop
  while !(← canvas.shouldClose) do
    canvas.pollEvents

    -- Calculate delta time
    let now ← IO.monoMsNow
    let dt := (now - state.lastTime).toFloat / 1000.0
    let t := (now - startTime).toFloat / 1000.0
    state := { state with lastTime := now }

    -- FPS calculation
    frameCount := frameCount + 1
    if dt > 0.0 then
      fpsAccumulator := fpsAccumulator + (1.0 / dt)
    if frameCount >= 30 then
      displayFps := fpsAccumulator / frameCount.toFloat
      fpsAccumulator := 0.0
      frameCount := 0

    -- Begin frame with deep space background
    let ok ← canvas.beginFrame (Color.rgba 0.02 0.02 0.06 1.0)

    if ok then
      -- Get current window size
      let (currentW, currentH) ← canvas.ctx.getCurrentSize

      -- Draw starfield
      for (x, y, brightness) in state.stars do
        -- Subtle twinkling effect
        let twinkle := 0.8 + 0.2 * Float.sin (t * 2.0 + x * 0.1 + y * 0.1)
        let finalBrightness := brightness * twinkle
        let starColor := Color.rgba finalBrightness finalBrightness (finalBrightness * 1.1) 1.0
        let size := 1.0 + brightness * 1.5
        canvas.ctx.fillRectXYWH (x - size/2) (y - size/2) size size starColor

      -- Draw title
      let titleText := "ESCHATON"
      let (titleWidth, _) ← canvas.ctx.measureText titleText titleFont
      let titleX := (currentW - titleWidth) / 2.0
      let titleY := currentH * 0.35
      canvas.ctx.fillTextXY titleText titleX titleY titleFont (Color.rgba 0.9 0.85 0.7 1.0)

      -- Draw subtitle with pulsing effect
      let subtitleText := "The End of Everything"
      let (subtitleWidth, _) ← canvas.ctx.measureText subtitleText subtitleFont
      let subtitleX := (currentW - subtitleWidth) / 2.0
      let subtitleY := titleY + 60.0 * screenScale
      let subtitleAlpha := 0.5 + 0.3 * Float.sin (t * 1.5)
      canvas.ctx.fillTextXY subtitleText subtitleX subtitleY subtitleFont (Color.rgba 0.7 0.7 0.8 subtitleAlpha)

      -- Draw "Press any key to continue" with slower pulse
      let promptText := "Press any key to begin"
      let (promptWidth, _) ← canvas.ctx.measureText promptText debugFont
      let promptX := (currentW - promptWidth) / 2.0
      let promptY := currentH * 0.75
      let promptAlpha := 0.4 + 0.4 * Float.sin (t * 2.0)
      canvas.ctx.fillTextXY promptText promptX promptY debugFont (Color.rgba 0.6 0.6 0.7 promptAlpha)

      -- Debug: FPS counter
      let fpsText := s!"FPS: {displayFps.toUInt32}"
      canvas.ctx.fillTextXY fpsText 10 30 debugFont (Color.rgba 0.5 0.5 0.5 1.0)

      canvas ← canvas.endFrame

  -- Cleanup
  IO.println "Cleaning up..."
  titleFont.destroy
  subtitleFont.destroy
  debugFont.destroy
  canvas.destroy
  IO.println "Done!"
