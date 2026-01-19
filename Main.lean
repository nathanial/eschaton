/-
  Eschaton - A Stellaris-inspired grand strategy game
  Main entry point with window setup and game loop
-/
import Afferent
import Afferent.Arbor
import Afferent.Widget
import Eschaton

open Afferent Afferent.FFI
open Linalg

namespace Eschaton

/-- Game screens. -/
inductive Screen where
  | title
  | galaxy
  deriving BEq, Inhabited

/-- Sample galaxy configuration with 6 star systems.
    Layout (normalized coordinates):
    ```
    Sol -------- Alpha Centauri
      \              |
       \             |
        Sirius ----- Vega
          |           \
          |            \
        Rigel ------- Betelgeuse
    ``` -/
def sampleGalaxyConfig : Widget.GalaxyConfig := {
  systems := #[
    { name := "Sol", x := 0.25, y := 0.25, color := Color.rgb 1.0 0.95 0.7, size := 8 },
    { name := "Alpha Centauri", x := 0.70, y := 0.20, color := Color.rgb 1.0 0.8 0.4, size := 6 },
    { name := "Sirius", x := 0.35, y := 0.50, color := Color.rgb 0.7 0.8 1.0, size := 10 },
    { name := "Vega", x := 0.65, y := 0.45, color := Color.rgb 0.9 0.9 1.0, size := 7 },
    { name := "Rigel", x := 0.30, y := 0.75, color := Color.rgb 0.6 0.7 1.0, size := 9 },
    { name := "Betelgeuse", x := 0.72, y := 0.70, color := Color.rgb 1.0 0.5 0.3, size := 12 }
  ]
  hyperlanes := #[
    { source := 0, target := 1 },  -- Sol - Alpha Centauri
    { source := 0, target := 2 },  -- Sol - Sirius
    { source := 1, target := 3 },  -- Alpha Centauri - Vega
    { source := 2, target := 3 },  -- Sirius - Vega
    { source := 2, target := 4 },  -- Sirius - Rigel
    { source := 3, target := 5 },  -- Vega - Betelgeuse
    { source := 4, target := 5 }   -- Rigel - Betelgeuse
  ]
  backgroundColor := Color.rgb 0.02 0.02 0.05  -- Dark space background
  hyperlaneColor := Color.rgba 0.4 0.5 0.7 0.4
  hyperlaneWidth := 2.0
}

/-- Game state. -/
structure GameState where
  lastTime : Nat
  screen : Screen
  starfieldConfig : Widget.StarfieldConfig
  galaxyConfig : Widget.GalaxyConfig
  screenWidth : Float
  screenHeight : Float

def GameState.create (width height : Float) : IO GameState := do
  let now ← IO.monoMsNow
  pure {
    lastTime := now
    screen := .title
    starfieldConfig := {}
    galaxyConfig := sampleGalaxyConfig
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

  -- Create font registry for Arbor
  let (fontRegistry, _debugFontId) := FontRegistry.empty.register debugFont "debug"

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

    -- Handle input
    if ← canvas.hasKeyPressed then
      canvas.clearKey
      -- Transition from title to galaxy on any key press
      if state.screen == .title then
        state := { state with screen := .galaxy }

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

      -- Screen-specific rendering
      match state.screen with
      | .title =>
        -- Build the starfield widget (GPU shader-based)
        let starfieldBuilder := Eschaton.Widget.starfieldWidget state.starfieldConfig t
        let starfieldWidget := Afferent.Arbor.buildFrom 1 starfieldBuilder

        -- Measure and layout the widget to fill the screen
        let measureResult ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget starfieldWidget currentW currentH)
        let layouts := Trellis.layout measureResult.node currentW currentH

        -- Collect render commands (none for starfield since it uses custom draw)
        let (commands, _cacheHits, _cacheMisses) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache measureResult.widget layouts

        -- Execute commands and render custom widgets (starfield)
        canvas ← CanvasM.run' canvas do
          Afferent.Widget.executeCommandsBatched fontRegistry commands
          Afferent.Widget.renderCustomWidgets measureResult.widget layouts

        -- Title screen: show title text over starfield
        canvas ← CanvasM.run' canvas do
          -- Draw title (centered on screen)
          let titleText := "ESCHATON"
          let (titleWidth, _) ← CanvasM.measureText titleText titleFont
          let titleX := (currentW - titleWidth) / 2.0
          let titleY := currentH * 0.5
          CanvasM.fillTextColor titleText ⟨titleX, titleY⟩ titleFont (Color.rgba 0.9 0.85 0.7 1.0)

          -- Draw subtitle with pulsing effect
          let subtitleText := "The End of Everything"
          let (subtitleWidth, _) ← CanvasM.measureText subtitleText subtitleFont
          let subtitleX := (currentW - subtitleWidth) / 2.0
          let subtitleY := titleY + 60.0 * screenScale
          let subtitleAlpha := 0.5 + 0.3 * Float.sin (t * 1.5)
          CanvasM.fillTextColor subtitleText ⟨subtitleX, subtitleY⟩ subtitleFont (Color.rgba 0.7 0.7 0.8 subtitleAlpha)

          -- Draw "Press any key to continue" with slower pulse
          let promptText := "Press any key to begin"
          let (promptWidth, _) ← CanvasM.measureText promptText debugFont
          let promptX := (currentW - promptWidth) / 2.0
          let promptY := currentH * 0.75
          let promptAlpha := 0.4 + 0.4 * Float.sin (t * 2.0)
          CanvasM.fillTextColor promptText ⟨promptX, promptY⟩ debugFont (Color.rgba 0.6 0.6 0.7 promptAlpha)

      | .galaxy =>
        -- Galaxy screen: show galaxy widget (no starfield)
        let galaxyConfig := { state.galaxyConfig with labelFont := some _debugFontId }
        let galaxyBuilder := Eschaton.Widget.galaxyWidget galaxyConfig
        let galaxyWidgetTree := Afferent.Arbor.buildFrom 2 galaxyBuilder

        -- Measure and layout the galaxy widget
        let galaxyMeasure ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget galaxyWidgetTree currentW currentH)
        let galaxyLayouts := Trellis.layout galaxyMeasure.node currentW currentH

        -- Collect and execute galaxy render commands
        let (galaxyCommands, _, _) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache galaxyMeasure.widget galaxyLayouts

        canvas ← CanvasM.run' canvas do
          Afferent.Widget.executeCommandsBatched fontRegistry galaxyCommands
          Afferent.Widget.renderCustomWidgets galaxyMeasure.widget galaxyLayouts

      -- Debug: FPS counter (always shown)
      canvas ← CanvasM.run' canvas do
        let fpsText := s!"FPS: {displayFps.toUInt32}"
        CanvasM.fillTextColor fpsText ⟨10, 30⟩ debugFont (Color.rgba 0.5 0.5 0.5 1.0)

      canvas ← canvas.endFrame

  -- Cleanup
  IO.println "Cleaning up..."
  titleFont.destroy
  subtitleFont.destroy
  debugFont.destroy
  canvas.destroy
  IO.println "Done!"
