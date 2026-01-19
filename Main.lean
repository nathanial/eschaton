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

/-- Simple hash function for pseudo-random generation. -/
private def hash (n : Float) : Float :=
  let x := Float.sin (n * 12.9898 + 78.233) * 43758.5453
  x - x.floor

/-- Generate a procedural star system from an index. -/
private def generateStar (idx : Nat) : Widget.StarSystem :=
  let i := idx.toFloat
  -- Position with some clustering
  let angle := hash (i * 1.1) * 2.0 * Float.pi
  let radius := 0.1 + hash (i * 2.3) * 0.35
  let x := 0.5 + radius * Float.cos angle
  let y := 0.5 + radius * Float.sin angle
  -- Color temperature (blue to red)
  let temp := hash (i * 3.7)
  let (r, g, b) := if temp < 0.2 then (0.6, 0.7, 1.0)      -- Blue
                   else if temp < 0.5 then (0.9, 0.9, 1.0) -- White
                   else if temp < 0.8 then (1.0, 0.95, 0.7) -- Yellow
                   else (1.0, 0.6, 0.4)                     -- Orange/Red
  -- Size variation
  let size := 8.0 + hash (i * 5.1) * 16.0
  { name := s!"Star-{idx}", x := x, y := y, color := Color.rgb r g b, size := size }

/-- Generate hyperlanes connecting nearby stars. -/
private def generateHyperlanes (systems : Array Widget.StarSystem) : Array Widget.Hyperlane := Id.run do
  let mut lanes : Array Widget.Hyperlane := #[]
  for i in [:systems.size] do
    if h₁ : i < systems.size then
      let s1 := systems[i]
      for j in [i+1:systems.size] do
        if h₂ : j < systems.size then
          let s2 := systems[j]
          let dx := s1.x - s2.x
          let dy := s1.y - s2.y
          let dist := Float.sqrt (dx * dx + dy * dy)
          -- Connect stars within a threshold distance
          if dist < 0.08 then
            lanes := lanes.push { source := i, target := j }
  lanes

/-- Sample galaxy configuration with 400 procedurally generated star systems. -/
def sampleGalaxyConfig : Widget.GalaxyConfig :=
  let systems := Array.ofFn (n := 400) (generateStar ·.val)
  let hyperlanes := generateHyperlanes systems
  {
    systems := systems
    hyperlanes := hyperlanes
    backgroundColor := Color.rgb 0.02 0.02 0.05  -- Dark space background
    hyperlaneColor := Color.rgba 0.4 0.5 0.7 0.3
    hyperlaneWidth := 1.0
  }

/-- Game state. -/
structure GameState where
  lastTime : Nat
  screen : Screen
  starfieldConfig : Widget.StarfieldConfig
  galaxyConfig : Widget.GalaxyConfig
  screenWidth : Float
  screenHeight : Float
  -- Viewport panning state
  panX : Float          -- viewport offset X (in pixels)
  panY : Float          -- viewport offset Y (in pixels)
  isDragging : Bool     -- true when left mouse held
  lastMouseX : Float    -- for calculating drag delta
  lastMouseY : Float
  -- Zoom state
  zoom : Float          -- zoom level (1.0 = 100%)

def GameState.create (width height : Float) : IO GameState := do
  let now ← IO.monoMsNow
  pure {
    lastTime := now
    screen := .title
    starfieldConfig := {}
    galaxyConfig := sampleGalaxyConfig
    screenWidth := width
    screenHeight := height
    panX := 0.0
    panY := 0.0
    isDragging := false
    lastMouseX := 0.0
    lastMouseY := 0.0
    zoom := 1.0
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
        -- Handle mouse drag for panning
        let (mouseX, mouseY) ← canvas.ctx.window.getMousePos
        let buttons ← canvas.ctx.window.getMouseButtons
        let leftDown := buttons &&& 1 != 0

        if leftDown then
          if state.isDragging then
            -- Continue drag: update pan by delta
            let dx := mouseX - state.lastMouseX
            let dy := mouseY - state.lastMouseY
            state := { state with
              panX := state.panX + dx
              panY := state.panY + dy
              lastMouseX := mouseX
              lastMouseY := mouseY
            }
          else
            -- Start drag
            state := { state with
              isDragging := true
              lastMouseX := mouseX
              lastMouseY := mouseY
            }
        else
          -- End drag
          if state.isDragging then
            state := { state with isDragging := false }

        -- Handle scroll wheel for zooming
        let (_, scrollY) ← canvas.ctx.window.getScrollDelta
        if scrollY != 0.0 then
          -- Zoom factor: scroll up = zoom in, scroll down = zoom out
          let zoomFactor := 1.0 + scrollY * 0.1
          let newZoom := (state.zoom * zoomFactor).max 0.1 |>.min 10.0  -- Clamp between 0.1x and 10x

          -- Zoom towards mouse position
          let centerX := currentW / 2.0
          let centerY := currentH / 2.0
          -- Adjust pan so the point under the mouse stays fixed
          let scale := newZoom / state.zoom
          let newPanX := mouseX - centerX - (mouseX - centerX - state.panX) * scale
          let newPanY := mouseY - centerY - (mouseY - centerY - state.panY) * scale

          state := { state with
            zoom := newZoom
            panX := newPanX
            panY := newPanY
          }
          canvas.ctx.window.clearScroll

        -- Galaxy screen: show galaxy widget with animated stars
        let galaxyConfig := { state.galaxyConfig with
          labelFont := some _debugFontId
          time := t
          panX := state.panX
          panY := state.panY
          zoom := state.zoom
        }
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
