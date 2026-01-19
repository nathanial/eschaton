/-
  Eschaton - A Stellaris-inspired grand strategy game
  Main entry point with window setup and FRP-based reactive game loop
-/
import Afferent
import Afferent.Arbor
import Afferent.Widget
import Afferent.Canopy
import Eschaton
import Reactive

open Afferent Afferent.FFI
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Reactive Reactive.Host Reactive.Host.Spider
open Linalg
open Eschaton.Widget (StarfieldConfig starfieldWidget GalaxyStaticConfig galaxySpecWithState
  StarSystem Hyperlane toStarHitInfoArray)
open Eschaton.Widget.Galaxy (GalaxyViewState reactiveGalaxy)

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
private def generateStar (idx : Nat) : StarSystem :=
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
private def generateHyperlanes (systems : Array StarSystem) : Array Hyperlane := Id.run do
  let mut lanes : Array Hyperlane := #[]
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
def sampleGalaxyConfig : GalaxyStaticConfig :=
  let systems := Array.ofFn (n := 400) (generateStar ·.val)
  let hyperlanes := generateHyperlanes systems
  {
    systems := systems
    hyperlanes := hyperlanes
    backgroundColor := Color.rgb 0.02 0.02 0.05  -- Dark space background
    hyperlaneColor := Color.rgba 0.4 0.5 0.7 0.3
    hyperlaneWidth := 1.0
  }

end Eschaton

def main : IO Unit := do
  IO.println "Eschaton"
  IO.println "========"
  IO.println "A grand strategy game (FRP-based reactive rendering)"
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
  let (fontRegistry, debugFontId) := FontRegistry.empty.register debugFont "debug"

  -- Game configuration
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale
  let galaxyConfig := { Eschaton.sampleGalaxyConfig with labelFont := some debugFontId }
  let hitInfos := toStarHitInfoArray galaxyConfig.systems

  -- Initialize FRP environment
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let startTime ← IO.monoMsNow
  let mut frameCount : Nat := 0
  let mut displayFps : Float := 0.0
  let mut fpsAccumulator : Float := 0.0
  let mut lastTime := startTime

  -- Screen state (not in FRP - simple state machine for title/galaxy transition)
  let mut currentScreen : Eschaton.Screen := .title

  -- Starfield config for title screen
  let starfieldConfig : StarfieldConfig := {}

  -- Track mouse button state for click/mouseup (manual since Window doesn't have this built-in)
  let prevLeftDown ← IO.mkRef false

  -- Run the FRP setup to create the reactive galaxy widget
  let ((galaxyResult, galaxyRender), inputs) ← (do
    let (events, inputs) ← createInputs
    let result ← ReactiveM.run events do
      runWidget do
        -- Use elapsed time from the reactive system
        let time ← useElapsedTime

        -- Create the render spec function that captures galaxyConfig
        let renderSpec := fun (viewState : GalaxyViewState) (t : Float) =>
          galaxySpecWithState galaxyConfig viewState t

        reactiveGalaxy hitInfos time renderSpec
    pure (result, inputs)
  ).run spiderEnv

  -- Main game loop
  while !(← canvas.shouldClose) do
    canvas.pollEvents

    -- Handle input for screen transitions
    if ← canvas.hasKeyPressed then
      canvas.clearKey
      if currentScreen == .title then
        currentScreen := .galaxy

    -- Calculate delta time
    let now ← IO.monoMsNow
    let dt := (now - lastTime).toFloat / 1000.0
    let t := (now - startTime).toFloat / 1000.0
    lastTime := now

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

      match currentScreen with
      | .title =>
        -- Build the starfield widget (GPU shader-based)
        let starfieldBuilder := Eschaton.Widget.starfieldWidget starfieldConfig t
        let starfieldWidget := Afferent.Arbor.buildFrom 1 starfieldBuilder

        -- Measure and layout the widget to fill the screen
        let measureResult ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget starfieldWidget currentW currentH)
        let layouts := Trellis.layout measureResult.node currentW currentH

        -- Collect render commands
        let (commands, _, _) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache measureResult.widget layouts

        -- Execute commands and render custom widgets
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
        -- Fire animation frame event to update time
        inputs.fireAnimationFrame dt

        -- Build the galaxy widget from the reactive render function
        let galaxyBuilder ← galaxyRender
        let galaxyWidgetTree := Afferent.Arbor.buildFrom 2 galaxyBuilder

        -- Measure and layout the galaxy widget
        let galaxyMeasure ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget galaxyWidgetTree currentW currentH)
        let galaxyLayouts := Trellis.layout galaxyMeasure.node currentW currentH

        -- Build the name map for hit testing
        let nameMap := buildNameMap galaxyMeasure.widget

        -- Get mouse state for hover/click events
        let (mouseX, mouseY) ← canvas.ctx.window.getMousePos
        let buttons ← canvas.ctx.window.getMouseButtons
        let leftDown := buttons &&& 1 != 0

        -- Fire hover event
        let hitPath := Afferent.Arbor.hitTestPath galaxyMeasure.widget galaxyLayouts mouseX mouseY
        inputs.fireHover {
          x := mouseX
          y := mouseY
          hitPath := hitPath
          widget := galaxyMeasure.widget
          layouts := galaxyLayouts
          nameMap := nameMap
        }

        -- Track mouse button state for click/mouseup events
        let wasLeftDown ← prevLeftDown.get
        if leftDown && !wasLeftDown then
          -- Mouse down - fire click
          inputs.fireClick {
            click := { button := 0, x := mouseX, y := mouseY, modifiers := 0 }
            hitPath := hitPath
            widget := galaxyMeasure.widget
            layouts := galaxyLayouts
            nameMap := nameMap
          }
        if !leftDown && wasLeftDown then
          -- Mouse up
          inputs.fireMouseUp {
            x := mouseX
            y := mouseY
            button := 0
            hitPath := hitPath
            widget := galaxyMeasure.widget
            layouts := galaxyLayouts
            nameMap := nameMap
          }
        prevLeftDown.set leftDown

        -- Handle scroll for zoom
        let (_, scrollY) ← canvas.ctx.window.getScrollDelta
        if scrollY != 0.0 then
          inputs.fireScroll {
            scroll := { x := mouseX, y := mouseY, deltaX := 0.0, deltaY := scrollY }
            hitPath := hitPath
            widget := galaxyMeasure.widget
            layouts := galaxyLayouts
            nameMap := nameMap
          }
          canvas.ctx.window.clearScroll

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
