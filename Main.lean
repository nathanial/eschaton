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
open Eschaton.Widget (StarfieldConfig starfieldWidget ProvinceMapStaticConfig provinceMapSpecWithState
  Province toProvinceHitInfoArray)
open Eschaton.Widget.ProvinceMap (ProvinceMapViewState reactiveProvinceMap)

namespace Eschaton

/-- Game screens. -/
inductive Screen where
  | title
  | galaxy
  deriving BEq, Inhabited

/-- Generate provinces using Voronoi tessellation for perfect tiling. -/
def sampleProvinceMapConfig : ProvinceMapStaticConfig :=
  -- Generate 24 provinces with Voronoi tessellation
  let provinces := generateDefaultProvinces 24 42
  {
    provinces := provinces
    backgroundColor := Color.rgb 0.15 0.2 0.3  -- Ocean blue
    borderWidth := 1.5
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
  let provinceMapConfig := { Eschaton.sampleProvinceMapConfig with labelFont := some debugFontId }
  let hitInfos := toProvinceHitInfoArray provinceMapConfig.provinces

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

  -- Run the FRP setup to create the reactive province map widget
  let ((_provinceMapResult, provinceMapRender), inputs) ← (do
    let (events, inputs) ← createInputs
    let result ← ReactiveM.run events do
      runWidget do
        -- Create the render spec function that captures provinceMapConfig
        let renderSpec := fun (viewState : ProvinceMapViewState) =>
          provinceMapSpecWithState provinceMapConfig viewState

        reactiveProvinceMap hitInfos renderSpec
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

        -- Build the province map widget from the reactive render function
        let provinceMapBuilder ← provinceMapRender
        let provinceMapWidgetTree := Afferent.Arbor.buildFrom 2 provinceMapBuilder

        -- Measure and layout the province map widget
        let provinceMapMeasure ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget provinceMapWidgetTree currentW currentH)
        let provinceMapLayouts := Trellis.layout provinceMapMeasure.node currentW currentH

        -- Build the name map for hit testing
        let nameMap := buildNameMap provinceMapMeasure.widget

        -- Get mouse state for hover/click events
        let (mouseX, mouseY) ← canvas.ctx.window.getMousePos
        let buttons ← canvas.ctx.window.getMouseButtons
        let leftDown := buttons &&& 1 != 0

        -- Fire hover event
        let hitPath := Afferent.Arbor.hitTestPath provinceMapMeasure.widget provinceMapLayouts mouseX mouseY
        inputs.fireHover {
          x := mouseX
          y := mouseY
          hitPath := hitPath
          widget := provinceMapMeasure.widget
          layouts := provinceMapLayouts
          nameMap := nameMap
        }

        -- Track mouse button state for click/mouseup events
        let wasLeftDown ← prevLeftDown.get
        if leftDown && !wasLeftDown then
          -- Mouse down - fire click
          inputs.fireClick {
            click := { button := 0, x := mouseX, y := mouseY, modifiers := 0 }
            hitPath := hitPath
            widget := provinceMapMeasure.widget
            layouts := provinceMapLayouts
            nameMap := nameMap
          }
        if !leftDown && wasLeftDown then
          -- Mouse up
          inputs.fireMouseUp {
            x := mouseX
            y := mouseY
            button := 0
            hitPath := hitPath
            widget := provinceMapMeasure.widget
            layouts := provinceMapLayouts
            nameMap := nameMap
          }
        prevLeftDown.set leftDown

        -- Handle scroll for zoom
        let (_, scrollY) ← canvas.ctx.window.getScrollDelta
        if scrollY != 0.0 then
          inputs.fireScroll {
            scroll := { x := mouseX, y := mouseY, deltaX := 0.0, deltaY := scrollY }
            hitPath := hitPath
            widget := provinceMapMeasure.widget
            layouts := provinceMapLayouts
            nameMap := nameMap
          }
          canvas.ctx.window.clearScroll

        -- Collect and execute province map render commands
        let (provinceMapCommands, _, _) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache provinceMapMeasure.widget provinceMapLayouts

        canvas ← CanvasM.run' canvas do
          Afferent.Widget.executeCommandsBatched fontRegistry provinceMapCommands
          Afferent.Widget.renderCustomWidgets provinceMapMeasure.widget provinceMapLayouts

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
