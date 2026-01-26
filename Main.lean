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
import Std.Internal.Async.Process

open Afferent Afferent.FFI
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Reactive Reactive.Host Reactive.Host.Spider
open Linalg
open Eschaton (generateDefaultProvinces)
open Eschaton.Widget (StarfieldConfig starfieldWidget ProvinceMapStaticConfig provinceMapSpecWithState
  Province toProvinceHitInfoArray)
open Eschaton.Widget.ProvinceMap (ProvinceMapViewState reactiveProvinceMap)

namespace Eschaton

/-- Game screens. -/
inductive Screen where
  | title
  | galaxy
  deriving BEq, Inhabited

/-- Footer bar height in logical pixels. -/
def footerBarHeight : Float := 90

/-- Build a performance stats footer widget. -/
def buildFooterWidget (startId : Nat) (fontId : Afferent.Arbor.FontId)
    (screenScale : Float) (line1 line2 line3 line4 : String) : Afferent.Arbor.Widget :=
  Afferent.Arbor.buildFrom startId do
    let s := fun (v : Float) => v * screenScale
    let outerStyle : Afferent.Arbor.BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := Trellis.EdgeInsets.symmetric (s 8) (s 4)
      height := .length (s footerBarHeight)
      flexItem := some (Trellis.FlexItem.fixed (s footerBarHeight))
    }
    let rowStyle : Afferent.Arbor.BoxStyle := {
      width := .percent 1.0
    }
    Afferent.Arbor.flexColumn (Trellis.FlexContainer.column (s 2)) outerStyle #[
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line1 fontId (Color.gray 0.7) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line2 fontId (Color.gray 0.65) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line3 fontId (Color.gray 0.6) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line4 fontId (Color.gray 0.55) .left none
      ]
    ]

/-- Get next available widget ID after a widget tree. -/
def nextWidgetId (w : Afferent.Arbor.Widget) : Nat :=
  (Afferent.Arbor.Widget.allIds w).foldl (fun acc wid => max acc wid) 0 + 1

/-- Wrap content widget with footer in a flex column. -/
def wrapWithFooter (content : Afferent.Arbor.Widget) (fontId : Afferent.Arbor.FontId)
    (screenScale : Float) (line1 line2 line3 line4 : String) : Afferent.Arbor.Widget :=
  let footerStartId := nextWidgetId content
  let footer := buildFooterWidget footerStartId fontId screenScale line1 line2 line3 line4
  .flex 0 none (Trellis.FlexContainer.column 0)
    { width := .percent 1.0, height := .percent 1.0 }
    #[content, footer]

/-- Generate provinces using Voronoi tessellation for perfect tiling. -/
def sampleProvinceMapConfig : ProvinceMapStaticConfig :=
  -- Generate 24 provinces with Voronoi tessellation
  let provinces := generateDefaultProvinces 2000 42
  {
    provinces := provinces
    backgroundColor := Color.rgb 0.15 0.2 0.3  -- Ocean blue
    borderWidth := 1.5
  }

private structure ProvinceGenState where
  seed : Nat
  relaxations : Nat
  provinces : Array Widget.Province
deriving Inhabited

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
  let provinceLabelFont ← Afferent.Font.load "/System/Library/Fonts/Monaco.ttf" (7 * screenScale).toUInt32

  -- Create font registry for Arbor
  let (fontRegistry, debugFontId) := FontRegistry.empty.register debugFont "debug"
  let (fontRegistry, provinceLabelFontId) := fontRegistry.register provinceLabelFont "provinceLabel"

  -- Province generation defaults
  let provinceCount : Nat := 2000
  let initialSeed : Nat := 42
  let initialRelaxations : Int := 0

  -- Initialize FRP environment
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let startTime ← IO.monoMsNow
  let mut frameCount : Nat := 0
  let mut displayFps : Float := 0.0
  let mut fpsAccumulator : Float := 0.0
  let mut lastTime := startTime

  -- Performance tracking state
  let mut batchStats : Afferent.Widget.BatchStats := {}
  let mut cacheHits : Nat := 0
  let mut cacheMisses : Nat := 0
  let mut widgetCount : Nat := 0
  let mut commandCount : Nat := 0
  let mut peakRssKb : UInt64 := 0
  -- Timing stats (in milliseconds)
  let mut timeUpdateMs : Float := 0.0
  let mut timeBuildMs : Float := 0.0
  let mut timeLayoutMs : Float := 0.0
  let mut timeCollectMs : Float := 0.0
  let mut timeGpuMs : Float := 0.0
  let mut lastPresentMs : Float := 0.0
  let mut lastWorkEndTime := startTime

  -- Screen state (not in FRP - simple state machine for title/galaxy transition)
  let mut currentScreen : Eschaton.Screen := .title

  -- Starfield config for title screen
  let starfieldConfig : StarfieldConfig := {}

  -- Track mouse button state for click/mouseup (manual since Window doesn't have this built-in)
  let prevLeftDown ← IO.mkRef false

  -- Run the FRP setup to create the reactive UI
  let theme : Theme := { Theme.dark with font := debugFontId, smallFont := debugFontId }
  let ((_uiResult, uiRender), inputs) ← (do
    let (events, inputs) ← createInputs fontRegistry theme
    let result ← ReactiveM.run events do
      runWidget do
        let stepperConfig : StepperConfig := {
          min := 0
          max := 10
          step := 1
          width := 140
          height := 32
          buttonWidth := 32
          cornerRadius := theme.cornerRadius
        }

        let initialRelaxationsNat := Int.toNat initialRelaxations
        let initialProvinces := generateDefaultProvinces provinceCount initialSeed initialRelaxationsNat
        let initialState : Eschaton.ProvinceGenState := {
          seed := initialSeed
          relaxations := initialRelaxationsNat
          provinces := initialProvinces
        }

        let rootStyle : Afferent.Arbor.BoxStyle := {
          width := .percent 1.0
          height := .percent 1.0
          backgroundColor := some (Color.gray 0.05)
        }

        flexRow' { Trellis.FlexContainer.row 0 with alignItems := .stretch } (style := rootStyle) do
          let sidebarStyle : Afferent.Arbor.BoxStyle := {
            width := .length 500
            minWidth := .some 500
            maxWidth := .some 500
            height := .percent 1.0
            padding := Trellis.EdgeInsets.uniform 16
            backgroundColor := some (Color.gray 0.12)
            borderColor := some (Color.gray 0.2)
            borderWidth := 1
            flexItem := some (Trellis.FlexItem.fixed 500)
          }

          let contentStyle : Afferent.Arbor.BoxStyle := {
            width := .percent 1.0
            height := .percent 1.0
            flexItem := some (Trellis.FlexItem.growing 1)
          }

          let (lloydStepper, regenClick) ← column' (gap := 12) (style := sidebarStyle) do
            heading3' "Province Generator"
            caption' "Lloyd relaxations"
            let stepperResult ← stepper initialRelaxations stepperConfig
            let _ ← dynWidget stepperResult.value fun value =>
              caption' s!"Relaxations: {value}"
            let regenClick ← button "Regenerate" .primary
            pure (stepperResult, regenClick)

          let stateDyn ← Reactive.foldDynM
            (fun _ (state : Eschaton.ProvinceGenState) => SpiderM.liftIO do
              let relaxInt ← lloydStepper.value.sample
              let relaxNat := Int.toNat relaxInt
              let newSeed := state.seed + 1
              let provinces := generateDefaultProvinces provinceCount newSeed relaxNat
              pure (⟨newSeed, relaxNat, provinces⟩ : Eschaton.ProvinceGenState)
            )
            initialState
            regenClick

          column' (gap := 0) (style := contentStyle) do
            let _ ← dynWidget stateDyn fun state => do
              let mapConfig : ProvinceMapStaticConfig := {
                provinces := state.provinces
                backgroundColor := Color.rgb 0.15 0.2 0.3
                borderWidth := 1.5
                labelFont := some provinceLabelFontId
              }
              let hitInfos := toProvinceHitInfoArray state.provinces
              let renderSpec := fun (viewState : ProvinceMapViewState) =>
                provinceMapSpecWithState mapConfig viewState
              let _ ← reactiveProvinceMap hitInfos renderSpec
              pure ()
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

    -- FPS and memory calculation (every 30 frames)
    frameCount := frameCount + 1
    if dt > 0.0 then
      fpsAccumulator := fpsAccumulator + (1.0 / dt)
    if frameCount >= 30 then
      displayFps := fpsAccumulator / frameCount.toFloat
      fpsAccumulator := 0.0
      frameCount := 0
      -- Update memory stats
      let usage ← Std.Internal.IO.Process.getResourceUsage
      peakRssKb := usage.peakResidentSetSizeKb

    -- Begin frame with deep space background
    let ok ← canvas.beginFrame (Color.rgba 0.02 0.02 0.06 1.0)

    -- Track present time (time waiting in beginFrame for vsync)
    let afterBeginFrame ← IO.monoMsNow
    lastPresentMs := (afterBeginFrame - lastWorkEndTime).toFloat

    if ok then
      -- Get current window size
      let (currentW, currentH) ← canvas.ctx.getCurrentSize

      -- Compute footer lines from previous frame's stats
      let memMb : UInt64 := peakRssKb / 1024
      let cacheTotal := cacheHits + cacheMisses
      let cacheRate := if cacheTotal > 0 then (cacheHits * 100) / cacheTotal else 0
      let totalDrawCalls := batchStats.batchedCalls + batchStats.individualCalls
      let fmt := fun (v : Float) => s!"{(v * 10).toUInt32.toFloat / 10}"

      let footerLine1 :=
        s!"FPS: {displayFps.toUInt32}  |  Commands: {commandCount}  |  Widgets: {widgetCount}  |  Mem: {memMb}MB  |  Cache: {cacheRate}%"
      let totalBatched := batchStats.rectsBatched + batchStats.circlesBatched + batchStats.strokeRectsBatched + batchStats.linesBatched + batchStats.textsBatched
      let avgBatchSize := if batchStats.batchedCalls > 0 then (totalBatched * 10 / batchStats.batchedCalls).toFloat / 10.0 else 0.0
      let avgBatchStr := s!"{(avgBatchSize * 10).toUInt32.toFloat / 10}"
      let batchBreakdown := s!"R:{batchStats.rectsBatched} C:{batchStats.circlesBatched} SR:{batchStats.strokeRectsBatched} L:{batchStats.linesBatched} T:{batchStats.textsBatched}"
      let footerLine2 :=
        s!"Draw: {totalDrawCalls} (Batched: {batchStats.batchedCalls}, Avg: {avgBatchStr})  |  {batchBreakdown}"
      let footerLine3 :=
        s!"Timing: Update {fmt timeUpdateMs}ms, Build {fmt timeBuildMs}ms, Layout {fmt timeLayoutMs}ms, Collect {fmt timeCollectMs}ms, GPU {fmt timeGpuMs}ms, Present {fmt lastPresentMs}ms"
      let footerLine4 := s!"GPU Detail: Flatten {fmt batchStats.timeFlattenMs}ms, Coalesce {fmt batchStats.timeCoalesceMs}ms, BatchLoop {fmt batchStats.timeBatchLoopMs}ms, DrawCalls {fmt batchStats.timeDrawCallsMs}ms"

      match currentScreen with
      | .title =>
        -- Timing: Build phase
        let tBuild0 ← IO.monoMsNow
        let starfieldBuilder := Eschaton.Widget.starfieldWidget starfieldConfig t
        let starfieldContent := Afferent.Arbor.buildFrom 1 starfieldBuilder
        -- Wrap content with footer
        let starfieldWidget := Eschaton.wrapWithFooter starfieldContent debugFontId screenScale
          footerLine1 footerLine2 footerLine3 footerLine4

        -- Timing: Measure/Layout phase
        let tLayout0 ← IO.monoMsNow
        let measureResult ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget starfieldWidget currentW currentH)
        let layouts := Trellis.layout measureResult.node currentW currentH
        let tLayout1 ← IO.monoMsNow

        -- Timing: Collect phase
        let tCollect0 ← IO.monoMsNow
        let (commands, hits, misses) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache measureResult.widget layouts
        let tCollect1 ← IO.monoMsNow

        -- Timing: GPU phase - Execute commands and render custom widgets
        let tGpu0 ← IO.monoMsNow
        let (newBatchStats, newCanvas) ← CanvasM.run canvas do
          let stats ← Afferent.Widget.executeCommandsBatchedWithStats fontRegistry commands
          Afferent.Widget.renderCustomWidgets measureResult.widget layouts
          return stats
        canvas := newCanvas
        let tGpu1 ← IO.monoMsNow

        -- Update stats
        timeBuildMs := (tLayout0 - tBuild0).toFloat
        timeLayoutMs := (tLayout1 - tLayout0).toFloat
        timeCollectMs := (tCollect1 - tCollect0).toFloat
        timeGpuMs := (tGpu1 - tGpu0).toFloat
        cacheHits := hits
        cacheMisses := misses
        commandCount := commands.size
        widgetCount := Afferent.Arbor.Widget.widgetCount measureResult.widget
        batchStats := newBatchStats

        -- Title screen: show title text over starfield (in content area, excluding footer)
        let footerHeightPx := Eschaton.footerBarHeight * screenScale
        let contentH := currentH - footerHeightPx
        canvas ← CanvasM.run' canvas do
          -- Draw title (centered in content area)
          let titleText := "ESCHATON"
          let (titleWidth, _) ← CanvasM.measureText titleText titleFont
          let titleX := (currentW - titleWidth) / 2.0
          let titleY := contentH * 0.5
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
          let promptY := contentH * 0.75
          let promptAlpha := 0.4 + 0.4 * Float.sin (t * 2.0)
          CanvasM.fillTextColor promptText ⟨promptX, promptY⟩ debugFont (Color.rgba 0.6 0.6 0.7 promptAlpha)

      | .galaxy =>
        -- Timing: Update phase (FRP propagation)
        let tUpdate0 ← IO.monoMsNow
        inputs.fireAnimationFrame dt
        let tUpdate1 ← IO.monoMsNow

        -- Timing: Build phase
        let tBuild0 ← IO.monoMsNow
        let uiBuilder ← uiRender
        let uiContent := Afferent.Arbor.buildFrom 2 uiBuilder
        -- Wrap content with footer
        let provinceMapWidgetTree := Eschaton.wrapWithFooter uiContent debugFontId screenScale
          footerLine1 footerLine2 footerLine3 footerLine4

        -- Timing: Measure/Layout phase
        let tLayout0 ← IO.monoMsNow
        let provinceMapMeasure ← runWithFonts fontRegistry
          (Afferent.Arbor.measureWidget provinceMapWidgetTree currentW currentH)
        let provinceMapLayouts := Trellis.layout provinceMapMeasure.node currentW currentH
        let tLayout1 ← IO.monoMsNow

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

        -- Timing: Collect phase
        let tCollect0 ← IO.monoMsNow
        let (provinceMapCommands, hits, misses) ←
          Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache provinceMapMeasure.widget provinceMapLayouts
        let tCollect1 ← IO.monoMsNow

        -- Timing: GPU phase
        let tGpu0 ← IO.monoMsNow
        let (newBatchStats, newCanvas) ← CanvasM.run canvas do
          let stats ← Afferent.Widget.executeCommandsBatchedWithStats fontRegistry provinceMapCommands
          Afferent.Widget.renderCustomWidgets provinceMapMeasure.widget provinceMapLayouts
          return stats
        canvas := newCanvas
        let tGpu1 ← IO.monoMsNow

        -- Update stats for galaxy view
        timeUpdateMs := (tUpdate1 - tUpdate0).toFloat
        timeBuildMs := (tLayout0 - tBuild0).toFloat
        timeLayoutMs := (tLayout1 - tLayout0).toFloat
        timeCollectMs := (tCollect1 - tCollect0).toFloat
        timeGpuMs := (tGpu1 - tGpu0).toFloat
        cacheHits := hits
        cacheMisses := misses
        commandCount := provinceMapCommands.size
        widgetCount := Afferent.Arbor.Widget.widgetCount provinceMapMeasure.widget
        batchStats := newBatchStats

      -- Track work end time for present calculation
      lastWorkEndTime ← IO.monoMsNow

      canvas ← canvas.endFrame

  -- Cleanup
  IO.println "Cleaning up..."
  titleFont.destroy
  subtitleFont.destroy
  debugFont.destroy
  provinceLabelFont.destroy
  canvas.destroy
  IO.println "Done!"
