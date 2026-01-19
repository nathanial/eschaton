/-
  Reactive Galaxy Widget
  FRP-based galaxy rendering using Canopy's reactive widget system.
-/
import Afferent
import Afferent.Arbor
import Afferent.Canopy
import Tincture
import Trellis
import Eschaton.Widget.Galaxy.State
import Eschaton.Widget.Galaxy.HitTest

open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Reactive Reactive.Host
open Tincture (Color)

namespace Eschaton.Widget.Galaxy

/-- Internal input event for the galaxy widget, combining all input sources. -/
private inductive GalaxyInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp (data : MouseButtonData)
  | scroll (data : ScrollData)

/-- Result returned by the reactive galaxy widget. -/
structure GalaxyWidgetResult where
  /-- Reactive view state (pan, zoom, selection, etc.) -/
  viewState : Dynamic Spider GalaxyViewState
  /-- Event fired when a star is selected -/
  onStarSelect : Event Spider Nat
  /-- Event fired when selection is cleared -/
  onDeselectStar : Event Spider Unit

/-- Style for full-size widget that fills its container. -/
def fullSizeStyle : BoxStyle := { width := .percent 1.0, height := .percent 1.0 }

/-- Get layout rect for the galaxy widget. -/
private def getGalaxyRect (widget : Widget) (layouts : Trellis.LayoutResult)
    (name : String) : Option Trellis.LayoutRect :=
  match findWidgetIdByName widget name with
  | some wid =>
    match layouts.get wid with
    | some layout => some layout.contentRect
    | none => none
  | none => none

/-- Create a reactive galaxy widget.
    - `hitInfos`: Array of star hit info for hit testing (derived from systems)
    - `time`: Dynamic time value for animations
    - `renderSpec`: Function to create the CustomSpec given view state and time

    This function is generic over the specific config type - the caller provides:
    1. `hitInfos` derived from their star system data
    2. `renderSpec` function that knows how to render with their specific config
-/
def reactiveGalaxy (hitInfos : Array StarHitInfo)
    (time : Dynamic Spider Float)
    (renderSpec : GalaxyViewState → Float → CustomSpec)
    : WidgetM GalaxyWidgetResult := do
  -- Register component name for event routing
  let name ← registerComponentW "galaxy-view"

  -- Get all event hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp
  let scrollEvents ← useScroll name

  -- Convert raw events to unified GalaxyInputEvent stream
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM GalaxyInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM GalaxyInputEvent.hover allHovers)
  let mouseUpEvents ← liftSpider (Event.mapM GalaxyInputEvent.mouseUp allMouseUp)
  let scrollInputEvents ← liftSpider (Event.mapM GalaxyInputEvent.scroll scrollEvents)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents, scrollInputEvents])

  -- Create trigger events for star selection
  let (starSelectEvent, fireStarSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (deselectEvent, fireDeselect) ← newTriggerEvent (t := Spider) (a := Unit)

  -- Initial view state
  let initialState : GalaxyViewState := {}

  -- Fold input events into view state
  let viewState ← Reactive.foldDynM
    (fun (event : GalaxyInputEvent) (state : GalaxyViewState) => do
      match event with
      | .click clickData =>
        let x := clickData.click.x
        let y := clickData.click.y
        let widget := clickData.widget
        let layouts := clickData.layouts

        -- Check if click is on the galaxy widget
        if hitWidget clickData name then
          match getGalaxyRect widget layouts name with
          | some rect =>
            let params : TransformParams := {
              screenWidth := rect.width
              screenHeight := rect.height
              panX := state.panX
              panY := state.panY
              zoom := state.zoom
            }
            -- Adjust click position to be relative to widget
            let relX := x - rect.x
            let relY := y - rect.y

            -- Check if we clicked on a star
            match starAtPoint hitInfos params relX relY with
            | some starIdx =>
              fireStarSelect starIdx
              pure (applyInput (.selectStar starIdx) state)
            | none =>
              -- Start panning
              pure (applyInput (.panStart relX relY) state)
          | none => pure state
        else
          pure state

      | .hover hoverData =>
        let x := hoverData.x
        let y := hoverData.y
        let widget := hoverData.widget
        let layouts := hoverData.layouts

        if hitWidgetHover hoverData name then
          match getGalaxyRect widget layouts name with
          | some rect =>
            let params : TransformParams := {
              screenWidth := rect.width
              screenHeight := rect.height
              panX := state.panX
              panY := state.panY
              zoom := state.zoom
            }
            let relX := x - rect.x
            let relY := y - rect.y

            if state.isDragging then
              -- Continue panning
              pure (applyInput (.panMove relX relY) state)
            else
              -- Update hovered star
              let hoveredStar := starAtPoint hitInfos params relX relY
              pure (applyInput (.hoverStar hoveredStar) state)
          | none => pure state
        else
          -- Mouse left the galaxy widget
          if state.isDragging then
            pure (applyInput (.panEnd) state)
          else if state.hoveredStar.isSome then
            pure (applyInput (.hoverStar none) state)
          else
            pure state

      | .mouseUp _ =>
        if state.isDragging then
          pure (applyInput (.panEnd) state)
        else
          pure state

      | .scroll scrollData =>
        let widget := scrollData.widget
        let layouts := scrollData.layouts

        if hitWidgetScroll scrollData name then
          match getGalaxyRect widget layouts name with
          | some rect =>
            let relX := scrollData.scroll.x - rect.x
            let relY := scrollData.scroll.y - rect.y
            let delta := scrollData.scroll.deltaY
            pure (applyInput (.zoom delta relX relY) state)
          | none => pure state
        else
          pure state
    )
    initialState
    allInputEvents

  -- Combine view state with time for rendering
  let renderState ← Dynamic.zipWithM (fun vs t => (vs, t)) viewState time

  -- Use dynWidget to rebuild when state or time changes
  let _ ← dynWidget renderState fun (state, t) => do
    emit do pure (namedCustom name (renderSpec state t) fullSizeStyle)

  pure {
    viewState := viewState
    onStarSelect := starSelectEvent
    onDeselectStar := deselectEvent
  }

end Eschaton.Widget.Galaxy
