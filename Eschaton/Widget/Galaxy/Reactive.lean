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

/-- Build transform params from layout rect and view state. -/
private def mkTransformParams (rect : Trellis.LayoutRect) (state : GalaxyViewState)
    : TransformParams :=
  { screenWidth := rect.width
    screenHeight := rect.height
    panX := state.panX
    panY := state.panY
    zoom := state.zoom }

/-- Result of a click on the galaxy widget. -/
private inductive ClickResult where
  | starSelected (idx : Nat) (state : GalaxyViewState)
  | panStarted (state : GalaxyViewState)
  | noHit

/-- Process a click at the given position. Returns the result without side effects. -/
private def processClick (hitInfos : Array StarHitInfo) (rect : Trellis.LayoutRect)
    (clickX clickY : Float) (state : GalaxyViewState) : ClickResult :=
  let params := mkTransformParams rect state
  let relX := clickX - rect.x
  let relY := clickY - rect.y
  match starAtPoint hitInfos params relX relY with
  | some starIdx => .starSelected starIdx (applyInput (.selectStar starIdx) state)
  | none => .panStarted (applyInput (.panStart relX relY) state)

/-- Process hover movement inside the galaxy widget. -/
private def processHoverInside (hitInfos : Array StarHitInfo) (rect : Trellis.LayoutRect)
    (hoverX hoverY : Float) (state : GalaxyViewState) : GalaxyViewState :=
  let params := mkTransformParams rect state
  let relX := hoverX - rect.x
  let relY := hoverY - rect.y
  if state.isDragging then
    applyInput (.panMove relX relY) state
  else
    let hoveredStar := starAtPoint hitInfos params relX relY
    applyInput (.hoverStar hoveredStar) state

/-- Process hover when mouse leaves the galaxy widget. -/
private def processHoverOutside (state : GalaxyViewState) : GalaxyViewState :=
  if state.isDragging then
    applyInput .panEnd state
  else if state.hoveredStar.isSome then
    applyInput (.hoverStar none) state
  else
    state

/-- Process scroll/zoom at the given position. -/
private def processScroll (rect : Trellis.LayoutRect) (scrollX scrollY delta : Float)
    (state : GalaxyViewState) : GalaxyViewState :=
  let centerX := rect.width / 2.0
  let centerY := rect.height / 2.0
  let relX := scrollX - rect.x - centerX
  let relY := scrollY - rect.y - centerY
  applyInput (.zoom delta relX relY) state

/-- Merge all input event sources into a unified stream. -/
private def mergeInputEvents (allClicks : Event Spider ClickData)
    (allHovers : Event Spider HoverData) (allMouseUp : Event Spider MouseButtonData)
    (scrollEvents : Event Spider ScrollData) : SpiderM (Event Spider GalaxyInputEvent) := do
  let clickEvents ← Event.mapM GalaxyInputEvent.click allClicks
  let hoverEvents ← Event.mapM GalaxyInputEvent.hover allHovers
  let mouseUpEvents ← Event.mapM GalaxyInputEvent.mouseUp allMouseUp
  let scrollInputEvents ← Event.mapM GalaxyInputEvent.scroll scrollEvents
  Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents, scrollInputEvents]

/-- Build the reactive view state from input events.
    Returns the view state Dynamic and fires star selection events. -/
private def buildViewState (hitInfos : Array StarHitInfo) (name : String)
    (fireStarSelect : Nat → IO Unit) (inputEvents : Event Spider GalaxyInputEvent)
    : WidgetM (Dynamic Spider GalaxyViewState) :=
  Reactive.foldDynM
    (fun (event : GalaxyInputEvent) (state : GalaxyViewState) => do
      match event with
      | .click clickData =>
        if hitWidget clickData name then
          match getGalaxyRect clickData.widget clickData.layouts name with
          | some rect =>
            match processClick hitInfos rect clickData.click.x clickData.click.y state with
            | .starSelected idx newState =>
              fireStarSelect idx
              pure newState
            | .panStarted newState => pure newState
            | .noHit => pure state
          | none => pure state
        else
          pure state

      | .hover hoverData =>
        if hitWidgetHover hoverData name then
          match getGalaxyRect hoverData.widget hoverData.layouts name with
          | some rect =>
            pure (processHoverInside hitInfos rect hoverData.x hoverData.y state)
          | none => pure state
        else
          pure (processHoverOutside state)

      | .mouseUp _ =>
        if state.isDragging then
          pure (applyInput .panEnd state)
        else
          pure state

      | .scroll scrollData =>
        if hitWidgetScroll scrollData name then
          match getGalaxyRect scrollData.widget scrollData.layouts name with
          | some rect =>
            pure (processScroll rect scrollData.scroll.x scrollData.scroll.y
                    scrollData.scroll.deltaY state)
          | none => pure state
        else
          pure state
    )
    ({} : GalaxyViewState)
    inputEvents

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
  let name ← registerComponentW "galaxy-view"

  -- Get all event hooks and merge into unified stream
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp
  let scrollEvents ← useScroll name
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let allInputEvents ← liftSpider (mergeInputEvents allClicks allHovers allMouseUp scrollEvents)

  -- Create trigger events for star selection
  let (starSelectEvent, fireStarSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (deselectEvent, _fireDeselect) ← newTriggerEvent (t := Spider) (a := Unit)

  -- Build reactive view state from input events
  let viewState ← buildViewState hitInfos name fireStarSelect allInputEvents

  -- Combine view state with time for rendering
  let renderState ← Dynamic.zipWithM (fun vs t => (vs, t)) viewState time

  -- Rebuild widget when state or time changes
  let _ ← dynWidget renderState fun (state, t) => do
    emit do pure (namedCustom name (renderSpec state t) fullSizeStyle)

  pure {
    viewState := viewState
    onStarSelect := starSelectEvent
    onDeselectStar := deselectEvent
  }

end Eschaton.Widget.Galaxy
