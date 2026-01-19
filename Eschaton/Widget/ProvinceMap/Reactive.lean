/-
  Reactive Province Map Widget
  FRP-based province map rendering using Canopy's reactive widget system.
-/
import Afferent
import Afferent.Arbor
import Afferent.Canopy
import Tincture
import Trellis
import Eschaton.Widget.ProvinceMap.State
import Eschaton.Widget.ProvinceMap.HitTest

open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Reactive Reactive.Host
open Tincture (Color)

namespace Eschaton.Widget.ProvinceMap

/-- Internal input event for the province map widget, combining all input sources. -/
private inductive ProvinceMapInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp (data : MouseButtonData)
  | scroll (data : ScrollData)

/-- Result returned by the reactive province map widget. -/
structure ProvinceMapWidgetResult where
  /-- Reactive view state (pan, zoom, selection, etc.) -/
  viewState : Dynamic Spider ProvinceMapViewState
  /-- Event fired when a province is selected -/
  onProvinceSelect : Event Spider Nat
  /-- Event fired when selection is cleared -/
  onDeselectProvince : Event Spider Unit

/-- Style for full-size widget that fills its container. -/
def fullSizeStyle : BoxStyle := { width := .percent 1.0, height := .percent 1.0 }

/-- Get layout rect for the province map widget. -/
private def getProvinceMapRect (widget : Widget) (layouts : Trellis.LayoutResult)
    (name : String) : Option Trellis.LayoutRect :=
  match findWidgetIdByName widget name with
  | some wid =>
    match layouts.get wid with
    | some layout => some layout.contentRect
    | none => none
  | none => none

/-- Build transform params from layout rect and view state. -/
private def mkTransformParams (rect : Trellis.LayoutRect) (state : ProvinceMapViewState)
    : TransformParams :=
  { screenWidth := rect.width
    screenHeight := rect.height
    panX := state.panX
    panY := state.panY
    zoom := state.zoom }

/-- Result of a click on the province map widget. -/
private inductive ClickResult where
  | provinceSelected (idx : Nat) (state : ProvinceMapViewState)
  | panStarted (state : ProvinceMapViewState)
  | noHit

/-- Process a click at the given position. Returns the result without side effects. -/
private def processClick (hitInfos : Array ProvinceHitInfo) (rect : Trellis.LayoutRect)
    (clickX clickY : Float) (state : ProvinceMapViewState) : ClickResult :=
  let params := mkTransformParams rect state
  let relX := clickX - rect.x
  let relY := clickY - rect.y
  match provinceAtPoint hitInfos params relX relY with
  | some provinceIdx => .provinceSelected provinceIdx (applyInput (.selectProvince provinceIdx) state)
  | none => .panStarted (applyInput (.panStart relX relY) state)

/-- Process hover movement inside the province map widget. -/
private def processHoverInside (hitInfos : Array ProvinceHitInfo) (rect : Trellis.LayoutRect)
    (hoverX hoverY : Float) (state : ProvinceMapViewState) : ProvinceMapViewState :=
  let params := mkTransformParams rect state
  let relX := hoverX - rect.x
  let relY := hoverY - rect.y
  if state.isDragging then
    applyInput (.panMove relX relY) state
  else
    let hoveredProvince := provinceAtPoint hitInfos params relX relY
    applyInput (.hoverProvince hoveredProvince) state

/-- Process hover when mouse leaves the province map widget. -/
private def processHoverOutside (state : ProvinceMapViewState) : ProvinceMapViewState :=
  if state.isDragging then
    applyInput .panEnd state
  else if state.hoveredProvince.isSome then
    applyInput (.hoverProvince none) state
  else
    state

/-- Process scroll/zoom at the given position. -/
private def processScroll (rect : Trellis.LayoutRect) (scrollX scrollY delta : Float)
    (state : ProvinceMapViewState) : ProvinceMapViewState :=
  let centerX := rect.width / 2.0
  let centerY := rect.height / 2.0
  let relX := scrollX - rect.x - centerX
  let relY := scrollY - rect.y - centerY
  applyInput (.zoom delta relX relY) state

/-- Merge all input event sources into a unified stream. -/
private def mergeInputEvents (allClicks : Event Spider ClickData)
    (allHovers : Event Spider HoverData) (allMouseUp : Event Spider MouseButtonData)
    (scrollEvents : Event Spider ScrollData) : SpiderM (Event Spider ProvinceMapInputEvent) := do
  let clickEvents ← Event.mapM ProvinceMapInputEvent.click allClicks
  let hoverEvents ← Event.mapM ProvinceMapInputEvent.hover allHovers
  let mouseUpEvents ← Event.mapM ProvinceMapInputEvent.mouseUp allMouseUp
  let scrollInputEvents ← Event.mapM ProvinceMapInputEvent.scroll scrollEvents
  Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents, scrollInputEvents]

/-- Build the reactive view state from input events.
    Returns the view state Dynamic and fires province selection events. -/
private def buildViewState (hitInfos : Array ProvinceHitInfo) (name : String)
    (fireProvinceSelect : Nat → IO Unit) (inputEvents : Event Spider ProvinceMapInputEvent)
    : WidgetM (Dynamic Spider ProvinceMapViewState) :=
  Reactive.foldDynM
    (fun (event : ProvinceMapInputEvent) (state : ProvinceMapViewState) => do
      match event with
      | .click clickData =>
        if hitWidget clickData name then
          match getProvinceMapRect clickData.widget clickData.layouts name with
          | some rect =>
            match processClick hitInfos rect clickData.click.x clickData.click.y state with
            | .provinceSelected idx newState =>
              fireProvinceSelect idx
              pure newState
            | .panStarted newState => pure newState
            | .noHit => pure state
          | none => pure state
        else
          pure state

      | .hover hoverData =>
        if hitWidgetHover hoverData name then
          match getProvinceMapRect hoverData.widget hoverData.layouts name with
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
          match getProvinceMapRect scrollData.widget scrollData.layouts name with
          | some rect =>
            pure (processScroll rect scrollData.scroll.x scrollData.scroll.y
                    scrollData.scroll.deltaY state)
          | none => pure state
        else
          pure state
    )
    ({} : ProvinceMapViewState)
    inputEvents

/-- Create a reactive province map widget.
    - `hitInfos`: Array of province hit info for hit testing (derived from provinces)
    - `renderSpec`: Function to create the CustomSpec given view state

    This function is generic over the specific config type - the caller provides:
    1. `hitInfos` derived from their province data
    2. `renderSpec` function that knows how to render with their specific config
-/
def reactiveProvinceMap (hitInfos : Array ProvinceHitInfo)
    (renderSpec : ProvinceMapViewState → CustomSpec)
    : WidgetM ProvinceMapWidgetResult := do
  let name ← registerComponentW "province-map-view"

  -- Get all event hooks and merge into unified stream
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp
  let scrollEvents ← useScroll name
  let allInputEvents ← mergeInputEvents allClicks allHovers allMouseUp scrollEvents

  -- Create trigger events for province selection
  let (provinceSelectEvent, fireProvinceSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (deselectEvent, _fireDeselect) ← newTriggerEvent (t := Spider) (a := Unit)

  -- Build reactive view state from input events
  let viewState ← buildViewState hitInfos name fireProvinceSelect allInputEvents

  -- Rebuild widget when state changes
  let _ ← dynWidget viewState fun state => do
    emit do pure (namedCustom name (renderSpec state) fullSizeStyle)

  pure {
    viewState := viewState
    onProvinceSelect := provinceSelectEvent
    onDeselectProvince := deselectEvent
  }

end Eschaton.Widget.ProvinceMap
