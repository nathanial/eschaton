/-
  Galaxy State Management
  View state and input events for the reactive galaxy widget.
-/
namespace Eschaton.Widget.Galaxy

/-- View state for the galaxy widget.
    Tracks pan, zoom, and interaction state reactively. -/
structure GalaxyViewState where
  /-- Viewport pan offset X in pixels -/
  panX : Float := 0.0
  /-- Viewport pan offset Y in pixels -/
  panY : Float := 0.0
  /-- Zoom level (1.0 = 100%) -/
  zoom : Float := 1.0
  /-- Index of the currently hovered star, if any -/
  hoveredStar : Option Nat := none
  /-- Index of the currently selected star, if any -/
  selectedStar : Option Nat := none
  /-- True when the user is dragging to pan -/
  isDragging : Bool := false
  /-- Mouse X position when drag started -/
  dragStartX : Float := 0.0
  /-- Mouse Y position when drag started -/
  dragStartY : Float := 0.0
  /-- Pan X value when drag started -/
  dragStartPanX : Float := 0.0
  /-- Pan Y value when drag started -/
  dragStartPanY : Float := 0.0
  deriving Inhabited, BEq

/-- Input events that can modify the galaxy view state. -/
inductive GalaxyInput where
  /-- Start a pan drag at the given screen coordinates -/
  | panStart (x y : Float)
  /-- Continue panning to the given screen coordinates -/
  | panMove (x y : Float)
  /-- End the current pan drag -/
  | panEnd
  /-- Zoom by the given delta, anchored at the given screen coordinates -/
  | zoom (delta : Float) (anchorX anchorY : Float)
  /-- Update the hovered star index -/
  | hoverStar (idx : Option Nat)
  /-- Select a star by index -/
  | selectStar (idx : Nat)
  /-- Deselect the current star -/
  | deselectStar
  deriving Inhabited, BEq

/-- Apply an input event to the view state to produce the new state. -/
def applyInput (input : GalaxyInput) (state : GalaxyViewState) : GalaxyViewState :=
  match input with
  | .panStart x y =>
    { state with
      isDragging := true
      dragStartX := x
      dragStartY := y
      dragStartPanX := state.panX
      dragStartPanY := state.panY
    }
  | .panMove x y =>
    if state.isDragging then
      let dx := x - state.dragStartX
      let dy := y - state.dragStartY
      { state with
        panX := state.dragStartPanX + dx
        panY := state.dragStartPanY + dy
      }
    else
      state
  | .panEnd =>
    { state with isDragging := false }
  | .zoom delta anchorX anchorY =>
    -- Zoom factor: positive delta = zoom in, negative = zoom out
    let zoomFactor := 1.0 + delta * 0.1
    let rawZoom := state.zoom * zoomFactor
    -- Clamp zoom between 0.1 and 10.0
    let newZoom := if rawZoom < 0.1 then 0.1 else if rawZoom > 10.0 then 10.0 else rawZoom
    -- Adjust pan so the point under the anchor stays fixed
    let scale := newZoom / state.zoom
    -- For zoom centered on anchor point:
    -- newPan = anchor - (anchor - oldPan) * scale
    let newPanX := anchorX - (anchorX - state.panX) * scale
    let newPanY := anchorY - (anchorY - state.panY) * scale
    { state with
      zoom := newZoom
      panX := newPanX
      panY := newPanY
    }
  | .hoverStar idx =>
    { state with hoveredStar := idx }
  | .selectStar idx =>
    { state with selectedStar := some idx }
  | .deselectStar =>
    { state with selectedStar := none }

end Eschaton.Widget.Galaxy
