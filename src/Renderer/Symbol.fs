module Symbol

open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


type Symbol =
    { Pos: XYPos
      LastDragPos: XYPos
      IsDragging: bool
      Id: CommonTypes.ComponentId
      Width: float
      Height: float
      Ports: Port list }


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId: CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId: CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId: CommonTypes.ComponentId
    | AddComponent of XYPos * float // used by demo code to add a circle
    | DeleteSymbol of sId: CommonTypes.ComponentId
    | UpdateSymbolModelWithComponent of CommonTypes.Component


//---------------------------------helper types and functions----------------//



let posDiff a b = { X = a.X - b.X; Y = a.Y - b.Y }

let posAdd a b = { X = a.X + b.X; Y = a.Y + b.Y }

let posOf x y = { X = x; Y = y }


/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (input: XYPos * float) =
    let pos, height = input
    let hostId = ComponentId(uuid ())

    { Pos = pos
      LastDragPos = { X = 0.; Y = 0. } // initial value can always be this
      IsDragging = false // initial value can always be this
      Id = hostId // create a unique id for this symbol
      Width = height
      Height = height
      Ports =
          [ // Creates one input and one output port. For demo only.
            { Id = ComponentId(uuid ())
              HostId = hostId
              PortType = PortType.Input
              Pos = fst input
              Width = 5
              IsHighlighted = false
              ParentHeight = height
              IsDragging = false }
            { Id = ComponentId(uuid ())
              HostId = hostId
              PortType = PortType.Output
              Pos = { fst input with X = pos.X + height }
              Width = 5
              IsHighlighted = false
              ParentHeight = height
              IsDragging = false } ] }


let init () =
    [ ({ X = 100.; Y = 100. }, 100.)
      ({ X = 500.; Y = 300. }, 100.)
      ({ X = 200.; Y = 500. }, 150.)
      ({ X = 500.; Y = 600. }, 80.) ]
    |> List.map createNewSymbol,
    Cmd.none

/// update function which displays symbols
let update (msg: Msg) (model: Model): Model * Cmd<'a> =
    match msg with
    | AddComponent (pos, height) -> createNewSymbol (pos, height) :: model, Cmd.none
    | DeleteSymbol sId -> List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                      LastDragPos = pagePos
                      IsDragging = true
                      Ports = List.map (fun port -> { port with IsDragging = true }) sym.Ports }),
        Cmd.none

    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos

                { sym with
                      Pos = posAdd sym.Pos diff
                      LastDragPos = pagePos
                      Ports = List.map (fun port -> { port with Pos = posAdd port.Pos diff }) sym.Ports }),
        Cmd.none

    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                      IsDragging = false }),
                      // Ports = List.map (fun port -> { port with IsDragging = false }) sym.Ports }),
        Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderComponentProps =
    { Component: Symbol // name works for the demo!
      Dispatch: Dispatch<Msg>
      key: string } // special field used by react to detect whether lists have changed, set to symbol Id

/// View for one symbol with caching for efficient execution when input does not change
let private renderComponent =
    FunctionComponent.Of
        ((fun (props: RenderComponentProps) ->
            let inputPorts =
                List.filter (fun port -> port.PortType = PortType.Input) props.Component.Ports

            let outputPorts =
                List.filter (fun port -> port.PortType = PortType.Output) props.Component.Ports

            // Dummy component has one input and one output
            let inputPort = inputPorts.Head
            let outputPort = outputPorts.Head

            let handleMouseMove =
                Hooks.useRef (fun (ev: Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Component.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch)

            let colour =
                if props.Component.IsDragging then "lightblue" else "grey"

            g [ OnMouseUp(fun _ ->
                    document.removeEventListener ("mousemove", handleMouseMove.current)
                    EndDragging props.Component.Id |> props.Dispatch)
                OnMouseDown(fun ev ->
                    StartDragging(props.Component.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch

                    document.addEventListener ("mousemove", handleMouseMove.current)) ] [
                polygon [ SVGAttr.Points
                              (sprintf
                                  "%0.2f,%0.2f %0.2f,%0.2f %0.2f,%0.2f %0.2f,%0.2f"
                                   (props.Component.Pos.X)
                                   (props.Component.Pos.Y
                                    - (props.Component.Height / 2.))
                                   (props.Component.Pos.X + props.Component.Height)
                                   (props.Component.Pos.Y
                                    - (props.Component.Height / 2.))
                                   (props.Component.Pos.X + props.Component.Height)
                                   (props.Component.Pos.Y
                                    + (props.Component.Height / 2.))
                                   (props.Component.Pos.X)
                                   (props.Component.Pos.Y
                                    + (props.Component.Height / 2.)))
                          SVGAttr.Fill colour
                          SVGAttr.Stroke colour
                          SVGAttr.StrokeWidth 1 ] []

                circle [ Cx inputPort.Pos.X
                         Cy inputPort.Pos.Y
                         R inputPort.Width
                         SVGAttr.Fill "darkgrey"
                         SVGAttr.Stroke "grey"
                         SVGAttr.StrokeWidth 1 ] []
                circle [ Cx outputPort.Pos.X
                         Cy outputPort.Pos.Y
                         R outputPort.Width
                         SVGAttr.Fill "darkgrey"
                         SVGAttr.Stroke "grey"
                         SVGAttr.StrokeWidth 1 ] []
            ]),
         "Component",
         equalsButFunctions)

/// View function for symbol layer of SVG
let view (model: Model) (dispatch: Msg -> unit) =
    model
    |> List.map (fun ({ Id = ComponentId id } as circle) ->
        renderComponent
            { Component = circle
              Dispatch = dispatch
              key = id })
    |> ofList


//---------------Other interface functions--------------------//

// let findPort (host: Symbol) (portId: ComponentId) : Port Option =
//     List.tryFind (fun port -> port.Id = portId) host.Ports
//
// let findSymbolWithPort (symModel: Model) (portId: ComponentId) : Port =
//     let host = List.find (fun symbol -> findPort symbol portId <> None) symModel
//     match findPort host portId with
//     | Some port -> port
//     | None -> failwithf "Host does not contain port"

let findPort (model: Model) (portId: ComponentId): Port =
    model
    |> List.collect (fun symbol -> symbol.Ports)
    |> List.filter (fun port -> port.Id = portId)
    |> List.head

/// Find a symbol such that its ports list contains a port with id=portId


/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp: CommonTypes.Component) = failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth (wId: CommonTypes.ConnectionId)
                         (outputPortNumber: int)
                         (inputPortWidths: int option list)
                         : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent (symModel: Model) (sId: CommonTypes.ComponentId): CommonTypes.Component =
    failwithf "Not implemented"

let extractComponents (symModel: Model): CommonTypes.Component list = failwithf "Not implemented"
