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

type Port =
    { Id: PortId
      HostId: ComponentId
      PortType: PortType
      Pos: XYPos
      BoundingBox: BoundingBox
      Width: int
      IsHighlighted: bool
      IsDragging: bool
      ParentHeight: float }

type Symbol =
    { Id: ComponentId
      BoundingBox: BoundingBox
      Ports: Port list
      Pos: XYPos
      LastDragPos: XYPos
      IsShowingPorts: bool
      IsHighlighted: bool
      IsTransparent: bool
      NoOfInputPorts: int
      NoOfOutputPorts: int
      ComponentType: ComponentType
      Label: string
      ShowingPortsType: PortType option }


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

type Msg =
    | ShowPorts of symbols: ComponentId list * PortType option
    | HidePorts of sIds: ComponentId list
    | StartDragging of sIds: ComponentId list * pagePos: XYPos
    | Dragging of sIds: ComponentId list * pagePos: XYPos
    | EndDragging of sIds: ComponentId list
    | SelectSymbols of sIds: ComponentId list
    | DeselectSymbols of sIds: ComponentId list
    | AddSymbol of cType: ComponentType * pagePos: XYPos * label: string
    | DeleteSymbols of sIds: ComponentId list
    | ShowConnectedPorts of srcPortId: PortId * targetPortId: PortId
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    | UpdateSymbolModelWithComponent of Component // Issie interface


//---------------------------------helper types and functions----------------//



let posDiff a b = { X = a.X - b.X; Y = a.Y - b.Y }

let posAdd a b = { X = a.X + b.X; Y = a.Y + b.Y }

let posOf x y = { X = x; Y = y }


/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
/// Set IsDragging (for ports) to true to adjust wire colours
let createNewSymbol (input: XYPos * float * float * int * int) =
    let pos, height, width, inputWidth, outputWidth = input
    let hostId = ComponentId(uuid ())

    { Pos = pos
      LastDragPos = { X = 0.; Y = 0. } // initial value can always be this
      IsDragging = false // initial value can always be this
      Id = hostId // create a unique id for this symbol
      Width = width
      Height = height
      Ports =
          [ // Creates one input and one output port. For demo only.
            { Id = PortId(uuid ())
              HostId = hostId
              PortType = PortType.Input
              Pos = pos
              Width = inputWidth
              IsHighlighted = false
              ParentHeight = height
              IsDragging = true }
            { Id = PortId(uuid ())
              HostId = hostId
              PortType = PortType.Output
              Pos = { pos with X = pos.X + width }
              Width = outputWidth
              IsHighlighted = false
              ParentHeight = height
              IsDragging = true } ] }


let init () =
    [ ({ X = 100.; Y = 100. }, 100., 50., 1, 1)
      ({ X = 500.; Y = 300. }, 100., 50., 1, 1)
      ({ X = 200.; Y = 500. }, 150., 50., 1, 1)
      ({ X = 500.; Y = 600. }, 80., 50., 1, 1) ]
    |> List.map createNewSymbol,
    Cmd.none

/// update function which displays symbols
let update (msg: Msg) (model: Model): Model * Cmd<'a> =
    match msg with
    | AddSymbol (_, pagePos, _) -> (createDummySymbol pagePos) :: model, Cmd.none
    | DeleteSymbols sIds ->
        model
        |> List.filter (fun sym -> not (List.contains sym.Id sIds)),
        Cmd.none
    | SelectSymbols sIds -> // Author: Lukas Baliunas
        model
        |> List.map (fun sym -> if List.contains sym.Id sIds then { sym with IsHighlighted = true } else sym),
        Cmd.none
    | DeselectSymbols sIds -> // Author: Lukas Baliunas
        model
        |> List.map (fun sym -> if List.contains sym.Id sIds then { sym with IsHighlighted = false } else sym),
        Cmd.none
    | StartDragging (sIds, pagePos) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sIds then
                { sym with
                      LastDragPos = pagePos
                      IsHighlighted = true
                      Ports =
                          sym.Ports
                          |> List.map (fun port -> { port with IsDragging = true }) }
            else
                sym),
        Cmd.none
    | Dragging (sIds, pagePos) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sIds then
                let diff = posDiff pagePos sym.LastDragPos
                let inputPort = List.head sym.Ports
                let outputPort = List.last sym.Ports

                { sym with
                      Pos = posAdd sym.Pos diff
                      IsTransparent = true
                      BoundingBox =
                          { P1 = posAdd sym.BoundingBox.P1 diff
                            P2 = posAdd sym.BoundingBox.P2 diff }
                      Ports =
                          [ { inputPort with
                                  Pos = posAdd inputPort.Pos diff
                                  BoundingBox =
                                      { P1 = posAdd inputPort.BoundingBox.P1 diff
                                        P2 = posAdd inputPort.BoundingBox.P2 diff } }
                            { outputPort with
                                  Pos = posAdd outputPort.Pos diff
                                  BoundingBox =
                                      { P1 = posAdd outputPort.BoundingBox.P1 diff
                                        P2 = posAdd outputPort.BoundingBox.P2 diff } } ]
                      LastDragPos = pagePos }
            else
                sym),
        Cmd.none
    | EndDragging sIds ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sIds then
                { sym with
                      IsShowingPorts = false //check the way this is actually working
                      ShowingPortsType = None
                      IsTransparent = false }
            else
                sym),
        Cmd.none
    | ShowPorts (symbolIds, portType) -> // Author: Lukas Baliunas
        printfn "Show ports"

        model
        |> List.map (fun sym ->
            if List.contains sym.Id symbolIds then
                { sym with
                      IsShowingPorts = true
                      ShowingPortsType = portType }
            else
                sym),
        Cmd.none
    | HidePorts smIds -> // Author: Lukas Baliunas
        model
        |> List.map (fun sym -> if List.contains sym.Id smIds then { sym with IsShowingPorts = false } else sym),
        Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messages
    | _ -> model, Cmd.none

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
                                   (props.Component.Pos.X) // Top left
                                   (props.Component.Pos.Y - (props.Component.Height / 2.))
                                   (props.Component.Pos.X + props.Component.Width) // Top right
                                   (props.Component.Pos.Y - (props.Component.Height / 2.))
                                   (props.Component.Pos.X + props.Component.Width) // Bottom right
                                   (props.Component.Pos.Y + (props.Component.Height / 2.))
                                   (props.Component.Pos.X) // Bottom left
                                   (props.Component.Pos.Y + (props.Component.Height / 2.)))
                          SVGAttr.Fill colour
                          SVGAttr.Stroke colour
                          SVGAttr.StrokeWidth 1 ] []

                circle [ Cx inputPort.Pos.X
                         Cy inputPort.Pos.Y
                         R 5
                         SVGAttr.Fill "darkgrey"
                         SVGAttr.Stroke "grey"
                         SVGAttr.StrokeWidth 1 ] []
                circle [ Cx outputPort.Pos.X
                         Cy outputPort.Pos.Y
                         R 5
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

let findPort (model: Model) (portId: PortId): Port =
    model
    |> List.collect (fun symbol -> symbol.Ports)
    |> List.find (fun port -> port.Id = portId)

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
