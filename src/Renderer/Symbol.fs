module Symbol

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

type Port =
    { Id: PortId
      PortType: PortType
      Pos: XYPos
      BoundingBox: BoundingBox
      Width: int
      IsHighlighted: bool
      PositionModified: bool }
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

let symbol (sModel: Model) (sId: ComponentId): Symbol =
    sModel |> List.find (fun sm -> sm.Id = sId)


// let circleRadius = 20.
let portRadius = 3.

let largerPortRadius = 6.

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
/// Set IsDragging (for ports) to true to adjust wire colours
let createNewSymbol (input: XYPos * float * float): Symbol =
    let pos, height, width = input

    let inputCirclePos = { pos with X = pos.X - width / 2. }
    let outputCirclePos = { pos with X = pos.X + width / 2. }

    let inputBoundingBox =
        { P1 =
              { X = inputCirclePos.X - portRadius
                Y = inputCirclePos.Y - portRadius }
          P2 =
              { X = inputCirclePos.X + portRadius
                Y = inputCirclePos.Y + portRadius } }

    let outputBoundingBox =
        { P1 =
              { X = outputCirclePos.X - portRadius
                Y = outputCirclePos.Y - portRadius }
          P2 =
              { X = outputCirclePos.X + portRadius
                Y = outputCirclePos.Y + portRadius } }

    let hostId = ComponentId(uuid ())

    { Id = hostId
      Pos = pos
      LastDragPos = { X = 0.; Y = 0. }
      IsTransparent = false
      IsHighlighted = false
      IsShowingPorts = false
      ShowingPortsType = None
      NoOfInputPorts = 1 //Default for demo
      NoOfOutputPorts = 1 //Default for demo
      ComponentType = Not //Default for demo
      Label = "" //Default for demo
      Ports =
          [ { Id = PortId(uuid ())
              PortType = PortType.Input
              Pos = inputCirclePos
              BoundingBox = inputBoundingBox
              Width = 1
              IsHighlighted = false
              PositionModified = false }
            { Id = PortId(uuid ())
              PortType = PortType.Output
              Pos = outputCirclePos
              BoundingBox = outputBoundingBox
              Width = 1
              IsHighlighted = false
              PositionModified = false } ]
      BoundingBox =
          { P1 =
                { X = pos.X - width / 2.
                  Y = pos.Y - height / 2. }
            P2 =
                { X = pos.X + width / 2.
                  Y = pos.Y + height / 2. } } }

let createDummySymbol pos = createNewSymbol (pos, 100., 50.)

let init () =
    [ ({ X = 100.; Y = 100. }, 100., 50.)
      ({ X = 200.; Y = 200. }, 100., 50.) ]
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
                          |> List.map (fun port -> { port with PositionModified = true }) }
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
            let opacity =
                if props.Component.IsTransparent then 0.5 else 1.

            let inputPorts =
                List.filter (fun port -> port.PortType = PortType.Input) props.Component.Ports

            let outputPorts =
                List.filter (fun port -> port.PortType = PortType.Output) props.Component.Ports

            let inputPortRadius, outputPortRadius =
                match props.Component.ShowingPortsType with
                | Some PortType.Input -> largerPortRadius, portRadius
                | Some PortType.Output -> portRadius, largerPortRadius
                | None -> portRadius, portRadius

            // Dummy component has one input and one output
            let inputPort = inputPorts.Head
            let outputPort = outputPorts.Head

            let colour =
                if props.Component.IsHighlighted then "lightblue" else "grey"

            let topLeft = props.Component.BoundingBox.P1

            let topRight =
                { props.Component.BoundingBox.P1 with
                      X = props.Component.BoundingBox.P2.X }

            let bottomRight = props.Component.BoundingBox.P2

            let BottomLeft =
                { props.Component.BoundingBox.P2 with
                      X = props.Component.BoundingBox.P1.X }

            g [] [
                polygon [ SVGAttr.Points
                          <| sprintf
                              "%0.2f,%0.2f %0.2f,%0.2f %0.2f,%0.2f %0.2f,%0.2f"
                                 topLeft.X
                                 topLeft.Y
                                 topRight.X
                                 topRight.Y
                                 bottomRight.X
                                 bottomRight.Y
                                 BottomLeft.X
                                 BottomLeft.Y
                          SVGAttr.Fill colour
                          SVGAttr.Stroke colour
                          SVGAttr.Opacity opacity
                          SVGAttr.StrokeWidth 1 ] []

                if props.Component.IsShowingPorts then
                    circle [ Cx inputPort.Pos.X
                             Cy inputPort.Pos.Y
                             R inputPortRadius
                             SVGAttr.Fill "darkgrey"
                             SVGAttr.Stroke "grey"
                             SVGAttr.StrokeWidth 1 ] []

                    circle [ Cx outputPort.Pos.X
                             Cy outputPort.Pos.Y
                             R outputPortRadius
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

let symbolPos (symModel: Model) (sId: ComponentId): XYPos =
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

let findPort (model: Model) (portId: PortId): Port =
    model
    |> List.collect (fun symbol -> symbol.Ports)
    |> List.find (fun port -> port.Id = portId)
