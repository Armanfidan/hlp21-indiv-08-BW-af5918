module Sheet

open CommonTypes
open Electron
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open BusWire
open Helpers

(*
    Sheet with extra features
    Author: Lukas Baliunas
*)

type SheetElement =
    | SymbolElement of Symbol.Symbol
    | PortElement of Symbol.Port
    | ConnectionElement of BusWire.Wire
    | CanvasElement

type DragType =
    | Symbol of ComponentId list
    | Connection of srcPort: Symbol.Port * srcPos: XYPos * lastDragPos: XYPos
    | Wire of ConnectionId
    | Canvas of srcPos: XYPos * lastDragPos: XYPos

type Model = {
    Wire: BusWire.Model
    DragType: DragType option
    SelectedSymbolIds: ComponentId list
    SelectedConnectionIds: ConnectionId list
    IsShiftPressed: bool
}

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | Shift of KeyOp

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    
/// Helper for better readability, when sending messages from Sheet to Symbol 
let getSymbolMsg (msg:Symbol.Msg) = 
    Cmd.ofMsg <| Wire (BusWire.Symbol msg)
    
/// Helper for better readability, when sending messages from Sheet to BusWire 
let getWireMsg (msg:BusWire.Msg) =
    Cmd.ofMsg <| Wire msg

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 2.0

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and change scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false

    let mouseOp (op:MouseOp) (ev:Types.MouseEvent) =
        dispatch <| MouseMsg {Op = op ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}}
        
    let keyOp (op:KeyOp) (ev:Types.KeyboardEvent) =
        match ev.key, op, model.IsShiftPressed with
            | "Shift", KeyDown, false | "Shift", KeyUp, true -> dispatch <| KeyPress (Shift op)
            | "Delete", KeyDown, _ -> dispatch <| KeyPress DEL
            | _ -> ()
        
    /// Creates the React dragging element (connection line or multi-select rectangle) or nothing
    let draggingSvgElement =
          match model.DragType with
            | Some (Connection (_,srcPos,lastDragPos)) ->
                [
                    line [
                        X1 srcPos.X
                        Y1 srcPos.Y
                        X2 lastDragPos.X
                        Y2 lastDragPos.Y
                        Style [
                            Stroke "blue"
                            StrokeWidth "0.5px"
                            StrokeDasharray "5 5"
                        ]
                    ] []
                ]
            | Some (Canvas (srcPos, lastDragPos)) ->
                let x1 = srcPos.X
                let y1 = srcPos.Y
                let x2 = lastDragPos.X
                let y2 = lastDragPos.Y
                [
                    polygon [
                        SVGAttr.Points <| sprintf "%f,%f %f,%f %f,%f %f,%f" x1 y1 x1 y2 x2 y2 x2 y1
                        SVGAttr.StrokeWidth "0.5px"
                        SVGAttr.Stroke "blue"
                        SVGAttr.StrokeDasharray "5 5"
                        SVGAttr.Fill "none"
                    ] []
                ]
            | _ -> List.empty
            
    let finalSvg = List.append [svgReact] draggingSvgElement

    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ]
          TabIndex -1
          OnKeyDown (keyOp KeyDown)
          OnKeyUp (keyOp KeyUp)
          OnMouseDown (mouseOp Down)
          OnMouseUp (mouseOp Up)
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else MouseOp.Move) ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                finalSvg
            ]
        ]

/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) = 
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch model
    
/// Returns the opposite of the given PortType, e.g. Input returns Output, Output returns Input
let getOppositePortType (portType:PortType):PortType =
    match portType with
        | PortType.Input -> PortType.Output
        | PortType.Output -> PortType.Input
        
/// Finds the Port that is in the location of the mouse, returns None if no such Port was found
let tryFindClickedPort (mousePos:XYPos) (symbols:Symbol.Symbol list):Symbol.Port option = 
    symbols
    |> List.collect (fun x -> x.Ports)
    |> List.tryFindBack (fun port -> containsPoint port.BoundingBox mousePos)
        
/// Finds the Port element that is in the location of the mouse, returns None if no such Port was found
let tryFindClickedPortElement (mousePos:XYPos) (symbols:Symbol.Symbol list):SheetElement option = 
    tryFindClickedPort mousePos symbols
    |> Option.bind (fun p -> Some <| PortElement p)
    
/// Finds the Symbol element that is in the location of the mouse, returns None if no such Symbol was found
let tryFindClickedSymbolElement (mousePos:XYPos) (symbols:Symbol.Symbol list):SheetElement option =
    symbols
    |> List.tryFindBack (fun sm -> containsPoint sm.BoundingBox mousePos)
    |> Option.bind (fun sm -> Some <| SymbolElement sm)
    
/// Finds the Wire element that is in the location of the mouse, returns None if no such Wire was found
let tryFindClickedWireElement (mousePos:XYPos) (wires: BusWire.Wire list):SheetElement option =
    let isSegmentClicked (wire: BusWire.Wire) =
        wire.BoundingBoxes
        |> List.exists (fun bb -> containsPoint bb.Box mousePos)

    wires
    |> List.tryFind isSegmentClicked //maybe tryFindBack??
    |> Option.bind (fun wr -> Some <| ConnectionElement wr)
    
/// Gets the element that is in the mouse position
let getClickedElement (mousePos:XYPos) (symbols:Symbol.Symbol list) (wires:BusWire.Wire list):SheetElement =
    let canvasOrWireThunk =
        fun () ->
            tryFindClickedWireElement mousePos wires
            |> Option.defaultValue SheetElement.CanvasElement

    let connectionOrSymbolThunk =
        fun () ->
            tryFindClickedSymbolElement mousePos symbols
            |> Option.defaultWith canvasOrWireThunk
    
    tryFindClickedPortElement mousePos symbols
    |> Option.defaultWith connectionOrSymbolThunk

/// Finds the symbols that are fully contained inside of the given bounding box and returns a list containing their ids.
let getSymbolsInsideBoundingBox (bBox:BoundingBox) (symbols:Symbol.Symbol list):ComponentId list = 
    symbols
    |> List.filter (fun sm -> containsBox sm.BoundingBox bBox)
    |> List.map (fun sm -> sm.Id)
    
let getWiresInsideBoundingBox (bBox:BoundingBox) (wires:BusWire.Wire list):ConnectionId list =
    let isAllSegmentsInside (wr:BusWire.Wire) =
        wr.BoundingBoxes
        |> List.forall (fun segment ->
            containsBox segment.Box bBox)

    wires
    |> List.filter isAllSegmentsInside
    |> List.map (fun wr -> wr.Id)

/// Returns a tuple containing the list of symbol ids within and outside a threshold from the mouse
let getToShowAndToHidePortsSymbols (mousePos:XYPos) (symbols:Symbol.Symbol list) = 
    let partitionedSymbols =
        symbols
        |> List.partition (fun sm -> distanceFromPoint sm.BoundingBox mousePos < 20.)
    
    let toShowPortsSymbolIds =
        fst partitionedSymbols
        |> List.filter (fun sm -> not sm.IsShowingPorts)
        |> List.map (fun sm -> sm.Id)
        
    let toHidePortsSymbolIds =
        snd partitionedSymbols
        |> List.filter (fun sm -> sm.IsShowingPorts)
        |> List.map (fun sm -> sm.Id)

    toShowPortsSymbolIds, toHidePortsSymbolIds
    
let appendIfAbsent item list =
    if List.contains item list then
        list
    else
        item::list

let removeItem item list =
    list
    |> List.filter (fun x -> x <> item)

let getShowPortsAndHidePortsCommandList (mousePos:XYPos) (symbols:Symbol.Symbol list) (specificPortType:PortType option):Cmd<Msg> list = 
    let toShowPortsSymbols, toHidePortsSymbols = getToShowAndToHidePortsSymbols mousePos symbols
    
    let toShowPortsCommand = 
        if List.isEmpty toShowPortsSymbols then
            List.empty
        else
            [getSymbolMsg <| Symbol.ShowPorts (toShowPortsSymbols, specificPortType)]
    
    let toHidePortsCommand = 
        if List.isEmpty toHidePortsSymbols then
            List.empty
        else 
            [getSymbolMsg <| Symbol.HidePorts toHidePortsSymbols]
    
    List.append toShowPortsCommand toHidePortsCommand
    
let getCmdIfNotEmpty (cmd:Cmd<Msg>) list =
    if List.isEmpty list then
        List.empty
    else
        [cmd]
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    /// hold often used values as constants for better readability
    let symbols = model.Wire.Symbols
    let wires = model.Wire.Wires
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress AltZ ->
        model, getSymbolMsg <| Symbol.AddSymbol (ComponentType.Not, {X=50.;Y=50.}, "")  //Random type for simplicity for demo
    | KeyPress (Shift KeyDown) ->
        { model with
            IsShiftPressed = true 
        }
        , Cmd.none
    | KeyPress (Shift KeyUp) ->
        { model with
            IsShiftPressed = false 
        }
        , Cmd.none
    | KeyPress DEL ->
        let deleteSymbolsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeleteSymbols model.SelectedSymbolIds) model.SelectedSymbolIds
        let deleteConnectionsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeleteWires model.SelectedConnectionIds) model.SelectedConnectionIds

        { model with
            SelectedSymbolIds = List.empty
            SelectedConnectionIds = List.empty
        }
        ,
        Cmd.batch <| deleteSymbolsCmd@deleteConnectionsCmd
    | MouseMsg mouseT ->
        match mouseT.Op with
            | Move ->
                let toShowPortsSymbols, toHidePortsSymbols = getToShowAndToHidePortsSymbols mouseT.Pos symbols
                
                let showPortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.ShowPorts (toShowPortsSymbols, None)) toShowPortsSymbols
                let hidePortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.HidePorts toHidePortsSymbols) toHidePortsSymbols

                model, Cmd.batch <| showPortsCmd@hidePortsCmd
            | Down ->
                let clickedElement = getClickedElement mouseT.Pos symbols wires
                
                match clickedElement with
                    | PortElement port ->
                        let deselectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.SelectedSymbolIds) model.SelectedSymbolIds
                        let deselectConnectionsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.SelectedConnectionIds) model.SelectedConnectionIds

                        { model with
                            DragType = Some <| Connection (port, port.Pos, port.Pos)
                            SelectedSymbolIds = List.empty
                            SelectedConnectionIds = List.empty
                        }
                        ,  Cmd.batch <| deselectSymbolsCmd@deselectConnectionsCmd
                    | SheetElement.SymbolElement symbol ->
                        
                        let selectedSymbolIds, deselectSymbolsCmd, selectedConnectionIds, deselectConnectionsCmd =
                            if model.IsShiftPressed || List.contains symbol.Id model.SelectedSymbolIds then
                                appendIfAbsent symbol.Id model.SelectedSymbolIds, List.empty, model.SelectedConnectionIds, List.empty
                            else
                                [symbol.Id], getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.SelectedSymbolIds) model.SelectedSymbolIds,
                                List.empty, getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.SelectedConnectionIds) model.SelectedConnectionIds
                        
                        let startDraggingCmd = [getSymbolMsg <| Symbol.StartDragging (selectedSymbolIds, mouseT.Pos)]

                        { model with
                            DragType = Some <| Symbol selectedSymbolIds
                            SelectedSymbolIds = selectedSymbolIds
                            SelectedConnectionIds = selectedConnectionIds
                        }
                        , Cmd.batch <| deselectSymbolsCmd@deselectConnectionsCmd@startDraggingCmd
                    | SheetElement.ConnectionElement connection ->                        
                        let selectedSymbolIds, deselectSymbolsCmd =
                            if model.IsShiftPressed then
                                model.SelectedSymbolIds, List.empty
                            else
                                List.empty, getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.SelectedSymbolIds) model.SelectedSymbolIds
                                
                        let selectedConnectionIds, deselectConnectionsCmd =
                            if model.IsShiftPressed then
                                appendIfAbsent connection.Id model.SelectedConnectionIds, List.empty
                            else
                                [connection.Id], getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.SelectedConnectionIds) model.SelectedConnectionIds 

                        let startDraggingCmd = [getWireMsg <| BusWire.StartDragging (connection.Id, mouseT.Pos)]
                        
                        { model with
                            DragType = Some <| DragType.Wire connection.Id
                            SelectedSymbolIds = selectedSymbolIds
                            SelectedConnectionIds = selectedConnectionIds
                        }
                        , Cmd.batch <| deselectSymbolsCmd@deselectConnectionsCmd@startDraggingCmd
                    | SheetElement.CanvasElement ->
                        let deselectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.SelectedSymbolIds) model.SelectedSymbolIds
                        let deselectConnectionsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.SelectedConnectionIds) model.SelectedConnectionIds

                        { model with
                            DragType = Some <| Canvas (mouseT.Pos, mouseT.Pos)
                            SelectedSymbolIds = List.empty
                            SelectedConnectionIds = List.empty
                        }
                        , Cmd.batch <| deselectSymbolsCmd@deselectConnectionsCmd
            | Drag ->
                match model.DragType with
                    //Dragging Symbol. Send Dragging message to Symbol
                    | Some (Symbol sIds) ->
                        model, getSymbolMsg <| Symbol.Dragging (sIds, mouseT.Pos)
                    //Dragging Connection. Update connection mouse position. 
                    | Some (Connection (srcPort, srcPos, _)) -> 
                        let toShowPortsSymbols, toHidePortsSymbols = getToShowAndToHidePortsSymbols mouseT.Pos symbols
                
                        let showPortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.ShowPorts (toShowPortsSymbols, Some <| getOppositePortType srcPort.PortType)) toShowPortsSymbols
                        let hidePortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.HidePorts toHidePortsSymbols) toHidePortsSymbols
                        
                        { model with
                            DragType = Some <| Connection (srcPort,srcPos, mouseT.Pos)
                        }
                        , Cmd.batch <| showPortsCmd@hidePortsCmd
                    | Some (DragType.Wire cId) ->
                        model, getWireMsg <| BusWire.Dragging (cId, mouseT.Pos)
                    //Dragging rectangle. Show Ports and update mouse position
                    | Some (Canvas (srcPos, _)) ->
                        let toShowPortsSymbols, toHidePortsSymbols = getToShowAndToHidePortsSymbols mouseT.Pos symbols
                
                        let showPortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.ShowPorts (toShowPortsSymbols, None)) toShowPortsSymbols
                        let hidePortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.HidePorts toHidePortsSymbols) toHidePortsSymbols

                        { model with
                            DragType = Some <| Canvas (srcPos, mouseT.Pos)
                        }
                        , Cmd.batch <| showPortsCmd@hidePortsCmd
                    // Will never happen
                    | None ->
                        model, Cmd.none
            | Up ->
                match model.DragType with
                    //End symbol dragging
                    | Some (Symbol sIds) ->
                        { model with
                            DragType = None
                        },
                        getSymbolMsg <| Symbol.EndDragging sIds
                    //End connection dragging. If on valid port, create a connection
                    | Some (Connection (srcPort,_,_)) ->
                        let clickedPort = tryFindClickedPort mouseT.Pos symbols
                        
                        { model with
                            DragType = None  
                        },
                        match clickedPort with
                            | Some targetPort ->
                                match srcPort.PortType, targetPort.PortType with
                                    | PortType.Input, PortType.Output -> 
                                        Cmd.ofMsg (Wire <| BusWire.CreateConnection (targetPort.Id, srcPort.Id))
                                    | PortType.Output, PortType.Input ->
                                        Cmd.ofMsg (Wire <| BusWire.CreateConnection (srcPort.Id, targetPort.Id))
                                    | _ -> Cmd.none
                            | None ->
                                Cmd.none
                    //Select symbols inside of rectangle
                    | Some (Canvas (srcPos, lastDragPos)) ->
                        let topLeft, bottomRight =
                            if srcPos.X < lastDragPos.X && srcPos.Y < lastDragPos.Y then srcPos, lastDragPos
                            elif srcPos.X < lastDragPos.X && srcPos.Y > lastDragPos.Y then { srcPos with Y = lastDragPos.Y }, { lastDragPos with Y = srcPos.Y }
                            elif srcPos.X > lastDragPos.X && srcPos.Y > lastDragPos.Y then lastDragPos, srcPos
                            else { srcPos with X = lastDragPos.X }, { lastDragPos with X = srcPos.X }
                        let toSelectSymbolIds = getSymbolsInsideBoundingBox {P1=topLeft; P2=bottomRight} symbols
                        let toSelectConnectionIds = getWiresInsideBoundingBox {P1=topLeft; P2=bottomRight} wires
                        
                        let selectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.SelectSymbols toSelectSymbolIds) toSelectSymbolIds
                        let selectConnectionsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.SelectWires toSelectConnectionIds) toSelectConnectionIds

                        { model with
                            DragType = None
                            SelectedSymbolIds = toSelectSymbolIds
                            SelectedConnectionIds = toSelectConnectionIds
                        }
                        , Cmd.batch <| selectSymbolsCmd@selectConnectionsCmd
                    | Some (DragType.Wire cId) ->
                        { model with
                            DragType = None
                        },
                        getWireMsg <| BusWire.EndDragging cId
                    | None ->
                        model, Cmd.none
    | _ -> model, Cmd.none
                

let init() = 
    let model,cmds = (BusWire.init 0)()
    {        
        Wire = model
        DragType = None
        SelectedSymbolIds = List.empty
        SelectedConnectionIds = List.empty
        IsShiftPressed = false
    }, Cmd.map Wire cmds
