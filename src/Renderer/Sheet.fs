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
let displaySvgWithZoom (zoom: float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev: Types.MouseEvent) = if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev: Types.MouseEvent) =
        dispatch
        <| Wire
            (BusWire.MouseMsg
                { Op = op
                  Pos =
                      { X = ev.clientX / zoom
                        Y = ev.clientY / zoom } })

    div [ Style [ Height "100vh"
                  MaxWidth "100vw"
                  CSSProp.OverflowX OverflowOptions.Auto
                  CSSProp.OverflowY OverflowOptions.Auto ]
          OnMouseDown(mouseOp Down)
          OnMouseUp(mouseOp Up)
          OnMouseMove(fun ev -> mouseOp (if mDown ev then Drag else Move) ev) ] [
        svg [ Style [ Border "3px solid green"
                      Height sizeInPixels
                      Width sizeInPixels ] ] [
            g [ Style [ Transform(sprintf "scale(%f)" zoom) ] ] [
                svgReact
            ]
        ]
    ]

/// for the demo code
let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | KeyPress AltShiftZ ->
        printStats () // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey

        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColour c)

let init () =
    // Don't forget to change this back to 400 before demo!
    let model, cmds = (BusWire.init 3) ()
    { Wire = model }, Cmd.map Wire cmds
