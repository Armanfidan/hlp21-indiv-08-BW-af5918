module Sheet

open BusWire
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = { Wire: BusWire.Model }

type KeyboardMsg =
    | CtrlS
    | AltC
    | AltV
    | AltZ
    | AltShiftZ
    | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

/// Determines top-level zoom, > 1 => magnify.
/// This should be moved into the model as state
let zoom = 1.0

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

let boxContainsPoint (boundingBox: BoundingBox) (pagePos: XYPos): bool =
    let p1 = boundingBox.P1
    let p2 = boundingBox.P2

    let xCondition = pagePos.X > p1.X && pagePos.X < p2.X
    let yCondition = pagePos.Y < p1.Y && pagePos.Y > p2.Y

    xCondition && yCondition

/// Given a model and a position on the page (mouse position), returns the first wire that the mouse is on,
/// along with the index of the segment of that wire that the mouse is on.
let tryFindClickedSegment (pagePos: XYPos) (model: BusWire.Model): (Wire * int) option =
    let wire =
        model.Wires
        |> List.tryFind (fun wire ->
                let foundBoxes =
                    List.tryFind (fun boundingBox -> boxContainsPoint boundingBox pagePos) wire.BoundingBoxes
                
                foundBoxes <> None)
    match wire with
    | Some w -> let segmentIndex =
                    w.BoundingBoxes
                    |> List.mapi (fun index boundingBox -> (index, boundingBox))
                    |> List.tryFind (fun (_, boundingBox) -> boxContainsPoint boundingBox pagePos)
                match segmentIndex with
                | Some s -> Some (w, fst s)
                | None -> None
    | None -> None

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
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

let init () =
    // Don't forget to change this back to 400 before demo!
    let model, cmds = (BusWire.init 1) ()
    { Wire = model }, Cmd.map Wire cmds
