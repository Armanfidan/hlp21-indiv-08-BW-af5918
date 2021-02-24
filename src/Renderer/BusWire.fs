﻿module BusWire

open System
open Symbol
open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type BoundingBox = { P1: XYPos; P2: XYPos }

type Wire =
    { Id: ConnectionId
      SourcePort: ComponentId
      TargetPort: ComponentId
      IsError: bool
      Width: int
      BoundingBoxes: BoundingBox list
      Corners: XYPos list }

type Model =
    { Symbols: Symbol.Model
      Wires: Wire list
      Colour: HighLightColour }

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of ConnectionId * ConnectionId
    | SetColor of HighLightColour
    | MouseMsg of MouseT

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
     wModel.Wires
     |> List.filter (fun wire -> wire.Id = wId)
     |> List.head


let findCorners (sourcePort: XYPos) (targetPort: XYPos) h1 h2 =
    // Midpoints
    let xMid = (targetPort.X + sourcePort.X) / 2.0

    let yMid = (targetPort.Y + sourcePort.Y) / 2.0

    // Source coordinates
    let x1 = sourcePort.X
    let y1 = sourcePort.Y

    // Target coordinates
    let x2 = targetPort.X
    let y2 = targetPort.Y

    // Heights of parent symbols of the source and target ports, adjusted manually
    let ha1 = h1 / 2. + 20.

    let ha2 = h1 / 2. + 20.

    // Minimum distance to go straight from ports
    let xMin = 20.

    // If source is on the left of target
    let xPositive = x2 > x1
    // If source is below target
    let yPositive = y2 > y1

    // Distances between ports
    let xDiff = Math.Abs(x2 - x1)
    let yDiff = Math.Abs(y2 - y1)

    /// Position of the first corner: If there is enough space then xMid,
    /// otherwise minimum distance from source port
    let xCorner1 =
        if xPositive && (yDiff < ha1 + ha2 || xDiff >= 2. * xMin) then xMid
        else x1 + xMin
    /// Position of the first corner: If there is enough space then xMid,
    /// otherwise minimum distance from target port
    let xCorner2 =
        if xPositive && (yDiff < ha1 + ha2 || xDiff >= 2. * xMin) then xMid
        else x2 - xMin

    /// Vertical line segments
    let yCorner1 =
        if yPositive && (yDiff >= ha1 || not xPositive)
        then y1 + ha1
        elif yDiff >= ha1 || not xPositive
        then y1 - ha1
        elif yPositive
        then y1 + yDiff
        else y1 - yDiff

    let yCorner2 =
        if yPositive && (yDiff >= ha2 || not xPositive)
        then y2 - ha2
        elif yDiff >= ha2 || not xPositive
        then y2 + ha2
        elif yPositive
        then y2 - yDiff
        else y2 + yDiff

    let yMidAdaptive1 =
        if yDiff >= ha1 + ha2 then yMid else yCorner1

    let yMidAdaptive2 =
        if yDiff >= ha1 + ha2 then yMid else yCorner2
    
    // Corner list    
    [
          { X = x1; Y = y1 }
          { X = xCorner1; Y = y1 }
          { X = xCorner1; Y = yMidAdaptive1 }
          { X = xMid; Y = yMidAdaptive1 }
          { X = xMid; Y = yMidAdaptive2 }
          { X = xCorner2; Y = yMidAdaptive2 }
          { X = xCorner2; Y = y2 }
          { X = x2; Y = y2 }
    ]    


// type WireRenderProps = {
//     key : ConnectionId
//     WireP: Wire
//     SourcePos: XYPos 
//     TargetPos: XYPos
//     WireColour: string
//     WireWidth: string }

type WireRenderProps =
    { key: ConnectionId
      SourcePos: XYPos
      TargetPos: XYPos
      SourceHeight: float
      TargetHeight: float
      WireColour: string
      WireWidth: string }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let corners = findCorners props.SourcePos props.TargetPos props.SourceHeight props.TargetHeight

        /// We use all above functions to construct all line and curve segments.
            g [] [
                // --------------------Port extensions-------------------------
                polyline [
                    SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f "
                                            corners.[0].X corners.[0].Y corners.[1].X corners.[1].Y
                                            corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y
                                            corners.[4].X corners.[4].Y corners.[5].X corners.[5].Y
                                            corners.[6].X corners.[6].Y corners.[7].X corners.[7].Y)
                    SVGAttr.Stroke props.WireColour
                    SVGAttr.StrokeWidth props.WireWidth
                    SVGAttr.FillOpacity "0"
                ][]
                // // Source to first corner
                // line [ X1 corners.[0].X
                //        Y1 corners.[0].Y
                //        X2 corners.[1].X
                //        Y2 corners.[1].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                // // props.Wire.BoundingBoxes = props.Wire.BoundingBoxes @ { P1 = { X = x1 - 5.; Y = y1 - 5. }; P2 = { X = xCorner1 + 5.; Y = y1 + 5. } }
                // // Last corner to target
                // line [ X1 corners.[6].X
                //        Y1 corners.[6].Y
                //        X2 corners.[7].X
                //        Y2 corners.[7].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                //
                //
                // // --------------------Vertical lines------------------------
                // // Left, connecting source extension to first horizontal line
                // line [ X1 corners.[1].X
                //        Y1 corners.[1].Y
                //        X2 corners.[2].X
                //        Y2 corners.[2].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                // // Middle vertical line, connecting the two horizontal lines
                // line [ X1 corners.[3].X
                //        Y1 corners.[3].Y
                //        X2 corners.[4].X
                //        Y2 corners.[4].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                // // Right, connecting second horizontal line to target extension
                // line [ X1 corners.[5].X
                //        Y1 corners.[5].Y
                //        X2 corners.[6].X
                //        Y2 corners.[6].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                //
                // // -------------------Horizontal connectors---------------------
                // // First mid horizontal line
                // line [ X1 corners.[2].X
                //        Y1 corners.[2].Y
                //        X2 corners.[3].X
                //        Y2 corners.[3].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                // // Second mid horizontal line
                // line [ X1 corners.[4].X
                //        Y1 corners.[4].Y
                //        X2 corners.[5].X
                //        Y2 corners.[5].Y
                //        SVGAttr.Stroke props.WireColour
                //        SVGAttr.StrokeWidth props.WireWidth ] []
                //
                if props.WireWidth = "3" then
                    text [ SVGAttr.X (corners.[0].X + 5.)
                           SVGAttr.Y (corners.[0].Y - 5.) ] [
                        str <| sprintf "%A" props.WireWidth
                    ]
        ])


let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        model.Wires
        |> List.map (fun wire ->
            let source = findPort model.Symbols wire.SourcePort
            let target = findPort model.Symbols wire.TargetPort
            let props = {
                key = wire.Id
                SourcePos = source.Pos
                TargetPos = target.Pos
                SourceHeight = source.ParentHeight
                TargetHeight = target.ParentHeight
                WireColour = model.Colour.Text()
                WireWidth = "2px" }
            singleWireView props)
    let symbols = Symbol.view model.Symbols (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]
    
    
let createWire (sourcePort: Port) (targetPort: Port) : Wire =
    { Id = ConnectionId(uuid ())
      SourcePort = sourcePort.Id
      TargetPort = targetPort.Id
      IsError = sourcePort.Width <> targetPort.Width
      Width = if sourcePort.Width = targetPort.Width then int sourcePort.Width else 3 // If there is an error then the width is 2, to make a thick red wire.
      BoundingBoxes = []
      Corners = findCorners sourcePort.Pos targetPort.Pos sourcePort.ParentHeight targetPort.ParentHeight}

let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    let makeRandomWire() =
        let n = symIds.Length
        let s1,s2 =
            match rng.Next(0,n-1), rng.Next(0,n-2) with
            | r1,r2 when r1 = r2 -> 
                symbols.[r1],symbols.[n-1] // prevents wire target and source being same
            | r1,r2 -> 
                symbols.[r1],symbols.[r2]
        {
            Id=CommonTypes.ConnectionId (uuid())
            SrcSymbol = s1.Id
            TargetSymbol = s2.Id
        }
    List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 
    failwith "Not implemented"

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    


