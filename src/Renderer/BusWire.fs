module BusWire

open System
open CommonTypes
open Symbol
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type WireBoundingBox = { Box: BoundingBox; Prev: XYPos }

// Add bounding boxes to each segment of the wire.
type Wire =
    { Id: ConnectionId
      SourcePort: PortId
      TargetPort: PortId
      IsError: bool
      Width: int
      IsDragging: bool
      BoundingBoxes: WireBoundingBox list
      Corners: XYPos list
      DraggedCornerIndex: int
      LastDragPos: XYPos
      IsHighlighted: bool }

type Model =
    { Symbols: Symbol.Model
      Wires: Wire list
      Colour: HighLightColour }

//----------------------------Message Type-----------------------------------//

type Msg =
    | Symbol of Symbol.Msg
    | CreateConnection of PortId * PortId
    | SetColour of HighLightColour
    | StartDragging of wireId: ConnectionId * pagePos: XYPos
    | Dragging of wireId: ConnectionId * pagePos: XYPos
    | EndDragging of wireId: ConnectionId
    | SelectWires of wireIds: ConnectionId list
    | DeselectWires of wireIds: ConnectionId list
    | DeleteWires of wireIds: ConnectionId list
    | DeleteSymbols of sIds: ComponentId list
    | MouseMsg of MouseT

/// look up wire in the model
let wire (model: Model) (wireId: ConnectionId) : Wire =
    model.Wires
    |> List.filter (fun wire -> wire.Id = wireId)
    |> List.head

/// This returns the first wire that the port is connected to, but it won't work if a port is connected to
/// multiple wires. I will fix/update this later.
let wireFromPort (model: Model) (portId: PortId) : Wire =
    model.Wires
    |> List.filter (fun wire ->
        wire.SourcePort = portId
        || wire.TargetPort = portId)
    |> List.head

/// Returns the specified port, along with its parent symbol's bounding box for wire routing purposes.
let findPortData (symbols: Symbol.Model) (portId: PortId) : (Port * BoundingBox) option =
    symbols
    |> List.tryFind (fun symbol ->
        symbol.Ports
        |> List.map (fun port -> port.Id)
        |> List.contains portId)
    |> Option.bind (fun symbol ->
        Some
            (symbol.Ports
            |> List.map (fun port -> port.Id, port)
            |> List.find (fun (id, _) -> id = portId)
            |> snd
            , symbol.BoundingBox))

/// Automatically finds corners given source and target port positions and symbol heights
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

    let ha2 = h2 / 2. + 20.

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
        if xPositive
           && (yDiff < ha1 + ha2 || xDiff >= 2. * xMin) then
            xMid
        else
            x1 + xMin
    /// Position of the first corner: If there is enough space then xMid,
    /// otherwise minimum distance from target port
    let xCorner2 =
        if xPositive
           && (yDiff < ha1 + ha2 || xDiff >= 2. * xMin) then
            xMid
        else
            x2 - xMin

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

    /// Corner list, number of corners may vary depending on shape of wire.
    /// I will need to adjust to this when rendering the wire.
    if xCorner1 = xCorner2 then
        [ { X = x1; Y = y1 }
          { X = xMid; Y = y1 }
          { X = xMid; Y = y2 }
          { X = x2; Y = y2 } ]
    elif yMidAdaptive1 = yMidAdaptive2 then
        [ { X = x1; Y = y1 } // Beginning of extension 1 (Source)
          { X = xCorner1; Y = y1 } // End of extension 1
          { X = xCorner1; Y = yMidAdaptive1 } // End of vertical 1
          { X = xCorner2; Y = yMidAdaptive2 } // End of horizontal
          { X = xCorner2; Y = y2 } // End of vertical 2
          { X = x2; Y = y2 } ]
    else
        [ { X = x1; Y = y1 } // Beginning of extension 1 (Source)
          { X = xCorner1; Y = y1 } // End of extension 1
          { X = xCorner1; Y = yMidAdaptive1 } // End of vertical 1
          { X = xMid; Y = yMidAdaptive1 } // End of horizontal 1
          { X = xMid; Y = yMidAdaptive2 } // End of vertical 2
          { X = xCorner2; Y = yMidAdaptive2 } // End of horizontal 2
          { X = xCorner2; Y = y2 } // End of vertical 3
          { X = x2; Y = y2 } ]

/// Chooses top left and bottom right corners out of the given two, for bounding boxes
let chooseCorners firstCorner secondCorner =
    let s1 = if (firstCorner.X < secondCorner.X && firstCorner.Y = secondCorner.Y)
                || (firstCorner.X = secondCorner.X && firstCorner.Y < secondCorner.Y)
             then firstCorner
             else secondCorner
    let s2 = if s1 = firstCorner then secondCorner else firstCorner
    s1, s2

/// These boxes are in the same order as the corners, which makes it easy to pair them up with which line segment
/// lies within them.
/// The lines can be in any orientation. The top left corner is defined as the top left of
/// the former corner, and the bottom right corner is the bottom right of the latter corner.
/// However, the lines are sometimes inverted (former corner is on the right and the latter
/// is on the left). This means that I need to be more careful when either creating the boxes
/// or when checking that a point is within them.
let createBoundingBoxes (corners: XYPos list): WireBoundingBox list =
    let diff = { X = 5.; Y = 5. }
    /// Assuming there will always be at least three corners on any given wire. Remove the first and last
    /// corners because we do not want the end segments to be draggable. rest may be an empty list.
    let (firstCorner :: secondCorner :: rest) = corners.[1..corners.Length - 2]
    
    // I have to check this condition
    let s1, s2 = chooseCorners firstCorner secondCorner
    
    /// This will work even if rest is empty, as we have a start state.
    rest
    |> List.fold (fun boxes currentCorner ->
        (let previousBox = List.head boxes
         let p1 = previousBox.Prev
         let p2 = currentCorner
         /// Problem: If the corners are switched, P2 of the previous box is actually P1. So the next box is created
         /// as a huge rectangle. To fix this, I made a new field in WireBoundingBox which keeps the previous corner.
         let topLeft, bottomRight = chooseCorners p1 p2

         { Box =
               { P1 = posDiff topLeft diff
                 P2 = posAdd bottomRight diff }
           Prev = p2 }
         :: boxes

        ))
           /// I can define the first box like this since it will always emanate from an output, meaning that it will
           /// always face to the right. I will have to change this when we add different symbol orientations.
           [ { Box =
                   { P1 = posDiff s1 diff
                     P2 = posAdd s2 diff }
               Prev = secondCorner } ]
    |> List.rev

type WireRenderProps =
    { key: ConnectionId
      Wire: Wire
      Source: Port
      Target: Port
      WireColour: string
      WireWidth: int
      Dispatch: Dispatch<Msg> }

let singleWireView =
    FunctionComponent.Of(fun (props: WireRenderProps) ->
        let corners = props.Wire.Corners
        
        let widthText =
            if props.WireWidth = 0 then str "Error: widths do not match" else str <| sprintf "%d" props.WireWidth

        // Use to draw bounding boxes for debugging purposes
        let boxes: ReactElement list =
            createBoundingBoxes corners
            |> List.map (fun box ->
                (polygon [ SVGAttr.Points
                               (sprintf
                                   "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                    box.Box.P1.X  // First corner
                                    box.Box.P1.Y
                                    box.Box.P2.X  // Second corner
                                    box.Box.P1.Y
                                    box.Box.P2.X  // Third corner
                                    box.Box.P2.Y
                                    box.Box.P1.X  // Fourth corner
                                    box.Box.P2.Y)
                           SVGAttr.Stroke "blue"
                           SVGAttr.Fill "lightblue"
                           SVGAttr.Opacity 0.5 ] []))
            
        let drawCorners =
            match corners.Length with
            | 4 ->
                sprintf
                    "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f" corners.[0].X corners.[0].Y corners.[1].X
                    corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y
            | 6 ->
                sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f" corners.[0].X corners.[0].Y corners.[1].X
                    corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y corners.[4].X corners.[4].Y
                    corners.[5].X corners.[5].Y
            | 8 ->
                sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f " corners.[0].X
                    corners.[0].Y corners.[1].X corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y
                    corners.[4].X corners.[4].Y corners.[5].X corners.[5].Y corners.[6].X corners.[6].Y corners.[7].X
                    corners.[7].Y
            | _ -> failwithf "Invalid corners."
        
        /// We use all above functions to construct all line and curve segments.
        g
            []
            ([
                polyline [
                    SVGAttr.Points drawCorners
                    SVGAttr.Stroke props.WireColour
                    SVGAttr.StrokeWidth
                      (str
                          (sprintf "%d"
                           <| if props.WireWidth = 0 then 3 else props.WireWidth))
                    SVGAttr.FillOpacity "0"
                    // SVGAttr.Custom ("stroke-linejoin", "round")
                     ] []
                if props.WireWidth <> 1 then
                    text [
                        SVGAttr.X(corners.[0].X + 6.)
                        SVGAttr.Y(corners.[0].Y - 6.)
                        SVGAttr.Stroke props.WireColour
                        SVGAttr.Fill props.WireColour ] [
                        widthText
                    ] 
             ]
             // @ boxes // Uncomment to display bounding boxes, for debugging purposes
            )
            
            )

let view (model: Model) (dispatch: Msg -> unit) =

    let wires =
        model.Wires
        |> List.map (fun wire ->
            // printf "Corners: %A" wire.Corners
            // printf "Boxes: %A" wire.BoundingBoxes

            let source = findPort model.Symbols wire.SourcePort
            let target = findPort model.Symbols wire.TargetPort
            
            let wireColour =
                if wire.IsError then Red.Text()
                elif wire.IsHighlighted then Yellow.Text()
                else model.Colour.Text()

            let width =
                if source.Width = target.Width then source.Width else 0
            
            let props =
                { key = wire.Id
                  Wire = wire
                  Source = source
                  Target = target
                  WireColour = wireColour
                  WireWidth = width
                  Dispatch = dispatch }

            singleWireView props)

    let symbols =
        Symbol.view model.Symbols (fun sMsg -> dispatch (Symbol sMsg))

    g [] [ (g [] wires); symbols ]

let createWire (sourcePort: PortId) (targetPort: PortId) (symbols: Symbol.Model) : Wire =
    let sourcePort, sBox, targetPort, tBox =
        match findPortData symbols sourcePort, findPortData symbols targetPort with
        | Some (sp, sb), Some (tp, tb) -> sp, sb, tp, tb
        | _ -> failwithf "Ports not found"
        
    let corners = findCorners sourcePort targetPort sBox tBox

    { Id = ConnectionId(uuid ())
      SourcePort = sourcePort.Id
      TargetPort = targetPort.Id
      IsError = sourcePort.Width <> targetPort.Width
      Width = if sourcePort.Width = targetPort.Width then int sourcePort.Width else 3 // If there is an error then the width is 2, to make a thick red wire.
      IsDragging = false
      BoundingBoxes = createBoundingBoxes corners
      Corners = corners
      DraggedCornerIndex = 1
      LastDragPos = { X = 0.; Y = 0. }
      IsHighlighted = false}

let init n () =
    let symbols, cmd = Symbol.init ()

    let symIds =
        List.map (fun (sym: Symbol.Symbol) -> sym.Id) symbols

    let rng = Random 0

    let makeRandomWire () =
        let n = symIds.Length

        let s1, s2 =
            match rng.Next(0, n - 1), rng.Next(0, n - 2) with
            | r1, r2 when r1 = r2 -> symbols.[r1], symbols.[n - 1]
            | r1, r2 -> symbols.[r1], symbols.[r2]

        let source =
            List.find (fun port -> port.PortType = PortType.Output) s1.Ports

        let target =
            List.find (fun port -> port.PortType = PortType.Input) s2.Ports

        createWire source.Id target.Id symbols

    List.map (fun i -> makeRandomWire ()) [ 1 .. n ]
    |> (fun wires ->
        { Wires = wires
          Symbols = symbols
          Colour = Blue },
        Cmd.none)

/// Given a model and a position on the page (mouse position), returns the first wire that the mouse is on,
/// along with the index of the segment of that wire that the mouse is on.
let tryFindClickedWire (pagePos: XYPos) (model: Model): Wire option =
    model.Wires
    |> List.tryFind (fun wire ->
        let foundBoxes =
            List.tryFind (fun boundingBox -> boxContainsPoint boundingBox.Box pagePos) wire.BoundingBoxes

        foundBoxes <> None)

/// Given a wire, tries to find and return the index of the segment that was clicked. If there are none, returns None.
let tryFindClickedSegment (pagePos: XYPos) (wire: Wire): int option =
    let segmentIndex =
        wire.BoundingBoxes
        |> List.mapi (fun index boundingBox -> (index, boundingBox))
        |> List.tryFind (fun (_, boundingBox) -> boxContainsPoint boundingBox.Box pagePos)

    match segmentIndex with
    | Some segment -> Some(fst segment + 1)
    | None -> None


/// For wire segments, to choose direction to move
type SegmentOrientation =
    | Vertical
    | Horizontal

/// Determine orientation of given segment
let segmentOrientation (corners: XYPos list) (index: int): SegmentOrientation =
    let corner1 = corners.[index]
    let corner2 = corners.[index + 1]

    if corner1.X = corner2.X && corner1.Y <> corner2.Y
    then Vertical
    else Horizontal

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbols
        
        { model with
            Symbols = sm
            Wires =
                model.Wires
                |> List.map (fun wire ->
                    (let corners =
                        let source = findPort model.Symbols wire.SourcePort
                        let target = findPort model.Symbols wire.TargetPort
                        
                        if source.IsDragging || target.IsDragging
                        then findCorners source.Pos target.Pos source.ParentHeight target.ParentHeight
                        else wire.Corners
                     { wire with
                           Corners = corners
                           BoundingBoxes = createBoundingBoxes wire.Corners  }))
                 }, Cmd.map Symbol sCmd
    | CreateConnection (source, target) ->
        { model with
              Wires = (createWire source target) :: model.Wires },
        Cmd.none
    | SetColour c -> { model with Colour = c }, Cmd.none
    | StartDragging (wireId, pagePos) ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire ->
                let source = findPort model.Symbols wire.SourcePort
                let target = findPort model.Symbols wire.TargetPort

                let corners =
                  if source.IsDragging || target.IsDragging
                  then findCorners source.Pos target.Pos source.ParentHeight target.ParentHeight
                  else wire.Corners

                let boundingBoxes = createBoundingBoxes corners

                if wireId <> wire.Id then
                    { wire with
                        LastDragPos = pagePos
                        Corners = corners
                        BoundingBoxes = boundingBoxes }
                else
                let i =
                    tryFindClickedSegment
                        pagePos
                        { wire with
                            Corners = corners
                            BoundingBoxes = boundingBoxes }

                match i with
                | Some i ->
                    { wire with
                        LastDragPos = pagePos
                        IsDragging = true
                        Corners = corners
                        DraggedCornerIndex = i
                        BoundingBoxes = boundingBoxes }
                | None ->
                    { wire with
                        LastDragPos = pagePos
                        Corners = corners
                        BoundingBoxes = boundingBoxes })
            Symbols =
                model.Symbols
                |> List.map (fun symbol ->
                    let wire =
                        model.Wires
                        |> List.find (fun wire -> wireId = wire.Id)

                    let sp = findPort model.Symbols wire.SourcePort
                    let tp = findPort model.Symbols wire.TargetPort

                    if (symbol.Id <> sp.HostId && symbol.Id <> tp.HostId) then
                        symbol
                    else
                        { symbol with
                            Ports = List.map (fun port -> { port with IsDragging = false }) symbol.Ports }) },
        Cmd.none

    | Dragging (wireId, pagePos) ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire ->
                      if wireId <> wire.Id || not wire.IsDragging then
                          wire
                      else
                          let i = wire.DraggedCornerIndex
                          let diff = posDiff pagePos wire.LastDragPos
                          let orientation = segmentOrientation wire.Corners i

                          printf
                              "Corners: index %d and %d, (%A and %A)."
                              i
                              (i + 1)
                              wire.Corners.[i]
                              wire.Corners.[i + 1]

                          printf "Segment orientation: %A" orientation

                          let adjusted =
                              match orientation with
                              | Vertical ->
                                  { wire.Corners.[i] with
                                        X = wire.Corners.[i].X + diff.X },
                                  { wire.Corners.[i + 1] with
                                        X = wire.Corners.[i + 1].X + diff.X }

                              | Horizontal ->
                                  { wire.Corners.[i] with
                                        Y = wire.Corners.[i].Y + diff.Y },
                                  { wire.Corners.[i + 1] with
                                        Y = wire.Corners.[i + 1].Y + diff.Y }

                          let corners =
                              wire.Corners.[..i - 1]
                              @ [ fst adjusted; snd adjusted ]
                                @ wire.Corners.[i + 2..]

                          { wire with
                                /// Change this to find the two corners that the mouse was between, then move
                                /// those corners only.
                                Corners = corners
                                BoundingBoxes = createBoundingBoxes corners
                                LastDragPos = pagePos }) },
        Cmd.none

    | EndDragging wireId ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if wireId <> wire.Id then wire else { wire with IsDragging = false }) },
        Cmd.none
    | SelectWires wireIds ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if List.contains wire.Id wireIds then { wire with IsHighlighted = true } else wire) },
        Cmd.none
    | DeselectWires wireIds ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if List.contains wire.Id wireIds then { wire with IsHighlighted = false } else wire) },
        Cmd.none
    | DeleteWires wireIds ->
        { model with
            Wires =
                model.Wires
                |> List.filter (fun wire -> not <| List.contains wire.Id wireIds) },
        Cmd.none
    | DeleteSymbols sIds ->
        printfn "DeleteSymbols in BusWire received"
        model, Cmd.ofMsg (Symbol (Symbol.DeleteSymbols sIds))
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | _ -> model, Cmd.none
