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

/// Bounding box to be used for wire segments. We need the "previous" field to keep track of the
/// segments when using List.fold, because sometimes we need to swap corners to make sure Box.P1
/// is always the top left corner.
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

/// Returns the specified port, along with its parent symbol's bounding box for wire routing purposes.
let findPortData (symbols: Symbol.Model) (portId: PortId) : (Port * BoundingBox * ComponentId) option =
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
            , symbol.BoundingBox, symbol.Id))

/// Automatically finds corners given source and target port positions and their parent symbol bounding boxes
let findCorners (source: Port) (target: Port) (sourceBox: BoundingBox) (targetBox: BoundingBox) : XYPos list =
    // Source coordinates
    let x1 = source.Pos.X
    let y1 = source.Pos.Y

    // Target coordinates
    let x2 = target.Pos.X
    let y2 = target.Pos.Y
    
    // Midpoints
    let xMid = (x1 + x2) / 2.0
    let yMid = (y1 + y2) / 2.0
    
    // Distance above and below the source and target ports, based on the parent bounding boxes
    let distAbove1 = y1 - sourceBox.P1.Y + 20.
    let distBelow1 = sourceBox.P2.Y - y1 + 20.
    
    let distAbove2 = y2 - targetBox.P1.Y + 20.
    let distBelow2 = targetBox.P2.Y - y2 + 20.
    
    // Minimum distance to go straight (right or left) from ports
    let xMin = 20.

    // Is the source on the left of the target?
    let xPositive = x2 > x1
    // Is the source below the target?
    let yPositive = y2 > y1

    // x and y distances between ports
    let xDiff = Math.Abs(x2 - x1)
    let yDiff = Math.Abs(y2 - y1)

    /// Position of the first corner: If there is enough space then xMid,
    /// otherwise minimum distance from source port
    let xCorner1 =
        /// If target is above and on the right of the source, and the x and y difference conditions are satisfied
        if xPositive && yPositive && (yDiff < distAbove1 + distBelow2 || xDiff >= 2. * xMin) then
            xMid
        /// If target is below and on the right of the source, and the x and y difference conditions (different
        /// from the ones above) are satisfied
        elif xPositive && (yDiff < distBelow1 + distAbove2 || xDiff >= 2. * xMin) then
            xMid
        /// If port locations are inverted
        else
            x1 + xMin
            
    /// Same logic as xCorner1 above
    let xCorner2 =
        if xPositive && yPositive && (yDiff < distAbove1 + distBelow2 || xDiff >= 2. * xMin) then
            xMid
        elif xPositive && (yDiff < distBelow1 + distAbove2 || xDiff >= 2. * xMin) then
            xMid
        else
            x2 - xMin

    /// Vertical line segments, similar logic to xCorner1 and xCorner2
    let yCorner1 =
        if yPositive && (yDiff >= distAbove1 || not xPositive)
        then y1 + distAbove1
        elif yDiff >= distBelow1 || not xPositive
        then y1 - distBelow1
        elif yPositive
        then y1 + yDiff
        else y1 - yDiff

    let yCorner2 =
        if yPositive && (yDiff >= distBelow2 || not xPositive)
        then y2 - distBelow2
        elif yDiff >= distAbove2 || not xPositive
        then y2 + distAbove2
        elif yPositive
        then y2 - yDiff
        else y2 + yDiff

    /// Choose between the vertical midpoint and adaptive heights based on conditions
    let yMidAdaptive1, yMidAdaptive2 =
        if (yPositive && yDiff >= distAbove1 + distBelow2) || yDiff >= distBelow1 + distAbove2
        then yMid, yMid
        else yCorner1, yCorner2


    /// Corner list, number of corners may vary depending on shape of wire.
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

// This method generates a list of bounding boxes for segments of the wire, based on its corners.
/// These boxes are in the same order as the corners, which makes figuring out the clicked segment easy.
/// 
/// topLeft is defined as the top left of the former corner, and bottomRight is the bottom right
/// of the latter corner. However, the lines are sometimes inverted (former corner is on the right and the latter
/// is on the left, or former is on the bottom and latter is on top). This means that chooseCorners must be used
/// to swap the corners if necessary. If corners are swapped, WireBoundingBox.prev always keeps the latter corner
/// so that we can generate the next bounding box.
let createBoundingBoxes (corners: XYPos list): WireBoundingBox list =
    let diff = { X = 5.; Y = 5. }
    /// Assuming there will always be at least three corners on any given wire, there is no need to match other cases.
    let (firstCorner :: secondCorner :: rest) = corners
    
    // Switching corners if necessary
    let s1, s2 = chooseCorners firstCorner secondCorner
    
    /// This will work even if rest was empty (if we had two segments, which we won't), as we have a start state.
    rest
    |> List.fold (fun boxes currentCorner ->
        (let previousBox = List.head boxes
         let p1 = previousBox.Prev
         let p2 = currentCorner
         
         // Switching corners if necessary
         let topLeft, bottomRight = chooseCorners p1 p2

         // Appending box to list of boxes so far
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
        
        /// If the width is 0, then there is an error. This is defined in the view function below.
        let widthText =
            if props.WireWidth = 0 then str "Different widths" else str <| sprintf "%d" props.WireWidth

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
            
        /// Can draw 3, 5 or 7 segments, depending on the wire position.
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
            (
                [
                    polyline [
                        SVGAttr.Points drawCorners
                        SVGAttr.Stroke props.WireColour
                        SVGAttr.StrokeWidth
                          (str (sprintf "%d" <| props.Wire.Width))
                        SVGAttr.FillOpacity "0"
                        // SVGAttr.Custom ("stroke-linejoin", "round")
                         ] []
                    /// Width legend, displayed if width <> 1
                    if props.Wire.Width <> 1 then
                        text [
                            SVGAttr.X(corners.[0].X + 6.)
                            SVGAttr.Y(corners.[0].Y - 6.)
                            SVGAttr.Stroke props.WireColour
                            SVGAttr.Fill props.WireColour
                            SVGAttr.FontSize 10 ] [
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
            let source = findPort model.Symbols wire.SourcePort
            let target = findPort model.Symbols wire.TargetPort
            
            let wireColour =
                if wire.IsHighlighted then Orange.Text()
                elif wire.IsError then Red.Text()
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
    /// Try to find the ports. If not found (shouldn't happen because they were created and do exist but still)
    /// throw an error.
    let sourcePort, sBox, targetPort, tBox =
        match findPortData symbols sourcePort, findPortData symbols targetPort with
        | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
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
      DraggedCornerIndex = 1 // This is just an initial value, does not change anything,
      LastDragPos = { X = 0.; Y = 0. }
      IsHighlighted = false}

let init () =
    let symbols, cmd = Symbol.init ()
    { Wires = []
      Symbols = symbols
      Colour = Blue },
    cmd

/// Given a wire, tries to find and return the index of the segment that was clicked. If there are none, returns None.
let tryFindClickedSegment (pagePos: XYPos) (wire: Wire): int option =
    let segmentIndex =
        wire.BoundingBoxes
        |> List.mapi (fun index boundingBox -> (index, boundingBox))
        |> List.tryFind (fun (_, boundingBox) -> containsPoint boundingBox.Box pagePos)

    match segmentIndex with
    | Some segment -> Some (fst segment)
    | None -> None

let connectionExists (model: Model) (source: PortId) (target: PortId) : bool =
    let exists =
        model.Wires
        |> List.tryFind (fun wire -> wire.SourcePort = source && wire.TargetPort = target)
    match exists with
    | Some _ -> true
    | None -> false

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
        
        /// Every time the symbol model gets updated, we need to update the bounding boxes of all wires. If we don't
        /// do this, the wire gets drawn again but the boxes don't get calculated.
        { model with
            Symbols = sm
            Wires =
                model.Wires
                |> List.map (fun wire ->
                    (let corners =
                        let source, sBox, target, tBox =
                            match findPortData sm wire.SourcePort, findPortData sm wire.TargetPort with
                            | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
                            | _ -> failwithf "Ports not found"
                            
                        /// If one of the ports has changed its location, we need to recalculate corners.
                        /// If not, we can keep the current corners. We have to do this because the wire might
                        /// be manually adjusted, in which case we won't want to recalculate corners/auto-route.
                        if source.IsPositionModified || target.IsPositionModified
                        then findCorners source target sBox tBox
                        else wire.Corners
                     { wire with
                           Corners = corners
                           BoundingBoxes = createBoundingBoxes wire.Corners  }))
                 }, Cmd.map Symbol sCmd
    | CreateConnection (source, target) ->
        if not (connectionExists model source target) then
            { model with
                  Wires = (createWire source target model.Symbols) :: model.Wires },
            Cmd.none
        else model, Cmd.none
    | SetColour c -> { model with Colour = c }, Cmd.none
    | StartDragging (wireId, pagePos) ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire ->
                let source, sBox, target, tBox =
                    match findPortData model.Symbols wire.SourcePort, findPortData model.Symbols wire.TargetPort with
                    | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
                    | _ -> failwithf "Ports not found"

                let corners =
                  if source.IsPositionModified || target.IsPositionModified
                  then findCorners source target sBox tBox
                  else wire.Corners

                let boundingBoxes = createBoundingBoxes corners

                /// If this isn't the wire being dragged, we have to still update the corners or else its position will
                /// be reset to its initial position.
                if wireId <> wire.Id then
                    { wire with
                        LastDragPos = pagePos
                        Corners = corners
                        BoundingBoxes = boundingBoxes }
                else
                let i = // Index of the clicked segment on the clicked wire
                    tryFindClickedSegment
                        pagePos
                        { wire with
                            Corners = corners
                            BoundingBoxes = boundingBoxes }

                match i with
                | Some i ->
                    /// If the end segments are clicked, select the wire but do not let the segment be dragged
                    if i = 0 || i = corners.Length - 2 then
                        { wire with
                            LastDragPos = pagePos
                            Corners = corners
                            BoundingBoxes = boundingBoxes
                            IsHighlighted = true }
                    /// Otherwise segment can be dragged    
                    else
                        { wire with
                            LastDragPos = pagePos
                            IsDragging = true
                            Corners = corners
                            DraggedCornerIndex = i
                            IsHighlighted = true
                            BoundingBoxes = boundingBoxes }                        
                | None ->
                    { wire with
                        LastDragPos = pagePos
                        Corners = corners
                        BoundingBoxes = boundingBoxes })
            /// We have to modify the symbol list every time a wire is dragged. This is because when we start dragging
            /// a wire, we need to state that its ports' positions are no longer modified. This is required to keep the
            /// dragged positions of the wires even after letting go after them, but to reset their positions (auto-route
            /// again) whenever one of its ports is dragged.
            Symbols =
                model.Symbols
                |> List.map (fun symbol ->
                    let wire =
                        model.Wires
                        |> List.find (fun wire -> wireId = wire.Id)

                    let sId, tId =
                        match findPortData model.Symbols wire.SourcePort, findPortData model.Symbols wire.TargetPort with
                        | Some (_, _, sid), Some (_, _, tid) -> sid, tid
                        | _ -> failwithf "Ports not found"

                    if (symbol.Id <> sId && symbol.Id <> tId) then
                        symbol
                    else
                        /// We set IsPositionModified to true when a symbol with this port is dragged. This is done
                        /// in Symbol.update.
                        { symbol with
                            Ports = List.map (fun port -> { port with IsPositionModified = false }) symbol.Ports }) },
        Cmd.none

    | Dragging (wireId, pagePos) ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire ->
                      if wireId <> wire.Id || not wire.IsDragging then
                          wire
                      else
                          /// Get information about which corners are dragged, and the orientation of the segment
                          /// between them.
                          let i = wire.DraggedCornerIndex
                          let diff = posDiff pagePos wire.LastDragPos
                          let orientation = segmentOrientation wire.Corners i

                          /// If a segment is vertical, only drag it in the horizontal direction.
                          /// If it is horizontal, only drag it in the vertical direction.
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
                          
                          /// Keep the corner list the same, except for the modified corners.
                          let corners =
                              wire.Corners.[..i - 1]
                              @ [ fst adjusted; snd adjusted ]
                                @ wire.Corners.[i + 2..]

                          { wire with
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
        /// Collect all ports and return their IDs
        let ports : PortId list =
            sIds
            |> List.map (symbol model.Symbols)
            |> List.collect (fun symbol -> symbol.Ports)
            |> List.map (fun port -> port.Id)
            
        /// Remove all wires originating from or ending at a port on this symbol,
        /// then send a message to symbol to remove the symbol.
        { model with
            Wires =
                model.Wires
                |> List.filter (fun wire ->
                    not (List.contains wire.SourcePort ports) &&
                    not (List.contains wire.TargetPort ports))   
        }, Cmd.ofMsg (Symbol (Symbol.DeleteSymbols sIds))
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | _ -> model, Cmd.none
