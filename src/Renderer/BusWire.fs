module BusWire

open System
open CommonTypes
open Symbol
open Browser
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type BoundingBox = { P1: XYPos; P2: XYPos }
type WireBoundingBox = { Box: BoundingBox; Prev: XYPos }

// Add bounding boxes to each segment of the wire.
type Wire =
    { Id: ConnectionId
      SourcePort: ComponentId
      TargetPort: ComponentId
      IsError: bool
      Width: int
      IsDragging: bool
      BoundingBoxes: WireBoundingBox list
      Corners: XYPos list
      DraggedCornerIndex: int
      LastDragPos: XYPos }

type Model =
    { Symbols: Symbol.Model
      Wires: Wire list
      Colour: HighLightColour }

//----------------------------Message Type-----------------------------------//

type Msg =
    | Symbol of Symbol.Msg
    | AddWire of Port * Port
    | SetColor of HighLightColour
    | StartDraggingWire of wireId: ConnectionId * pagePos: XYPos
    | DraggingWire of wireId: ConnectionId * pagePos: XYPos
    | EndDraggingWire of wireId: ConnectionId
    | MouseMsg of MouseT

/// look up wire in the model
let wire (model: Model) (wireId: ConnectionId): Wire =
    model.Wires
    |> List.filter (fun wire -> wire.Id = wireId)
    |> List.head

/// This returns the first wire that the port is connected to, but it won't work if a port is connected to
/// multiple wires. I will fix/update this later.
let wireFromPort (model: Model) (portId: ComponentId): Wire =
    model.Wires
    |> List.filter (fun wire ->
        wire.SourcePort = portId
        || wire.TargetPort = portId)
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

    // Corner list
    [ { X = x1; Y = y1 }
      { X = xCorner1; Y = y1 }
      { X = xCorner1; Y = yMidAdaptive1 }
      { X = xMid; Y = yMidAdaptive1 }
      { X = xMid; Y = yMidAdaptive2 }
      { X = xCorner2; Y = yMidAdaptive2 }
      { X = xCorner2; Y = y2 }
      { X = x2; Y = y2 } ]

/// These boxes are in the same order as the corners, which makes it easy to pair them up with which line segment
/// lies within them.
/// The lines can be in any orientation. The top left corner is defined as the top left of
/// the former corner, and the bottom right corner is the bottom right of the latter corner.
/// However, the lines are sometimes inverted (former corner is on the right and the latter
/// is on the left). This means that I need to be more careful when either creating the boxes
/// or when checking that a point is within them.
let createBoundingBoxes (corners: XYPos list): WireBoundingBox list =
    let diff = { X = 5.; Y = 5. }
    // Assuming there will always be at least three corners on any given wire
    let (firstCorner :: secondCorner :: rest) = corners

    rest
    |> List.fold (fun boxes currentCorner ->
        (let previousBox = List.head boxes
         let p1 = previousBox.Prev
         let p2 = currentCorner
         printf "p1: %A, p2: %A" p1 p2
         /// Problem: If the corners are switched, P2 of the previous box is actually P1. So the next box is created
         /// as a huge rectangle. To fix this, I made a new field in WireBoundingBox which keeps the previous corner.
         let topLeft =
             if (p1.X < p2.X && p1.Y = p2.Y)
                || (p1.X = p2.X && p1.Y < p2.Y) then
                 p1
             else
                 p2

         let bottomRight = if topLeft = p1 then p2 else p1

         { Box =
              { P1 = posDiff topLeft diff
                P2 = posAdd bottomRight diff }
           Prev = p2 }
         :: boxes

        ))
           /// I can define the first box like this since it will always emanate from an output, meaning that it will
           /// always face to the right. I will have to change this when we add different symbol orientations.
           [ { Box = { P1 = posDiff firstCorner diff
                       P2 = posAdd secondCorner diff }
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

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView model =
    FunctionComponent.Of(fun (props: WireRenderProps) ->
        // This should be the wire with changing corners

        let corners =
            if props.Source.IsDragging || props.Target.IsDragging
            then findCorners props.Source.Pos props.Target.Pos props.Source.ParentHeight props.Target.ParentHeight
            else props.Wire.Corners
        //
        // // printf "Render: %A" props.Wire.Corners
        // // printf "Render corners: %A" corners

        let handleMouseMove =
            Hooks.useRef (fun (ev: Types.Event) ->
                let ev = ev :?> Types.MouseEvent

                DraggingWire(props.Wire.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch)

        let widthText =
            if props.WireWidth = 0 then str "Error: widths do not match" else str <| sprintf "%d" props.WireWidth

        let boxes : ReactElement list =
            createBoundingBoxes corners
            |> List.map (fun box ->
                (polygon [ SVGAttr.Points
                               (sprintf
                                   "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                    box.Box.P1.X // First corner
                                    box.Box.P1.Y
                                    box.Box.P2.X // Second corner
                                    box.Box.P1.Y
                                    box.Box.P2.X // Third corner
                                    box.Box.P2.Y
                                    box.Box.P1.X // Fourth corner
                                    box.Box.P2.Y)
                           SVGAttr.Stroke "blue"
                           SVGAttr.Fill "lightblue"
                           SVGAttr.Opacity 0.3 ] []))

        /// We use all above functions to construct all line and curve segments.
        g [ OnMouseUp(fun _ ->
                document.removeEventListener ("mousemove", handleMouseMove.current)
                EndDraggingWire props.Wire.Id |> props.Dispatch)
            OnMouseDown(fun ev ->
                StartDraggingWire(props.Wire.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch

                document.addEventListener ("mousemove", handleMouseMove.current)) ] ([
            // --------------------Port extensions-------------------------
            polyline [ SVGAttr.Points
                           (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f " corners.[0].X
                                corners.[0].Y corners.[1].X corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X
                                corners.[3].Y corners.[4].X corners.[4].Y corners.[5].X corners.[5].Y corners.[6].X
                                corners.[6].Y corners.[7].X corners.[7].Y)
                       SVGAttr.Stroke props.WireColour
                       SVGAttr.StrokeWidth
                           (str
                               (sprintf "%d"
                                <| if props.WireWidth = 0 then 3 else props.WireWidth))
                       SVGAttr.FillOpacity "0" ] []
            if props.WireWidth <> 1 then
                text [ SVGAttr.X(corners.[0].X + 6.)
                       SVGAttr.Y(corners.[0].Y - 6.)
                       SVGAttr.Stroke props.WireColour
                       SVGAttr.Fill props.WireColour ] [
                    widthText
                ]    
        ] @ boxes)
    )

let view (model: Model) (dispatch: Msg -> unit) =

    let wires =
        model.Wires
        |> List.map (fun wire ->
            // So this wire's corners are changing
            // printf "Corners: %A" wire.Corners
            // printf "Boxes: %A" wire.BoundingBoxes

            let source = findPort model.Symbols wire.SourcePort
            let target = findPort model.Symbols wire.TargetPort

            let width =
                if source.Width = target.Width then source.Width else 0

            let props =
                { key = wire.Id
                  // This is the id of the wire with changing corners
                  Wire = wire
                  Source = source
                  Target = target
                  WireColour = if not wire.IsError then model.Colour.Text() else Red.Text()
                  WireWidth = width
                  Dispatch = dispatch }

            singleWireView model props)

    let symbols =
        Symbol.view model.Symbols (fun sMsg -> dispatch (Symbol sMsg))

    g [] [ (g [] wires); symbols ]




let createWire (sourcePort: Port) (targetPort: Port): Wire =
    let corners =
        findCorners sourcePort.Pos targetPort.Pos sourcePort.ParentHeight targetPort.ParentHeight

    { Id = ConnectionId(uuid ())
      SourcePort = sourcePort.Id
      TargetPort = targetPort.Id
      IsError = sourcePort.Width <> targetPort.Width
      Width = if sourcePort.Width = targetPort.Width then int sourcePort.Width else 3 // If there is an error then the width is 2, to make a thick red wire.
      IsDragging = false
      BoundingBoxes = createBoundingBoxes corners
      Corners = corners
      DraggedCornerIndex = 0
      LastDragPos = { X = 0.; Y = 0. } }

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

        createWire source target

    List.map (fun i -> makeRandomWire ()) [ 1 .. n ]
    |> (fun wires ->
        { Wires = wires
          Symbols = symbols
          Colour = Red },
        Cmd.none)

/// The lines can be in any orientation. The top left corner is defined as the top left of
/// the former corner, and the bottom right corner is the bottom right of the latter corner.
/// However, the lines are sometimes inverted (former corner is on the right and the latter
/// is on the left). This means that I need to be more careful when either creating the boxes
/// or when checking that a point is within them.
let boxContainsPoint (boundingBox: WireBoundingBox) (pagePos: XYPos): bool =
    let p1 = boundingBox.Box.P1
    let p2 = boundingBox.Box.P2

    let xCondition = pagePos.X > p1.X && pagePos.X < p2.X
    let yCondition = pagePos.Y > p1.Y && pagePos.Y < p2.Y
    // if xCondition && yCondition then
    // printf "%A contains the point %A." boundingBox pagePos
    xCondition && yCondition

/// Given a model and a position on the page (mouse position), returns the first wire that the mouse is on,
/// along with the index of the segment of that wire that the mouse is on.
let tryFindClickedWire (pagePos: XYPos) (model: Model): Wire option =
    model.Wires
    |> List.tryFind (fun wire ->
        let foundBoxes =
            List.tryFind (fun boundingBox -> boxContainsPoint boundingBox pagePos) wire.BoundingBoxes

        foundBoxes <> None)

/// This one was made to work with options in case a wire isn't clicked but now that's not going to be the case
/// so we can simplify the implementation.
// let tryFindClickedSegment (pagePos: XYPos) (wire: Wire option) : int option =
//     match wire with
//     | Some w -> let segmentIndex =
//                     w.BoundingBoxes
//                     |> List.mapi (fun index boundingBox -> (index, boundingBox))
//                     |> List.tryFind (fun (_, boundingBox) -> boxContainsPoint boundingBox pagePos)
//                 match segmentIndex with
//                 | Some s -> Some <| fst s
//                 | None -> None
//     | None -> None

let findClickedSegment (pagePos: XYPos) (wire: Wire): int =
    printf "Finding clicked segment. Bounding boxes: %A" wire.BoundingBoxes
    printf "Mouse position: %A" pagePos

    let segmentIndex =
        wire.BoundingBoxes
        |> List.mapi (fun index boundingBox -> (index, boundingBox))
        |> List.find (fun (_, boundingBox) -> boxContainsPoint boundingBox pagePos)

    fst segmentIndex

type Orientation =
    | Vertical
    | Horizontal

let segmentOrientation (corners: XYPos list) (index: int): Orientation =
    let corner1 = corners.[index]
    let corner2 = corners.[index + 1]

    if corner1.X = corner2.X && corner1.Y <> corner2.Y
    then Vertical
    else Horizontal

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        let sm, sCmd = Symbol.update sMsg model.Symbols
        { model with Symbols = sm }, Cmd.map Symbol sCmd
    | AddWire (source, target) ->
        { model with
              Wires = (createWire source target) :: model.Wires },
        Cmd.none
    | SetColor c -> { model with Colour = c }, Cmd.none
    | StartDraggingWire (wireId, pagePos) ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire ->
                      let source = findPort model.Symbols wire.SourcePort
                      let target = findPort model.Symbols wire.TargetPort

                      if wireId <> wire.Id then
                          wire
                      else
                          let corners =
                              if source.IsDragging || target.IsDragging
                              then findCorners source.Pos target.Pos source.ParentHeight target.ParentHeight
                              else wire.Corners

                          let boundingBoxes = createBoundingBoxes corners

                          let i =
                              findClickedSegment
                                  pagePos
                                  { wire with
                                        Corners = corners
                                        BoundingBoxes = boundingBoxes }
                          // if i <> 0 && i <> boundingBoxes.Length - 1
                          // then
                          { wire with
                                LastDragPos = pagePos
                                IsDragging = true
                                Corners = corners
                                DraggedCornerIndex = i
                                BoundingBoxes = boundingBoxes }
                      // else wire
                      )
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

    | DraggingWire (wireId, pagePos) ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire ->
                      if wireId <> wire.Id then
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

    | EndDraggingWire wireId ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire -> if wireId <> wire.Id then wire else { wire with IsDragging = false }) },
        Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
