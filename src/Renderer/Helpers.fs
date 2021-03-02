module Helpers

open System
open Fable.Core.JsInterop

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos = { X: float; Y: float }

type BoundingBox = { P1: XYPos; P2: XYPos }


type MouseOp =
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move
    /// Move with button Down
    | Drag

type MouseT = { Pos: XYPos; Op: MouseOp }

type KeyOp = KeyDown | KeyUp

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid (): string = import "v4" "uuid"

/// Assuming that boundingBox.P1 is always top left and boundingBox.P2 is always bottom right.
let containsPoint (boundingBox: BoundingBox) (pagePos: XYPos): bool =
    let p1 = boundingBox.P1
    let p2 = boundingBox.P2

    let xCondition = pagePos.X > p1.X && pagePos.X < p2.X
    let yCondition = pagePos.Y > p1.Y && pagePos.Y < p2.Y
    // if xCondition && yCondition then
    // printf "%A contains the point %A." boundingBox pagePos
    xCondition && yCondition
    
let containsBox (innerBox: BoundingBox) (outerBox: BoundingBox) : bool =
    containsPoint outerBox innerBox.P1 && containsPoint outerBox innerBox.P2

/// Calculate distance from centre of box for now, not from closest point
let distanceFromPoint (box: BoundingBox) (pagePos: XYPos) : float =
    if containsPoint box pagePos then 0.
    else
        let closestX =
            if pagePos.X > box.P2.X then box.P2.X
            elif pagePos.X < box.P1.X then box.P1.X
            else pagePos.X
        let closestY =
            if pagePos.Y > box.P2.Y then box.P2.Y
            elif pagePos.Y < box.P1.Y then box.P1.Y
            else pagePos.Y
            
        let x = Math.Abs (closestX - pagePos.X)
        let y = Math.Abs (closestY - pagePos.Y)
        Math.Sqrt <| (x ** 2.) + (y ** 2.)

//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS () =
    System.DateTime.Now.Ticks |> (fun t -> t / 10L)

type Stats =
    { Min: float
      Max: float
      Av: float
      Num: float }

/// add time t to st
let addTimeToStats (t: float) (st: Stats) =
    { Min = min st.Min t
      Max = max st.Max t
      Av = (st.Av * st.Num + t) / (st.Num + 1.)
      Num = st.Num + 1. }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string, Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a): 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS ()
    let execTime () = float (timeNowInMicroS () - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1

    while execTime () < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again

    let t = execTime () / float iterations

    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue { Min = t; Max = t; Av = t; Num = 1. }
        |> (fun st -> Map.add name st executionStats)

    res

/// print
let printStats () =
    executionStats
    |> Map.toList
    |> List.iter (fun (name, st) ->
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))

    executionStats <- Map []

//--------------------------------Constants----------------------------------//

/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions: XYPos = { X = 1000.; Y = 1000. }
