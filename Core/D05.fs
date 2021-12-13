module aoc2021.Core.D05

open System
open Tools
open Geom2D

type Point = Coords

type Line =
  { P1 : Point
    P2 : Point }
  static member ReadFrom (s:string) : Line =
    let ps = splitOnSpaces s in { P1 = Point.ReadFrom ps.Head; P2 = Point.ReadFrom ps.Tail.Tail.Head }
  member this.Xs () : int list = [ this.P1.X .. Math.Sign (this.P2.X - this.P1.X) .. this.P2.X ]
  member this.Ys () : int list = [ this.P1.Y .. Math.Sign (this.P2.Y - this.P1.Y) .. this.P2.Y ]

let allHVPoints (line:Line) : Point list =
  if line.P1.X = line.P2.X then
    List.map (fun y -> Point(line.P1.X,y)) (line.Ys())
  else
    if line.P1.Y = line.P2.Y then
      List.map (fun x -> Point(x,line.P1.Y)) (line.Xs())
    else
      List.empty

let allPoints (line:Line) : Point list =
  let hvPoints = allHVPoints line
  if hvPoints.Length = 0 then
    List.zip (line.Xs()) (line.Ys()) |> List.map Point
  else
    hvPoints

type Intersections = Map<Point,int>
let findIntersections (pointFinder: Line -> Point list) (lines : Line list) : Intersections =
  let addPoint (counters:Intersections) (point:Point) : Intersections =
    let currentCount = counters.TryFind(point) |> Option.defaultValue 0
    counters.Add(point, currentCount+1)
  let addLine (counts:Intersections) (line:Line) : Intersections = List.fold addPoint counts (pointFinder line)
  List.fold addLine (Intersections []) lines

let countIntersections (counters : Intersections) : int =
  Seq.where (fun x -> x>1) counters.Values |> Seq.length
