module aoc2021.Core.D09

open Tools
open Geom2D

type Position = Coords
let adjacent w h (p:Position) = p.AdjacentCoordsHV |> Zone(Coords(0,0),Coords(w-1,h-1)).Filter

type Basin = Set<Position>

type HeightMap =
  { Heights: int [,]
    LowPoints: Position seq
    Basins: Basin seq }
  static member ReadFrom(input: string list) : HeightMap =
    let charToInt c = int c - int '0'
    let readLine (input: string) : int seq = Seq.map charToInt input
    let heights = Seq.map readLine input |> array2D
    let xSize = Array2D.length2 heights
    let ySize = Array2D.length1 heights
    let getHeight (p: Position) : int = Array2D.get heights p.Y p.X

    let low y x height : Position option =
      let p = Position(x,y)

      let isLow =
        adjacent xSize ySize p
        |> Zone(Coords(0,0),Coords(xSize-1,ySize-1)).Filter
        |> List.map (fun (q: Position) -> getHeight q)
        |> List.forall (fun h -> h > height)

      if isLow then Some p else None

    let lowPoints =
      Array2D.mapi low heights
      |> Seq.cast<Position option>
      |> Seq.choose id

    let growBasin (b: Basin) : Basin =
      let adjacentPositions =
        Seq.collect (adjacent xSize ySize) b
        |> Seq.filter (fun (p: Position) -> (getHeight p) < 9)
        |> Set.ofSeq

      b + adjacentPositions

    let findBasin (lp: Position) : Basin =
      generate growBasin (set [ lp ])
      |> rollUntil (fun (b1, b2) -> b1 = b2)

    { Heights = heights
      LowPoints = lowPoints
      Basins = Seq.map findBasin lowPoints }

  member this.Risks(positions: Position seq) : int seq =
    Seq.map (fun (p: Position) -> (Array2D.get this.Heights p.Y p.X) + 1) positions

  member this.MultiplyThreeBiggestBasinsSize: int =
    Seq.map (fun (b: Basin) -> b.Count) this.Basins
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (*) 1
