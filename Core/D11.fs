module aoc2021.Core.D11

open Tools
open Geom2D

type Position = Coords
let adjacent (p:Position) = p.AdjacentCoordsHVD |> Zone(Coords(0,0),Coords(9,9)).Filter

type Octopus =
  { Level: int }
  member this.Flashing: bool = this.Level = 0
  member this.Increase: Octopus = { Level = (this.Level + 1) % 10 }
  member this.IncreaseIfNotFlashing: Octopus = if this.Flashing then this else this.Increase

type Cavern = Map<Position, Octopus>
let readCavernFrom (ss: string list) : Cavern =
  let makeOctopus y x c : Position * Octopus =
    (Position(x,y), { Level = int c - int '0' })
  let readLine (y: int) (s: string) : (Position * Octopus) seq = Seq.mapi (makeOctopus y) s
  Seq.mapi readLine ss
  |> Seq.collect id
  |> Map.ofSeq

let flashingIn (cavern: Cavern) : Position list =
  let hasFlash (_: Position) (o: Octopus) = o.Flashing
  Map.filter hasFlash cavern
  |> Map.keys
  |> Seq.cast
  |> List.ofSeq

type Frame =
  { Cavern: Cavern
    NewFlashes: Position list
    AllFlashes: Position list }

let initialFrame (cavern: Cavern) : Frame =
  { Cavern = cavern
    NewFlashes = []
    AllFlashes = [] }
let newFrame (step: Frame) (cavern: Cavern) (flashes : Position list) : Frame = 
  { Cavern = cavern
    NewFlashes = flashes
    AllFlashes = List.concat [ step.AllFlashes; flashes ] }
let firstIncreaseEnergyLevel (frame: Frame) : Frame =
  let newCavern = Map.map (fun _ (o: Octopus) -> o.Increase) frame.Cavern
  newFrame frame newCavern (flashingIn newCavern)
let propagateFlashes (frame: Frame) : Frame = 
  let targets = List.collect adjacent frame.NewFlashes
  let increaseExceptIfAlreadyFlashing (c: Cavern) (p: Position) = c.Add(p, (c.Item p).IncreaseIfNotFlashing)
  let newCavern = List.fold increaseExceptIfAlreadyFlashing frame.Cavern targets
  newFrame frame newCavern (List.except (flashingIn frame.Cavern) (flashingIn newCavern))
let nextFrame (frame: Frame) : Frame =
  firstIncreaseEnergyLevel frame
  |> iterate propagateFlashes
  |> Seq.skipWhile (fun s -> s.NewFlashes.Length > 0)
  |> Seq.head

let fastForward (n: int) (cavern: Cavern) : Frame =
  cavern
  |> initialFrame
  |> iterate nextFrame
  |> Seq.skip (n - 1)
  |> Seq.head

let firstMegaFlash (cavern: Cavern) : int =
  let mutable f = initialFrame cavern
  let mutable n = 0
  while (flashingIn f.Cavern |> List.length) < 100 do
    f <- nextFrame f
    n <- n + 1
  n
