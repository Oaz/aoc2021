module aoc2021.D07

open System
open Tools

type Position = int
type Fuel = int

let readInput (input: string) : Position list =
  splitOnComma input |> List.map Position.Parse

let crabFuel1 (start: Position) (stop: Position) : Fuel = Math.Abs(stop - start)

let crabFuel2 (start: Position) (stop: Position) : Fuel =
  let distance = Math.Abs(stop - start) in (distance * (distance + 1)) / 2

let evalFuel (crabFuel: Position -> Position -> Fuel) (crabs: Position list) (target: Position) : Fuel =
  List.map (fun crab -> crabFuel crab target) crabs
  |> List.sum

let findLeastFuel (crabFuel: Position -> Position -> Fuel) (crabs: Position list) : Position =
  let sorted = List.sort crabs
  List.sortBy (evalFuel crabFuel crabs) [ (List.head sorted) .. (List.last sorted) ]
  |> List.head
