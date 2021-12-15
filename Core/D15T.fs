module aoc2021.Core.D15T

open NUnit.Framework
open D15
open Tools
open Geom2D

let input = [
  "1163751742"
  "1381373672"
  "2136511328"
  "3694931569"
  "7463417111"
  "1319128137"
  "1359912421"
  "3125421639"
  "1293138521"
  "2311944581"
]

[<Test>]
let TestExample () =
  let cave = readCave input
  Assert.AreEqual(40, cave |> findLowerRisk)
  Assert.AreEqual(315, cave |> makeItBig |> findLowerRisk)

[<Test>]
let TestParts () =
  let cave = inputForDay 15 |> splitLines |> readCave
  Assert.AreEqual(583, cave |> findLowerRisk)
  Assert.AreEqual(2927, cave |> makeItBig |> findLowerRisk)



