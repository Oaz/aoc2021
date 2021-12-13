module aoc2021.Core.D09T

open NUnit.Framework
open D09
open Tools

[<SetUp>]
let Setup () = ()

[<Test>]
let TestExample () =
  let input =
    [ "2199943210"
      "3987894921"
      "9856789892"
      "8767896789"
      "9899965678" ]

  let heightMap = HeightMap.ReadFrom input

  Assert.AreEqual(
    { Heights =
        array2D [ [ 2; 1; 9; 9; 9; 4; 3; 2; 1; 0 ]
                  [ 3; 9; 8; 7; 8; 9; 4; 9; 2; 1 ]
                  [ 9; 8; 5; 6; 7; 8; 9; 8; 9; 2 ]
                  [ 8; 7; 6; 7; 8; 9; 6; 7; 8; 9 ]
                  [ 9; 8; 9; 9; 9; 6; 5; 6; 7; 8 ] ]
      LowPoints =
        [ Position(1,0)
          Position(9,0)
          Position(2,2)
          Position(6,4) ]
      Basins =
        [ set [ Position(0,0)
                Position(0,1)
                Position(1,0) ]
          set [ Position(5,0)
                Position(6,0)
                Position(6,1)
                Position(7,0)
                Position(8,0)
                Position(8,1)
                Position(9,0)
                Position(9,1)
                Position(9,2) ]
          set [ Position(0,3)
                Position(1,2)
                Position(1,3)
                Position(1,4)
                Position(2,1)
                Position(2,2)
                Position(2,3)
                Position(3,1)
                Position(3,2)
                Position(3,3)
                Position(4,1)
                Position(4,2)
                Position(4,3)
                Position(5,2) ]
          set [ Position(5,4)
                Position(6,3)
                Position(6,4)
                Position(7,2)
                Position(7,3)
                Position(7,4)
                Position(8,3)
                Position(8,4)
                Position(9,4) ] ] },
    heightMap
  )

  Assert.AreEqual(15, heightMap.Risks(heightMap.LowPoints) |> Seq.sum)
  Assert.AreEqual(1134, heightMap.MultiplyThreeBiggestBasinsSize)

[<Test>]
let TestParts () =
  let input = inputForDay 9 |> splitLines
  let heightMap = HeightMap.ReadFrom input
  Assert.AreEqual(498, heightMap.Risks(heightMap.LowPoints) |> Seq.sum)
  Assert.AreEqual(1071000, heightMap.MultiplyThreeBiggestBasinsSize)
