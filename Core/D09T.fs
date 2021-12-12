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
        [ { X = 1; Y = 0 }
          { X = 9; Y = 0 }
          { X = 2; Y = 2 }
          { X = 6; Y = 4 } ]
      Basins =
        [ set [ { X = 0; Y = 0 }
                { X = 0; Y = 1 }
                { X = 1; Y = 0 } ]
          set [ { X = 5; Y = 0 }
                { X = 6; Y = 0 }
                { X = 6; Y = 1 }
                { X = 7; Y = 0 }
                { X = 8; Y = 0 }
                { X = 8; Y = 1 }
                { X = 9; Y = 0 }
                { X = 9; Y = 1 }
                { X = 9; Y = 2 } ]
          set [ { X = 0; Y = 3 }
                { X = 1; Y = 2 }
                { X = 1; Y = 3 }
                { X = 1; Y = 4 }
                { X = 2; Y = 1 }
                { X = 2; Y = 2 }
                { X = 2; Y = 3 }
                { X = 3; Y = 1 }
                { X = 3; Y = 2 }
                { X = 3; Y = 3 }
                { X = 4; Y = 1 }
                { X = 4; Y = 2 }
                { X = 4; Y = 3 }
                { X = 5; Y = 2 } ]
          set [ { X = 5; Y = 4 }
                { X = 6; Y = 3 }
                { X = 6; Y = 4 }
                { X = 7; Y = 2 }
                { X = 7; Y = 3 }
                { X = 7; Y = 4 }
                { X = 8; Y = 3 }
                { X = 8; Y = 4 }
                { X = 9; Y = 4 } ] ] },
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
