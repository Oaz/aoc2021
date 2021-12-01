module aoc2021.D01T

open System
open NUnit.Framework
open aoc2021

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
  let input =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

  Assert.AreEqual(7, D01.CountIncreases input)

  Assert.AreEqual(
    5,
    D01.SumAcrossThreeMeasurementsSlidingWindow input
    |> D01.CountIncreases
  )

[<Test>]
let TestParts () =
  let input =
    Tools.inputForDay 1
    |> Tools.splitLines
    |> List.map Int32.Parse

  Assert.AreEqual(1233, D01.CountIncreases input)

  Assert.AreEqual(
    1275,
    D01.SumAcrossThreeMeasurementsSlidingWindow input
    |> D01.CountIncreases
  )
