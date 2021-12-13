module aoc2021.Core.D13T

open NUnit.Framework
open D13
open Tools

[<SetUp>]
let Setup () = ()

let input =
    [ "6,10"
      "0,14"
      "9,10"
      "0,3"
      "10,4"
      "4,11"
      "6,0"
      "6,12"
      "4,1"
      "0,13"
      "10,12"
      "3,4"
      "3,0"
      "8,4"
      "1,10"
      "2,14"
      "8,10"
      "9,0"
      ""
      "fold along y=7"
      "fold along x=5" ]

[<Test>]
let TestExample () =
  let sheet,folds = readInstructions input
  Assert.AreEqual(17, folds.Head sheet |> List.length)

[<Test>]
let TestParts () =
  let input = inputForDay 13 |> splitLines
  let sheet,folds = readInstructions input
  Assert.AreEqual(675, folds.Head sheet |> List.length)
  Assert.AreEqual("HZKHFEJZ", foldAll (sheet,folds) |> scan)

