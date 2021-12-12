module aoc2021.Core.D07T

open NUnit.Framework
open D07

[<SetUp>]
let Setup () = ()
let input = "16,1,2,0,4,2,7,1,2,14"
let crabs = readInput input

[<TestCase(1,41)>]
[<TestCase(2,37)>]
[<TestCase(3,39)>]
[<TestCase(10,71)>]
let TestExampleComputeFuel1 (target:int) (fuel:int) =
  Assert.AreEqual(fuel, evalFuel crabFuel1 crabs target)

[<TestCase(5,168)>]
[<TestCase(2,206)>]
let TestExampleComputeFuel2 (target:int) (fuel:int) =
  Assert.AreEqual(fuel, evalFuel crabFuel2 crabs target)

[<Test>]
let TestExampleFindBestPosition () =
  Assert.AreEqual(2, findLeastFuel crabFuel1 crabs)
  Assert.AreEqual(5, findLeastFuel crabFuel2 crabs)

[<Test>]
let TestParts () =
  let crabs = Tools.inputForDay 7 |> readInput

  Assert.AreEqual(
    328318,
    findLeastFuel crabFuel1 crabs
    |> evalFuel crabFuel1 crabs
  )
  Assert.AreEqual(
    89791146,
    findLeastFuel crabFuel2 crabs
    |> evalFuel crabFuel2 crabs
  )
