module aoc2021.Core.D14T

open NUnit.Framework
open D14
open Tools

let input =
    [ "NNCB"
      ""
      "CH -> B"
      "HH -> N"
      "CB -> H"
      "NH -> C"
      "HB -> C"
      "HC -> B"
      "HN -> C"
      "NN -> C"
      "BH -> H"
      "NC -> B"
      "NB -> B"
      "BN -> B"
      "BB -> N"
      "BC -> B"
      "CC -> N"
      "CN -> C" ]

[<Test>]
let TestExample () =
  let polymer,rules = readInstructions input
  Assert.AreEqual(
    {
      Pairs = Map.ofList [
                (('N','N'),1)
                (('N','C'),1)
                (('C','B'),1)
              ]
      Missing = 'B','N'
    }, polymer)
  Assert.AreEqual(16, rules.Count)
  Assert.AreEqual(
    Map.ofList [
      ('N',2)
      ('C',1)
      ('B',1)
    ], quantities polymer)
  
  Assert.AreEqual(
    {
      Pairs = Map.ofList [
                  (('N','N'),0)
                  (('N','C'),0)
                  (('C','B'),0)
                  (('N','C'),1)
                  (('C','N'),1)
                  (('N','B'),1)
                  (('B','C'),1)
                  (('C','H'),1)
                  (('H','B'),1)
              ]
      Missing = 'B','N'
    }, step rules polymer)
  Assert.AreEqual(7, step rules polymer |> polymerLength)
  Assert.AreEqual(
    Map.ofList [
      ('N',2)
      ('C',2)
      ('B',2)
      ('H',1)
    ], step rules polymer |> quantities)
  
  Assert.AreEqual(13, multiStep 2 rules polymer |> polymerLength)
  Assert.AreEqual(97, multiStep 5 rules polymer |> polymerLength)
  let s10 = multiStep 10 rules polymer
  Assert.AreEqual(3073, s10 |> polymerLength)
  Assert.AreEqual(
    Map.ofList [
      ('N',865)
      ('C',298)
      ('B',1749)
      ('H',161)
    ], s10 |> quantities)
  Assert.AreEqual(1588, s10 |> quantities |> score)
  Assert.AreEqual(2188189693529L, multiStep 40 rules polymer |> quantities |> score)

[<Test>]
let TestParts () =
  let input = inputForDay 14 |> splitLines
  let polymer,rules = readInstructions input
  Assert.AreEqual(2194, multiStep 10 rules polymer |> quantities |> score)
  Assert.AreEqual(2360298895777L, multiStep 40 rules polymer |> quantities |> score)
