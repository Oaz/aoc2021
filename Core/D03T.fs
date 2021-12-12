module aoc2021.Core.D03T

open NUnit.Framework
open D03

[<SetUp>]
let Setup () = ()

[<TestCase("00100",4)>]
[<TestCase("11110",30)>]
let TestReadBinary (input: string) (expected: int) =
  Assert.AreEqual(expected, ReadBinary input)

[<TestCase("ABB",'B')>]
[<TestCase("ABA",'A')>]
let TestMostCommonItem (input: string) (expected: char) =
  Assert.AreEqual(expected, Seq.toList input |> CommonItem List.sortByDescending |> fst)

[<Test>]
let TestShortExample () =
  let input: string list =
    [ "00100"
      "11110"
      "01010" ]

  Assert.AreEqual([0;0;0], ReadBinaries input |> ExtractBits 0)
  Assert.AreEqual([0;1;1], ReadBinaries input |> ExtractBits 1)
  Assert.AreEqual([0;1;0], ReadBinaries input |> ExtractBits 4)

[<Test>]
let TestExample () =
  let input: string list =
    [ "00100"
      "11110"
      "10110"
      "10111"
      "10101"
      "01111"
      "00111"
      "11100"
      "10000"
      "11001"
      "00010"
      "01010" ]

  Assert.AreEqual(22, ReadBinaries input |> GammaRate)
  Assert.AreEqual(9, ReadBinaries input |> EpsilonRate)
  Assert.AreEqual(22*9, ReadBinaries input |> PowerConsumption)
  Assert.AreEqual(23, ReadBinaries input |> OxygenGeneratorRating)
  Assert.AreEqual(10, ReadBinaries input |> CO2ScrubberRating)
  Assert.AreEqual(230, ReadBinaries input |> LifeSupportRating)

[<Test>]
let TestParts () =
  let input =
    Tools.inputForDay 3
    |> Tools.splitLines

  Assert.AreEqual(3242606, ReadBinaries input |> PowerConsumption)
  Assert.AreEqual(4856080, ReadBinaries input |> LifeSupportRating)
