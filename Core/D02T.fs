module aoc2021.Core.D02T

open NUnit.Framework
open D02

[<SetUp>]
let Setup () = ()

let parseCommandCases =
  [ TestCaseData("down 5", Down 5)
    TestCaseData("forward 8", Forward 8)
    TestCaseData("up 3", Up 3) ]

[<TestCaseSource("parseCommandCases")>]
let TestParseCommand (input: string) (expectedCommand: Command) =
  Assert.AreEqual(expectedCommand, Parse input)

[<Test>]
let TestExample () =
  let input: string list =
    [ "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2" ]

  Assert.AreEqual(150, Start |> Travel Interpret1 input |> Result)
  Assert.AreEqual(900, Start |> Travel Interpret2 input |> Result)

[<Test>]
let TestParts () =
  let input =
    Tools.inputForDay 2
    |> Tools.splitLines

  Assert.AreEqual(1636725, Start |> Travel Interpret1 input |> Result)
  Assert.AreEqual(1872757425, Start |> Travel Interpret2 input |> Result)
  
  
