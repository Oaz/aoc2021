module aoc2021.Core.D10T

open NUnit.Framework
open D10
open Tools
open System

let exampleData =
  [ TestCaseData("01","[({(<(())[]>[[{[]{<()<>>", Incomplete [ Curly;Curly;Square;Square;Round;Curly;Round;Square ])
    TestCaseData("02","[(()[<>])]({[<{<<[]>>(", Incomplete [Round;Curly;Angle;Square;Curly;Round])
    TestCaseData("03","{([(<{}[<>[]}>{[]{[(<()>", Corrupted (Square,Curly))
    TestCaseData("04","(((({<>}<{<{<>}{[]{[]{}", Incomplete [Curly;Curly;Angle;Curly;Angle;Round;Round;Round;Round])
    TestCaseData("05","[[<[([]))<([[{}[[()]]]", Corrupted (Square,Round))
    TestCaseData("06","[{[{({}]{}}([{[{{{}}([]", Corrupted (Round, Square))
    TestCaseData("07","{<[[]]>}<{[{[{[]{()[[[]", Incomplete [Square;Square;Curly;Curly;Square;Curly;Square;Curly;Angle])
    TestCaseData("08","[<(<(<(<{}))><([]([]()", Corrupted (Angle, Round))
    TestCaseData("09","<{([([[(<>()){}]>(<<{{", Corrupted (Square, Angle))
    TestCaseData("10","<{([{{}}[<[[[<>{}]]]>[]]", Incomplete [Square;Round;Curly;Angle])
    TestCaseData("11","<>", Legal)
    TestCaseData("12","{()()()}", Legal)
    TestCaseData("13","[<>({}){}[([])<>]]", Legal)
    TestCaseData("14","(((((((((())))))))))", Legal)
    TestCaseData("15","))", Missing) ]

[<TestCaseSource("exampleData")>]
let TestExampleParse (_:string) (input:string) (expectedResult:Result) =
    Assert.AreEqual(expectedResult, chunk input)

let exampleDataPoints =
  [ TestCaseData(288957, Incomplete [ Curly;Curly;Square;Square;Round;Curly;Round;Square ])
    TestCaseData(5566, Incomplete [Round;Curly;Angle;Square;Curly;Round])
    TestCaseData(1480781, Incomplete [Curly;Curly;Angle;Curly;Angle;Round;Round;Round;Round])
    TestCaseData(995444, Incomplete [Square;Square;Curly;Curly;Square;Curly;Square;Curly;Angle])
    TestCaseData(294, Incomplete [Square;Round;Curly;Angle]) ]

[<TestCaseSource("exampleDataPoints")>]
let TestExampleIncompletePoints (expectedPoints:Int64) (input:Result) =
    Assert.AreEqual(Some expectedPoints, incompletePoints input)

[<Test>]
let TestExamplePoints () =
  let input =
    [ "[({(<(())[]>[[{[]{<()<>>"
      "[(()[<>])]({[<{<<[]>>("
      "{([(<{}[<>[]}>{[]{[(<()>"
      "(((({<>}<{<{<>}{[]{[]{}"
      "[[<[([]))<([[{}[[()]]]"
      "[{[{({}]{}}([{[{{{}}([]"
      "{<[[]]>}<{[{[{[]{()[[[]"
      "[<(<(<(<{}))><([]([]()"
      "<{([([[(<>()){}]>(<<{{"
      "<{([{{}}[<[[[<>{}]]]>[]]" ]

  Assert.AreEqual(26397, illegalCharacterScore input)
  Assert.AreEqual(288957, autocompleteScore input)

[<Test>]
let TestParts () =
  let input = inputForDay 10 |> splitLines
  Assert.AreEqual(369105, illegalCharacterScore input)
  Assert.AreEqual(3999363569L, autocompleteScore input)


