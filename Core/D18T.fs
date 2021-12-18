module aoc2021.Core.D18T

open NUnit.Framework
open D18
open Tools

let readExamples =
  [ TestCaseData("01","[1,2]", Pair (Leaf 1,Leaf 2))
    TestCaseData("02","[[1,2],3]", Pair( Pair (Leaf 1,Leaf 2), Leaf 3))
    TestCaseData("03","[9,[8,7]]", Pair( Leaf 9, Pair (Leaf 8,Leaf 7)))
    TestCaseData("04","[[1,9],[8,5]]", Pair (Pair (Leaf 1,Leaf 9),Pair (Leaf 8,Leaf 5)))
    TestCaseData(
      "05","[[[[1,2],[3,4]],[[5,6],[7,8]]],9]",
      Pair (Pair (Pair (Pair (Leaf 1,Leaf 2),Pair (Leaf 3,Leaf 4)),Pair (Pair (Leaf 5,Leaf 6),Pair (Leaf 7,Leaf 8))),Leaf 9)
    )
  ]

[<TestCaseSource("readExamples")>]
let TestExampleRead (_:string) (input:string) (expectedResult:Number) =
    Assert.AreEqual(expectedResult, readNumber input)

[<TestCase("01", "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")>]
[<TestCase("02", "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")>]
[<TestCase("03", "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")>]
[<TestCase("04", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")>]
[<TestCase("05", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
[<TestCase("06", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")>]
[<TestCase("07", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[?,[0,=]]],[1,1]]")>]
[<TestCase("08", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
[<TestCase("09", "[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]", "[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]")>]
[<TestCase("10", "[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]", "[[[[3,0],[5,3]],[4,4]],[5,5]]")>]
let TestExampleExplode (_:string) (input:string) (expectedResult:string) =
    Assert.AreEqual((readNumber expectedResult,true), readNumber input |> explode)

[<TestCase("01", "[[[[0,9],2],3],4]")>]
[<TestCase("02", "[7,[6,[5,[7,0]]]]")>]
[<TestCase("03", "[[6,[5,[7,0]]],3]")>]
[<TestCase("05", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
let TestExampleNothingToExplode (_:string) (input:string) =
    let n = readNumber input
    Assert.AreEqual((n,false), explode n)

[<TestCase("01", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", false)>]
[<TestCase("02", "[[[[0,7],4],[?,[0,=]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,=]]],[1,1]]", true)>]
[<TestCase("03", "[[[[0,7],4],[[7,8],[0,=]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", true)>]
[<TestCase("04", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", false)>]
let TestExampleSplit (_:string) (input:string) (expectedNumber:string) (expectedSplit:bool) =
    Assert.AreEqual((readNumber expectedNumber,expectedSplit), readNumber input |> split)
    
[<TestCase("01", "[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
let TestExampleAdd (_:string) (i1:string) (i2:string) (expectedResult:string) =
    let n1 = readNumber i1
    let n2 = readNumber i2
    Assert.AreEqual(readNumber expectedResult, add n1 n2)

let sumExamples =
  [ TestCaseData(
    "01", [ "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]" ], "[[[[1,1],[2,2]],[3,3]],[4,4]]")
    TestCaseData(
    "02", [ "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]"
            "[5,5]" ], "[[[[3,0],[5,3]],[4,4]],[5,5]]")
    TestCaseData(
    "03", [ "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]"
            "[5,5]"
            "[6,6]" ], "[[[[5,0],[7,4]],[5,5]],[6,6]]")
    TestCaseData(
    "04", [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
            "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
            "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
            "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
            "[7,[5,[[3,8],[1,4]]]]"
            "[[2,[2,2]],[8,[8,1]]]"
            "[2,9]"
            "[1,[[[9,3],9],[[9,0],[0,7]]]]"
            "[[[5,[7,4]],7],1]"
            "[[[[4,2],2],6],[8,7]]" ], "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
  ]
  
[<TestCaseSource("sumExamples")>]
let TestExampleSum (_:string) (input:string list) (expectedResult:string) =
    let ns = List.map readNumber input
    Assert.AreEqual(readNumber expectedResult, sum ns)
    
    
[<TestCase("01", "[[1,2],[[3,4],5]]", 143)>]
[<TestCase("02", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
[<TestCase("03", "[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)>]
[<TestCase("04", "[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)>]
[<TestCase("05", "[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)>]
[<TestCase("05", "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
let TestExampleMagnitude (_:string) (input:string) (expectedResult:int) =
    Assert.AreEqual(expectedResult, readNumber input |> magnitude)

let fullAssignment =
  [ TestCaseData(
    "01", [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
            "[[[5,[2,8]],4],[5,[[9,9],0]]]"
            "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
            "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
            "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
            "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
            "[[[[5,4],[7,7]],8],[[8,3],8]]"
            "[[9,3],[[9,9],[6,[4,9]]]]"
            "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
            "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" ],
    "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]", 4140, 3993)
  ]
  
[<TestCaseSource("fullAssignment")>]
let TestFullAssignment (_:string) (input:string list) (expectedResult:string) (expectedMagnitude:int) (expectedLargestMagnitude:int) =
    let ns = List.map readNumber input
    let s = sum ns
    Assert.AreEqual(readNumber expectedResult, s)
    Assert.AreEqual(expectedMagnitude, magnitude s)
    Assert.AreEqual(expectedLargestMagnitude, largestMagnitude ns)

[<Test>]
let TestParts () =
  let numbers = inputForDay 18 |> splitLines |> List.map readNumber
  Assert.AreEqual(3216, numbers |> sum |> magnitude)
  Assert.AreEqual(4643, numbers |> largestMagnitude)

