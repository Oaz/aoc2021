module aoc2021.Core.D05T

open NUnit.Framework
open D05

[<SetUp>]
let Setup () = ()

let allPointsHVCases =
  [ TestCaseData({ P1=Point(1,1); P2=Point(1,3) }, [Point(1,1); Point(1,2); Point(1,3)])
    TestCaseData({ P1=Point(9,7); P2=Point(7,7) }, [Point(9,7); Point(8,7); Point(7,7)]) ]
[<TestCaseSource("allPointsHVCases")>]
let TestAllHVPointsInLine (line: Line) (points: Point list) =
  Assert.AreEqual(points, allHVPoints line)
  Assert.AreEqual(points, allPoints line)

let allPointsDiagCases =
  [ TestCaseData({ P1=Point(1,1); P2=Point(3,3) }, [Point(1,1); Point(2,2); Point(3,3)])
    TestCaseData({ P1=Point(9,7); P2=Point(7,9) }, [Point(9,7); Point(8,8); Point(7,9)]) ]
[<TestCaseSource("allPointsDiagCases")>]
let TestAllDiagPointsInLine (line: Line) (points: Point list) =
  let empty : Point list = []
  Assert.AreEqual(empty, allHVPoints line)
  Assert.AreEqual(points, allPoints line)

let sampleLines : Line list = [
  { P1=Point(0,9); P2=Point(5,9) }; 
  { P1=Point(8,0); P2=Point(0,8) }; 
  { P1=Point(9,4); P2=Point(3,4) }; 
  { P1=Point(2,2); P2=Point(2,1) }; 
  { P1=Point(7,0); P2=Point(7,4) }; 
  { P1=Point(6,4); P2=Point(2,0) }; 
  { P1=Point(0,9); P2=Point(2,9) }; 
  { P1=Point(3,4); P2=Point(1,4) }; 
  { P1=Point(0,0); P2=Point(8,8) }; 
  { P1=Point(5,5); P2=Point(8,2) }; 
]

[<Test>]
let TestReadExample () =
  let input = [
    "0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"
  ]
  Assert.AreEqual(sampleLines, List.map Line.ReadFrom input)

let pointsCountCases =
  [ TestCaseData([
    0;0;0;0;0;0;0;1;0;0;
    0;0;1;0;0;0;0;1;0;0;
    0;0;1;0;0;0;0;1;0;0;
    0;0;0;0;0;0;0;1;0;0;
    0;1;1;2;1;1;1;2;1;1;
    0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;0;0;
    2;2;2;1;1;1;0;0;0;0;
  ], allHVPoints, 5)
    TestCaseData([
    1;0;1;0;0;0;0;1;1;0;
    0;1;1;1;0;0;0;2;0;0;
    0;0;2;0;1;0;1;1;1;0;
    0;0;0;1;0;2;0;2;0;0;
    0;1;1;2;3;1;3;2;1;1;
    0;0;0;1;0;2;0;0;0;0;
    0;0;1;0;0;0;1;0;0;0;
    0;1;0;0;0;0;0;1;0;0;
    1;0;0;0;0;0;0;0;1;0;
    2;2;2;1;1;1;0;0;0;0;
  ], allPoints, 12) ]

[<TestCaseSource("pointsCountCases")>]
let TestCountsExample (expected:int list) (transform:Line -> Point list) (expectedIntersections : int) =  
  let addCount (counts:Intersections) ((index,value):int*int) : Intersections =
    if value = 0 then counts else counts.Add(Point(index%10,index/10), value)
  Assert.AreEqual(List.fold addCount (Intersections []) (List.indexed expected), findIntersections transform sampleLines)
  Assert.AreEqual(expectedIntersections, findIntersections transform sampleLines |> countIntersections)

[<Test>]
let TestParts () =
  let input = Tools.inputForDay 5 |> Tools.splitLines
  let lines = List.map Line.ReadFrom input
  Assert.AreEqual(4745, findIntersections allHVPoints lines |> countIntersections)
  Assert.AreEqual(18442, findIntersections allPoints lines |> countIntersections)

