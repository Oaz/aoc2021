module aoc2021.Core.D22T

open FSharp.Stats
open NUnit.Framework
open D22
open Tools

[<TestCase("x=10..12",10,12)>]
[<TestCase("z=-50..-1",-50,-1)>]
[<TestCase("y=-45373..81175",-45373,81175)>]
let TestReadInterval (input:string) (min:int) (max:int) =
  Assert.AreEqual((min,max), readInterval input)

[<Test>]
let TestReadCuboid () =
  Assert.AreEqual(
    {
     Min=Vector.ofList [-22;19;-38]
     Max=Vector.ofList [-8;23;16]
    },
    readCuboid "x=-22..-8,y=19..23,z=-38..16"
  )

[<Test>]
let TestReadOp() =
  let cuboid = {
     Min=Vector.ofList [10;10;10]
     Max=Vector.ofList [12;12;12]
  }
  Assert.AreEqual(
    On cuboid,
    readOp "on x=10..12,y=10..12,z=10..12"
  )
  Assert.AreEqual(
    Off cuboid,
    readOp "off x=10..12,y=10..12,z=10..12"
  )

[<TestCase("x=10..12,y=10..12,z=10..12","x=11..13,y=11..13,z=11..13","x=11..12,y=11..12,z=11..12")>]
[<TestCase("x=11..13,y=11..13,z=11..13","x=10..12,y=10..12,z=10..12","x=11..12,y=11..12,z=11..12")>]
[<TestCase("x=10..12,y=11..13,z=10..12","x=11..13,y=10..12,z=11..13","x=11..12,y=11..12,z=11..12")>]
[<TestCase("x=10..11,y=10..12,z=10..12","x=12..13,y=11..13,z=11..13","")>]
[<TestCase("x=10..12,y=10..11,z=10..12","x=11..13,y=12..13,z=11..13","")>]
[<TestCase("x=10..12,y=10..12,z=10..11","x=11..13,y=11..13,z=12..13","")>]
let TestIntersect (c1:string) (c2:string) (c:string) =
  Assert.AreEqual(
    (if c = "" then None else Some (readCuboid c)),
    (readCuboid c1).Intersect(readCuboid c2)
  )

let removeCaseData = [
  TestCaseData("A","x=10..11,y=10..11,z=10..11","x=12..13,y=12..13,z=12..13",["x=10..11,y=10..11,z=10..11"])
  TestCaseData("B","x=10..11,y=10..11,z=10..11","x=9..13,y=9..13,z=9..13",List.empty<string>)
  TestCaseData("C","x=10..11,y=10..11,z=10..11","x=10..13,y=10..13,z=10..13",List.empty<string>)
  TestCaseData("D","x=10..15,y=10..15,z=10..15","x=12..13,y=12..13,z=12..13",
               [
                 "x=10..15,y=10..15,z=10..11"
                 "x=10..15,y=10..15,z=14..15"
                 "x=10..15,y=10..11,z=12..13"
                 "x=10..15,y=14..15,z=12..13"
                 "x=10..11,y=12..13,z=12..13"
                 "x=14..15,y=12..13,z=12..13"
               ])
  TestCaseData("E","x=10..11,y=10..11,z=10..11","x=11..12,y=11..12,z=11..12",
               [
                 "x=10..11,y=10..11,z=10..10"
                 "x=10..11,y=10..10,z=11..11"
                 "x=10..10,y=11..11,z=11..11"
               ])
  TestCaseData("F","x=11..12,y=11..12,z=11..12","x=10..11,y=10..11,z=10..11",
               [
                 "x=11..12,y=11..12,z=12..12"
                 "x=11..12,y=12..12,z=11..11"
                 "x=12..12,y=11..11,z=11..11"
               ])
]
[<TestCaseSource("removeCaseData")>]
let TestRemoveSimple _ (c1:string) (c2:string) (cs:string list) =
  let expected = List.map readCuboid cs
  let actual = (readCuboid c1).Remove(readCuboid c2)
  Assert.AreEqual( expected, actual )

let example1 = [
  "on x=10..12,y=10..12,z=10..12"
  "on x=11..13,y=11..13,z=11..13"
  "off x=9..11,y=9..11,z=9..11"
  "on x=10..10,y=10..10,z=10..10"
]

[<Test>]
let TestSwitchOnOff () =
  let op1 = readOp example1[0]
  let r1 = apply [] op1
  Assert.AreEqual(1, r1.Length)
  Assert.AreEqual(27I, sizeOf r1)
  let op2 = readOp example1[1]
  let r2 = apply r1 op2
  Assert.AreEqual(27I+19I, sizeOf r2)
  let op3 = readOp example1[2]
  let r3 = apply r2 op3
  Assert.AreEqual(27I+19I-8I, sizeOf r3)

let example2 = [
  "on x=-20..26,y=-36..17,z=-47..7"
  "on x=-20..33,y=-21..23,z=-26..28"
  "on x=-22..28,y=-29..23,z=-38..16"
  "on x=-46..7,y=-6..46,z=-50..-1"
  "on x=-49..1,y=-3..46,z=-24..28"
  "on x=2..47,y=-22..22,z=-23..27"
  "on x=-27..23,y=-28..26,z=-21..29"
  "on x=-39..5,y=-6..47,z=-3..44"
  "on x=-30..21,y=-8..43,z=-13..34"
  "on x=-22..26,y=-27..20,z=-29..19"
  "off x=-48..-32,y=26..41,z=-47..-37"
  "on x=-12..35,y=6..50,z=-50..-2"
  "off x=-48..-32,y=-32..-16,z=-15..-5"
  "on x=-18..26,y=-33..15,z=-7..46"
  "off x=-40..-22,y=-38..-28,z=23..41"
  "on x=-16..35,y=-41..10,z=-47..6"
  "off x=-32..-23,y=11..30,z=-14..3"
  "on x=-49..-5,y=-3..45,z=-29..18"
  "off x=18..30,y=-20..-8,z=-3..13"
  "on x=-41..9,y=-7..43,z=-33..15"
]

[<Test>]
let TestExample2 () =
  Assert.AreEqual(590784I, List.map readOp example2 |> List.fold apply [] |> sizeOf)

[<Test>]
let TestParts () =
  let input = inputForDay 22 |> splitLines
  let initPhase = List.take 20 input
  Assert.AreEqual(648681I, List.map readOp initPhase |> List.fold apply [] |> sizeOf)
  Assert.AreEqual(1302784472088899I, List.map readOp input |> List.fold apply [] |> sizeOf)

