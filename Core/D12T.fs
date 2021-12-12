module aoc2021.Core.D12T

open NUnit.Framework
open System
open Tools
open D12
open Graph

let input =
    [ "start-A"
      "start-b"
      "A-c"
      "A-b"
      "b-d"
      "A-end"
      "b-end" ]

[<Test>]
let TestExample1 () =

  Assert.AreEqual(
    Map.ofList [ (Start, [ Big "A"; Small "b" ])
                 (Big "A", [ Small "b"; Small "c"; End ])
                 (Small "b", [ Big "A"; Small "d"; End ])
                 (Small "c", [ Big "A" ])
                 (Small "d", [ Small "b" ]) ],
    readGraph input
  )

  Assert.AreEqual(
    List.map
      readPath
      [ "start,A,b,A,c,A,end"
        "start,A,b,A,end"
        "start,A,b,end"
        "start,A,c,A,b,A,end"
        "start,A,c,A,b,end"
        "start,A,c,A,end"
        "start,A,end"
        "start,b,A,c,A,end"
        "start,b,A,end"
        "start,b,end" ],
    readGraph input |> recordAllPaths onlyOnceInEachSmallCave
  )


[<Test>]
let TestExample1b () =
  let expected : Path<Node> list =
                  List.map
                    readPath
                      [ "start,A,b,A,b,A,c,A,end"
                        "start,A,b,A,b,A,end"
                        "start,A,b,A,b,end"
                        "start,A,b,A,c,A,b,A,end"
                        "start,A,b,A,c,A,b,end"
                        "start,A,b,A,c,A,c,A,end"
                        "start,A,b,A,c,A,end"
                        "start,A,b,A,end"
                        "start,A,b,d,b,A,c,A,end"
                        "start,A,b,d,b,A,end"
                        "start,A,b,d,b,end"
                        "start,A,b,end"
                        "start,A,c,A,b,A,b,A,end"
                        "start,A,c,A,b,A,b,end"
                        "start,A,c,A,b,A,c,A,end"
                        "start,A,c,A,b,A,end"
                        "start,A,c,A,b,d,b,A,end"
                        "start,A,c,A,b,d,b,end"
                        "start,A,c,A,b,end"
                        "start,A,c,A,c,A,b,A,end"
                        "start,A,c,A,c,A,b,end"
                        "start,A,c,A,c,A,end"
                        "start,A,c,A,end"
                        "start,A,end"
                        "start,b,A,b,A,c,A,end"
                        "start,b,A,b,A,end"
                        "start,b,A,b,end"
                        "start,b,A,c,A,b,A,end"
                        "start,b,A,c,A,b,end"
                        "start,b,A,c,A,c,A,end"
                        "start,b,A,c,A,end"
                        "start,b,A,end"
                        "start,b,d,b,A,c,A,end"
                        "start,b,d,b,A,end"
                        "start,b,d,b,end"
                        "start,b,end" ]
  let actual : Path<Node> list = readGraph input |> recordAllPaths weirdRules
  Assert.AreEqual(36, actual |> List.length)
  Assert.AreEqual(expected, actual)

[<Test>]
let TestExample2 () =
  let input =
    [ "dc-end"
      "HN-start"
      "start-kj"
      "dc-start"
      "dc-HN"
      "LN-dc"
      "HN-end"
      "kj-sa"
      "kj-HN"
      "kj-dc" ]

  Assert.AreEqual(
    List.map
      readPath
          [ "start,HN,dc,HN,kj,HN,end"
            "start,HN,dc,HN,end"
            "start,HN,dc,kj,HN,end"
            "start,HN,dc,end"
            "start,HN,kj,HN,dc,HN,end"
            "start,HN,kj,HN,dc,end"
            "start,HN,kj,HN,end"
            "start,HN,kj,dc,HN,end"
            "start,HN,kj,dc,end"
            "start,HN,end"
            "start,dc,HN,kj,HN,end"
            "start,dc,HN,end"
            "start,dc,kj,HN,end"
            "start,dc,end"
            "start,kj,HN,dc,HN,end"
            "start,kj,HN,dc,end"
            "start,kj,HN,end"
            "start,kj,dc,HN,end"
            "start,kj,dc,end" ],
    readGraph input |> recordAllPaths onlyOnceInEachSmallCave
  )
  Assert.AreEqual(103, readGraph input |> recordAllPaths weirdRules |> List.length)

[<Test>]
let TestExample3 () =
  let input =
    [ "fs-end"
      "he-DX"
      "fs-he"
      "start-DX"
      "pj-DX"
      "end-zg"
      "zg-sl"
      "zg-pj"
      "pj-he"
      "RW-he"
      "fs-DX"
      "pj-RW"
      "zg-RW"
      "start-pj"
      "he-WI"
      "zg-he"
      "pj-fs"
      "start-RW" ]

  Assert.AreEqual(226, readGraph input |> recordAllPaths onlyOnceInEachSmallCave |> List.length)
  Assert.AreEqual(3509, readGraph input |> recordAllPaths weirdRules |> List.length)


[<Test>]
let TestParts () =
  let input = inputForDay 12 |> splitLines
  Assert.AreEqual(3369, readGraph input |> recordAllPaths onlyOnceInEachSmallCave |> List.length)
  Assert.AreEqual(85883, readGraph input |> recordAllPaths weirdRules |> List.length)



