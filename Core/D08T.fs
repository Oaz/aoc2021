module aoc2021.Core.D08T

open NUnit.Framework
open D08
open Tools

[<SetUp>]
let Setup () = ()

[<Test>]
let TestReadSegments () =
  Assert.AreEqual(Set.ofList [ A; B; D; G ], readSegmentSet "gadb")
  Assert.AreEqual(Set.ofList [ D; E; G ], readSegmentSet "egd")

let mixedSegments =
  readSegmentSets "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"

[<Test>]
let TestInDisplayWithTurnedOn () =
  Assert.AreEqual(Set.ofList [ A; B ], segmentsInDisplayWithTurnedOn 2 mixedSegments)
  Assert.AreEqual(Set.ofList [ A; B; D ], segmentsInDisplayWithTurnedOn 3 mixedSegments)
  Assert.AreEqual(Set.ofList [ E; A; F; B ], segmentsInDisplayWithTurnedOn 4 mixedSegments)

[<Test>]
let TestNotInDisplayWithTurnedOn () =
  Assert.AreEqual(Set.ofList [ A; F; G ], segmentsNotInDisplayWithTurnedOn 6 mixedSegments)

[<Test>]
let TestFindWireMapping () =
  Assert.AreEqual(
    WireMapping [ (D, A)
                  (G, E)
                  (F, D)
                  (A, C)
                  (B, F)
                  (E, B)
                  (C, G) ],
    findWireMapping mixedSegments
  )

[<Test>]
let TestDigits () =
  let digitBlocks = readSegmentSets "cdfeb fcadb cdfeb cdbaf"
  let wireMapping = findWireMapping mixedSegments
  Assert.AreEqual(
    [ 5; 3; 5; 3 ],
    digits wireMapping digitBlocks
  )

[<Test>]
let TestDecodeEntry () =
  let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  Assert.AreEqual(
    [ 5; 3; 5; 3 ],
    decodeEntry input
  )

[<Test>]
let TestExample () =
  let input = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]
  Assert.AreEqual(26, List.collect decodeEntry input |> List.where isEasy |> List.length)
  Assert.AreEqual(61229, List.map (decodeEntry >> asNumber) input |> List.sum)

[<Test>]
let TestParts () =
  let entries = inputForDay 8 |> splitLines
  Assert.AreEqual(
    488,
    List.collect decodeEntry entries |> List.where isEasy |> List.length
  )
  Assert.AreEqual(
    1040429,
    List.map (decodeEntry >> asNumber) entries |> List.sum
  )
