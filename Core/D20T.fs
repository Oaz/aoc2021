module aoc2021.Core.D20T

open NUnit.Framework
open D20
open Tools

[<Test>]
let TestReadImage () =
  Assert.AreEqual( { center=array2D [|
    [|1;0;0;1;0|]
    [|1;0;0;0;0|]
    [|1;1;0;0;1|]
    [|0;0;1;0;0|]
    [|0;0;1;1;1|]
  |]; others=0 }, readImage [
    "#..#."
    "#...."
    "##..#"
    "..#.."
    "..###"
  ])

[<Test>]
let TestReadAlgorithm () =
  Assert.AreEqual( [| 0; 0; 1 |], readAlgorithm "..#" )

let input = [
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
  ""
  "#..#."
  "#...."
  "##..#"
  "..#.."
  "..###"
]

[<Test>]
let TestExample () =
  let algorithm,image0 = readInstructions input
  Assert.AreEqual( 512, algorithm.Length )
  Assert.AreEqual( 5, Array2D.length1 image0.center )
  Assert.AreEqual( 5, Array2D.length2 image0.center )
  
  let image1 = enhance algorithm image0
  Assert.AreEqual( readImage [
    ".##.##."
    "#..#.#."
    "##.#..#"
    "####..#"
    ".#..##."
    "..##..#"
    "...#.#."
  ], image1
  )
  
  let image2 = enhance algorithm image1
  Assert.AreEqual( readImage [
    ".......#."
    ".#..#.#.."
    "#.#...###"
    "#...##.#."
    "#.....#.#"
    ".#.#####."
    "..#.#####"
    "...##.##."
    "....###.."
  ], image2
  )
  Assert.AreEqual( image2, mEnhance 2 algorithm image0 )
  Assert.AreEqual( 35, litCount image2 )
  Assert.AreEqual( 3351, mEnhance 50 algorithm image0 |> litCount )
 
[<Test>]
let TestParts () =
  let algorithm,image0 = inputForDay 20 |> splitLines |> readInstructions
  Assert.AreEqual( 5475, mEnhance 2 algorithm image0 |> litCount )
  Assert.AreEqual( 17548, mEnhance 50 algorithm image0 |> litCount )
  Assert.AreEqual( 35131, mEnhance 100 algorithm image0 |> litCount )


