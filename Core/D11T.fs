module aoc2021.Core.D11T

open NUnit.Framework
open D11
open Tools

let cavern0 =
  readCavernFrom [ "5483143223"
                   "2745854711"
                   "5264556173"
                   "6141336146"
                   "6357385478"
                   "4167524645"
                   "2176841721"
                   "6882881134"
                   "4846848554"
                   "5283751526" ]

[<Test>]
let TestExample1 () =
  let next =
    cavern0 |> initialFrame |> nextFrame

  Assert.AreEqual(
    readCavernFrom [ "6594254334"
                     "3856965822"
                     "6375667284"
                     "7252447257"
                     "7468496589"
                     "5278635756"
                     "3287952832"
                     "7993992245"
                     "5957959665"
                     "6394862637" ],
    next.Cavern
  )

  Assert.AreEqual(0, next.AllFlashes.Length)

[<Test>]
let TestExample2a () =
  let next =
    cavern0
    |> initialFrame
    |> nextFrame
    |> firstIncreaseEnergyLevel

  Assert.AreEqual(
    readCavernFrom [ "7605365445"
                     "4967076933"
                     "7486778395"
                     "8363558368"
                     "8579507690"
                     "6389746867"
                     "4398063943"
                     "8004003356"
                     "6068060776"
                     "7405973748" ],
    next.Cavern
  )

  Assert.AreEqual(13, next.AllFlashes.Length)

[<Test>]
let TestExample2b () =
  let next =
    cavern0
    |> initialFrame
    |> nextFrame
    |> firstIncreaseEnergyLevel
    |> propagateFlashes

  Assert.AreEqual(
    readCavernFrom [ "7707475445"
                     "4079086933"
                     "7487888395"
                     "8363669379"
                     "8579608600"
                     "6380967878"
                     "5500094943"
                     "0008005456"
                     "8000000876"
                     "8607094848" ],
    next.Cavern
  )

  Assert.AreEqual(23, next.AllFlashes.Length)

[<Test>]
let TestExample2 () =
  Assert.AreEqual(
    readCavernFrom [ "8807476555"
                     "5089087054"
                     "8597889608"
                     "8485769600"
                     "8700908800"
                     "6600088989"
                     "6800005943"
                     "0000007456"
                     "9000000876"
                     "8700006848" ],
    (cavern0
     |> initialFrame
     |> nextFrame
     |> nextFrame)
      .Cavern
  )

[<Test>]
let TestExample3 () =
  Assert.AreEqual(
    readCavernFrom [ "0050900866"
                     "8500800575"
                     "9900000039"
                     "9700000041"
                     "9935080063"
                     "7712300000"
                     "7911250009"
                     "2211130000"
                     "0421125000"
                     "0021119000" ],
    (fastForward 3 cavern0).Cavern
  )

[<Test>]
let TestExample100 () =
  let ff100 = fastForward 100 cavern0

  Assert.AreEqual(
    readCavernFrom [ "0397666866"
                     "0749766918"
                     "0053976933"
                     "0004297822"
                     "0004229892"
                     "0053222877"
                     "0532222966"
                     "9322228966"
                     "7922286866"
                     "6789998766" ],
    ff100.Cavern
  )

  Assert.AreEqual(1656, ff100.AllFlashes.Length)

[<Test>]
let TestExampleMegaflash () =
  Assert.AreEqual(195, firstMegaFlash cavern0)

[<Test>]
let TestParts () =
  let input = inputForDay 11 |> splitLines
  let cavern0 = readCavernFrom input
  Assert.AreEqual(1739, (fastForward 100 cavern0).AllFlashes.Length)
  Assert.AreEqual(324, firstMegaFlash cavern0)
