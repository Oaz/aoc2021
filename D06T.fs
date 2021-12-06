module aoc2021.D06T

open NUnit.Framework
open aoc2021.D06

[<SetUp>]
let Setup () = ()

[<Test>]
let TestExample () =
  let input = "3,4,3,1,2"

  let initialState =
    Population [ (FishKind.T1, 1L)
                 (FishKind.T2, 1L)
                 (FishKind.T3, 2L)
                 (FishKind.T4, 1L) ]

  Assert.AreEqual(initialState, loadInitialState input)

  Assert.AreEqual(
    Population [ (FishKind.TimerEnd, 1L)
                 (FishKind.T1, 1L)
                 (FishKind.T2, 2L)
                 (FishKind.T3, 1L) ],
    initialState |> nextDay
  )

  Assert.AreEqual(5, initialState |> nextDay |> countFish)

  Assert.AreEqual(
    Population [ (FishKind.TimerEnd, 1L)
                 (FishKind.T1, 2L)
                 (FishKind.T2, 1L)
                 (FishKind.TimerReset, 1L)
                 (FishKind.NewBorn, 1L) ],
    initialState |> nextDay |> nextDay
  )

  Assert.AreEqual(6, initialState |> nextDay |> nextDay |> countFish)
  Assert.AreEqual(26, initialState |> fastForward 18 |> countFish)
  Assert.AreEqual(5934, initialState |> fastForward 80 |> countFish)
  Assert.AreEqual(26984457539L, initialState |> fastForward 256 |> countFish)

[<Test>]
let TestParts () =
  let input = Tools.inputForDay 6

  Assert.AreEqual(
    386755,
    loadInitialState input
    |> fastForward 80
    |> countFish
  )

  Assert.AreEqual(
    1732731810807L,
    loadInitialState input
    |> fastForward 256
    |> countFish
  )
