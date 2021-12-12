module aoc2021.Core.D01

let allButLast (n: int) (l: 'a list) : 'a list = List.take (List.length l - n) l

let CountIncreases (input: int list) : int =
  let isIncrease (x, y) = x > y

  List.zip input.Tail (allButLast 1 input)
  |> List.where isIncrease
  |> List.length

let SlidingWindow (size: int) (output: 'a list -> 'b) (input: 'a list) : 'b list =
  let getWindow (index: int) : 'b =
    List.skip index input |> List.take size |> output

  [ 0 .. (List.length input - size) ]
  |> List.map getWindow

let SumAcrossThreeMeasurementsSlidingWindow (input: int list) : int list = SlidingWindow 3 List.sum input
