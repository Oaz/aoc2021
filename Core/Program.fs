module aoc2021.Core.Program
open D13
open Tools

[<EntryPoint>]
let main args =
  let input = inputForDay 13 |> splitLines 
  readInstructions input |> foldAll |> print
  0

