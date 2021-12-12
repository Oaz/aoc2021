module aoc2021.Core.D02

open System

type Position =
  { Horizontal: int
    Depth: int
    Aim: int }

type Command =
  | Forward of int
  | Down of int
  | Up of int

let Start: Position = { Horizontal = 0; Depth = 0; Aim = 0 }

let Parse (command: string) : Command =
  let words = command.Split(' ')
  let arg = Int32.Parse(Array.item 1 words)

  match Array.item 0 words with
  | "forward" -> Forward arg
  | "down" -> Down arg
  | "up" -> Up arg
  | _ -> failwith "Not a valid command"

let Interpret1 (pos: Position) (command: Command) : Position =
  match command with
  | Forward h ->
    { pos with
        Horizontal = pos.Horizontal + h }
  | Down d -> { pos with Depth = pos.Depth + d }
  | Up d -> { pos with Depth = pos.Depth - d }

let Interpret2 (start: Position) (command: Command) : Position =
  match command with
  | Forward x ->
    { start with
        Horizontal = start.Horizontal + x
        Depth = start.Depth + start.Aim * x }
  | Down x -> { start with Aim = start.Aim + x }
  | Up x -> { start with Aim = start.Aim - x }

let Travel interpreter (commands: string list) (start: Position) : Position =
  List.map Parse commands
  |> List.fold interpreter start

let Result (position: Position) : int = position.Horizontal * position.Depth
