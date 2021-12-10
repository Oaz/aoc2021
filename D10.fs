module aoc2021.D10

open System
open aoc2021.Stack

type Bracket =
  | Round
  | Angle
  | Square
  | Curly

type Token =
  | Open of Bracket
  | Close of Bracket

let getToken (c: char) =
  match c with
  | '(' -> Open Round
  | ')' -> Close Round
  | '<' -> Open Angle
  | '>' -> Close Angle
  | '[' -> Open Square
  | ']' -> Close Square
  | '{' -> Open Curly
  | '}' -> Close Curly
  | _ -> failwith $"Unknown token {c}"

type Result =
  | Legal
  | Incomplete of Bracket list
  | Corrupted of Bracket * Bracket
  | Missing

type Chunking = Result option * Stack<Bracket>

let read ((result, brackets): Chunking) (token: Token) : Chunking =
  match token with
  | Open b -> (result, brackets.Push b)
  | Close b ->
    if brackets = Empty then
      (Some Missing, Empty)
    else if b = brackets.Top then
      (result, brackets.Pop)
    else
      (Some <| Corrupted(brackets.Top, b), brackets.Pop)

let chunk (input: string) : Result =
  let result, remaining: Chunking =
    Seq.map getToken input
    |> Seq.fold read (None, Empty)

  if result.IsSome then result.Value
  else if remaining = Empty then Legal
  else Incomplete remaining.ToList

let corruptedPoints (r: Result) : int =
  match r with
  | Corrupted (_, Round) -> 3
  | Corrupted (_, Square) -> 57
  | Corrupted (_, Curly) -> 1197
  | Corrupted (_, Angle) -> 25137
  | _ -> 0

let illegalCharacterScore : string list -> int = List.map (chunk >> corruptedPoints) >> List.sum

let incompletePoints (r: Result) : Int64 option =
  let bracketPoints (b: Bracket) =
    match b with
    | Round -> 1L
    | Square -> 2L
    | Curly -> 3L
    | Angle -> 4L

  let compute (score: Int64) (b: Bracket) = (score * 5L) + (bracketPoints b)

  match r with
  | Incomplete bs -> Some <| List.fold compute 0L bs
  | _ -> None

let middle (xs: 'a list) : 'a =
  List.sort xs |> List.item ((List.length xs) / 2)

let autocompleteScore : string list -> Int64 = List.map (chunk >> incompletePoints) >> List.choose id >> middle
