module aoc2021.Core.D04

open System
open Tools

type Cell =
  { Number : int
    Drawn : bool }
  static member ReadFrom (s:string) : Cell = { Number = Int32.Parse s; Drawn = false }
  member this.Mark () : Cell = { Number = this.Number; Drawn = true }

type Grid =
  { Cells : Cell list
    Bingo : bool }
  member this.Mark (drawn:int) : Grid =
    let newCells = List.map (fun c -> if c.Number = drawn then c.Mark() else c ) this.Cells
    let all5 (cells:Cell list) : bool = List.forall (fun c -> c.Drawn) cells
    let hasWinner (getCells:int -> Cell list) = [0..4] |> List.map (getCells >> all5) |> List.reduce (||)
    let row (n:int) : Cell list = List.skip (n*5) newCells |> List.take 5
    let column (n:int) : Cell list = List.map (flip List.item newCells) [n..5..n+20]
    {
      Cells = newCells
      Bingo = this.Bingo || hasWinner row || hasWinner column
    }
  member this.RemainingNumbers () : int list =
    List.where (fun c -> not c.Drawn) this.Cells |> List.map (fun c -> c.Number)

type Game =
  { NextDrawn: int list
    Grids: Grid list }
  static member ReadFrom (input:string list) : Game =
    let readGridLine (line:string) = splitOnSpaces line |> List.map Cell.ReadFrom 
    let readGrid (index:int) : Cell list =
      List.skip index input
      |> List.take 5
      |> List.map readGridLine
      |> List.reduce (@)
    {
      NextDrawn = splitOnComma input.Head |> List.map Int32.Parse
      Grids = [2..6..input.Length - 4] |> List.map (fun n -> { Cells = readGrid n; Bingo = false })
    }
  member this.Play () : Game seq =
    let drawNextNumber (gm:Game) = {
      NextDrawn = gm.NextDrawn.Tail;
      Grids = gm.Grids |> List.map (fun g -> g.Mark(gm.NextDrawn.Head))
    }
    iterate drawNextNumber this
  member this.ToFirstWinningGrid () : Game*int =
    let winningGrid (gm:Game) : int option = List.tryFindIndex (fun g -> g.Bingo) gm.Grids
    this.Play() |> Seq.map (addOption winningGrid) |> Seq.choose id |> Seq.head
  member this.ToLastWinningGrid () : Game*int =
    let winningGrids (gm:Game) : int list = List.indexed gm.Grids |> List.where (fun (_,g) -> g.Bingo) |> List.map fst
    let (_,before),(lastGame,after) =
      this.Play()
      |> Seq.map (fun g -> (g,winningGrids g))
      |> Seq.pairwise
      |> Seq.where (fun (_,(_,ws)) -> ws.Length = this.Grids.Length)
      |> Seq.head
    lastGame,(List.except before after |> List.head)

let Score (game:Game) ((playedGame,winningGridIndex):Game*int) : int =
  let lastDrawn = List.item (game.NextDrawn.Length-playedGame.NextDrawn.Length-1) game.NextDrawn
  let winningGrid = List.item winningGridIndex playedGame.Grids
  let remainingSum = winningGrid.RemainingNumbers() |> List.sum
  remainingSum * lastDrawn
  