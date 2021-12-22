module aoc2021.Core.D21

open System
open Tuple2
open Tools

type Player = Player of position:int*score:int
type Round = Round of id:int*dice:int*player1:Player*player2:Player

let movePlayer (spaces:int) (Player (position,score)) : Player =
  let newPosition = ((position+spaces-1)%10)+1
  Player (newPosition,score+newPosition)

let rollDice previous : int*int =
  let next n = n%100+1
  let d1 = next previous
  let d2 = next d1
  let d3 = next d2
  (d1+d2+d3,d3)

let playRound (Round (id,dice,p1,p2)) : Round =
  let spaces,newDice = rollDice dice
  if id%2 = 0 then
    Round (id+1, newDice, movePlayer spaces p1, p2)
  else
    Round (id+1, newDice, p1, movePlayer spaces p2)

let computeScore (Round (id,_,Player (_,score1),Player (_,score2))) : int option =
  if score1 >= 1000 then Some (3*id*score2)
  else if score2 >= 1000 then Some (3*id*score1)
  else None

let deterministicGame (pos1:int) (pos2:int) : int =
  let mutable round = Round (0,100,Player (pos1,0),Player (pos2,0))
  let mutable score = None
  while score.IsNone do
    round <- playRound round
    score <- computeScore round
  score.Value


//====================================

type DiceHistory = Player * int list

let diracWins (pos:int) : DiceHistory list =
  let winning (Player (_,score)) : bool = score >= 21
  let mutable histories : DiceHistory list = [(Player (pos,0),[])]
  let mutable winningHistories : DiceHistory list = []
  while histories.Length > 0 do
    let pastPlayer,pastDices = histories.Head
    histories <- histories.Tail
    for dice in [3..9] do
      let newPlayer = movePlayer dice pastPlayer
      let newHistory = (newPlayer,dice::pastDices)
      if winning newPlayer then
        winningHistories <- newHistory::winningHistories
      else
        histories <- newHistory::histories
  winningHistories

type Ratio = Ratio of wins:bigint * all:bigint

let countUniverses (wins:DiceHistory list) : Map<int,Ratio> =
  let rounds = [|bigint 0;bigint 0;bigint 0;bigint 1;bigint 3;bigint 6;bigint 7;bigint 6;bigint 3;bigint 1|]
  let aggregateUniversesOnSingleWin (_,dices) : int*bigint =
    (List.length dices, List.map (fun dice -> rounds[dice]) dices |> List.reduce (*))
  let aggregateUniversesByRoundLength (len,counts) : int*bigint = (len,List.map snd counts |> List.sum)
  let universesByRoundCount =
    List.map aggregateUniversesOnSingleWin wins
    |> List.groupBy fst
    |> List.map aggregateUniversesByRoundLength
    |> Map.ofList
  let mutable remainingUniverses = 1I
  let mutable rank = 0
  let ratios = seq {
    while remainingUniverses > 0I do
      rank <- rank + 1
      remainingUniverses <- remainingUniverses*27I
      let wins = universesByRoundCount.TryFind rank |> Option.defaultValue 0I
      yield (rank, Ratio (wins,remainingUniverses))
      remainingUniverses <- remainingUniverses - wins
  }
  ratios |> Map.ofSeq

let weavedUniverses (ratios1:Map<int,Ratio>) (ratios2:Map<int,Ratio>) : bigint*bigint =
  let computeWins (Ratio (win,all)) remaining = remaining * win / all
  let mutable remainingUniverses = 27I
  let mutable rank = 0
  let wins = seq {
    while remainingUniverses > 0I do
      rank <- rank + 1
      let win1 = computeWins ratios1[rank] remainingUniverses
      remainingUniverses <- (remainingUniverses - win1)*27I
      let win2 = computeWins ratios2[rank] remainingUniverses
      remainingUniverses <- (remainingUniverses - win2)*27I
      yield (win1,win2)
  }
  Seq.fold (combine (+) (+)) (0I,0I) wins

