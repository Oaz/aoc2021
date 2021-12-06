module aoc2021.D06

open System
open Tools

type FishKind = TimerEnd=0 | T1=1 | T2=2 | T3=3 | T4=4 | T5=5 | TimerReset=6 | T7=7 | NewBorn=8
let nextFishDay (fishKind:FishKind) : FishKind list =
  if fishKind = FishKind.TimerEnd then
    [ FishKind.TimerReset; FishKind.NewBorn ]
  else
    [ enum<FishKind> <| int fishKind-1 ]

type Population = Map<FishKind,int64>

let addToFishKind (fishToAdd:int64) (population:Population) (fishKind:FishKind) : Population =
  let currentCount = population.TryFind(fishKind) |> Option.defaultValue 0L
  population.Add(fishKind, currentCount+fishToAdd)

let addToMultipleKindOfFish (fishToAdd:int64) (population:Population) (fishKinds:FishKind list) : Population =
  List.fold (addToFishKind fishToAdd) population fishKinds

let loadInitialState (input: string) : Population =
  splitOnComma input |> List.map (Int32.Parse >> enum<FishKind>) |> List.fold (addToFishKind 1L) (Population [])

let nextDay (population:Population) : Population =
  Map.fold (fun pop kind count -> (addToMultipleKindOfFish count) pop (nextFishDay kind) ) (Population []) population

let fastForward (days:int) (population:Population) : Population =
  generate nextDay population |> Seq.skip (days-1) |> Seq.head

let countFish (population:Population) : int64 = Seq.sum population.Values
