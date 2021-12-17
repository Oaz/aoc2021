module aoc2021.Core.D14

open System
open Tools
open Counters

type Pair = char*char
type Polymer = {
  Pairs: Map<Pair,Int64>
  Missing: Pair
}
type Rules = Map<Pair,char>

let readInstructions (input:string list) : Polymer*Rules =
  let readRule (input:string) : Pair*char =
    let xs = splitOnSpaces input
    let existing : Pair = xs[0][0],xs[0][1]
    let inserted : char = xs[2][0]
    existing,inserted
  let readPolymer (input:string) : Polymer =
    let pairs : seq<Pair> = Seq.pairwise input
    {
      Pairs = pairs |> Seq.fold incrementCounter Map.empty
      Missing = (Seq.last pairs |> snd, Seq.head pairs |> fst)
    }
  readPolymer input.Head,List.map readRule input.Tail.Tail |> Map.ofList

let step (rules:Rules) (polymer:Polymer) : Polymer =
  let findOperation (ops:Map<Pair,Int64>) ((c1,c2):Pair) (count:Int64) : Map<Pair,Int64> =
    let change = addToAKC 0L
    match rules.TryFind (c1,c2) with
    | Some c12 -> ops |> change count (c1,c12) |> change count (c12,c2) |> change (-count) (c1,c2)
    | None -> ops
  {
    polymer with Pairs = Map.fold findOperation Map.empty polymer.Pairs |> Map.fold (addToCKA 0L) polymer.Pairs
  }

let multiStep (n:int) (rules:Rules) (polymer:Polymer) : Polymer = iterate (step rules) polymer |> Seq.item (n-1)

let polymerLength (polymer:Polymer) : Int64 = 1L+(polymer.Pairs |> Map.values |> Seq.sum)

let quantities (polymer:Polymer) : Map<char,Int64> =
  let adjust (counts:Map<char,Int64>) ((c1,c2):Pair) (count:Int64) : Map<char,Int64> =
    let addTo = addToAKC 0L count
    counts |> addTo c1 |> addTo c2
  let start : Map<char,Int64> = adjust Map.empty polymer.Missing 1
  Map.fold adjust start polymer.Pairs |> Map.map (fun _ v -> v/2L)

let score (quantities:Map<char,Int64>) : Int64 =
  let values = quantities.Values
  (Seq.max values) - (Seq.min values)
