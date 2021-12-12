module aoc2021.Core.D12

open System
open Tools
open Graph

type Node =
  | Start
  | Big of string
  | Small of string
  | End
let isSmall n =
  match n with
  | Small _ -> true
  | _ -> false

type Graph = Map<Node,Node list>

let readNode (input:string) : Node =
  match input with
  | "start" -> Start
  | "end" -> End
  | s when Char.IsUpper (s.Chars 0) -> Big s
  | s when Char.IsLower (s.Chars 0) -> Small s
  | _ -> failwith $"Unknown node {input}"

let readGraph (input : string list) : Graph =
  let readPair (input:string) : Node*Node =
    let parts = splitOnChar '-' input
    (readNode <| parts.Head, readNode <| parts.Tail.Head)
  let addPair (from:Node) (towards:Node) (dest:Graph) : Graph =
    match from with
    | End -> dest
    | _ ->
      match towards with
      | Start -> dest
      | _ ->
        let targets : Node list = dest.TryFind from |> Option.defaultValue []
        dest.Add (from,List.sort (towards::targets))
  let addPairs (dest:Graph) ((n1,n2):Node*Node) : Graph =
    dest |> addPair n1 n2 |> addPair n2 n1
  List.map readPair input
  |> List.fold addPairs Map.empty

type VisitsCount = Map<Node,int>

let readPath (input:string) : Path<Node> = splitOnComma input |> List.map readNode
  
let recordAllPaths (wr:FindAllPathWalkingRules<VisitsCount,Node>) (g:Graph) : Path<Node> list =
  findAllPaths g wr (fun all _ current -> current::all) [] Start End |> List.rev

let onlyOnceInEachSmallCave : FindAllPathWalkingRules<VisitsCount,Node> = {
  EmptyHistory = Map.empty
  Visit = fun (v:VisitsCount) (n:Node) -> if isSmall n then v.Add (n,1) else v
  Accept = fun (v:VisitsCount) (n:Node) -> not (v.ContainsKey n)
}

let weirdRules : FindAllPathWalkingRules<VisitsCount,Node> = {
  EmptyHistory = Map.empty
  Visit = fun (vc:VisitsCount) (n:Node) ->
    if isSmall n then
      vc.Change (n,fun c -> Some (1+(Option.defaultValue 0 c)))
    else
      vc
  Accept = fun (v:VisitsCount) (n:Node) ->
    if isSmall n then
      let myCount = Map.tryFind n v |> Option.defaultValue 0
      match myCount with
      | 0 -> true
      | 1 ->
        let maxOthers : int = 0::(Map.remove n v |> Map.values |> Seq.cast |> List.ofSeq) |> List.max
        maxOthers < 2
      | _ -> false
    else
      true
}
