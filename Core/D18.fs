module aoc2021.Core.D18

open System
open Tools

type Number =
  | Leaf of int
  | Pair of Number*Number

let readNumber (input:string) : Number =
  let f s c =
    match c with
    | '[' | ',' -> s
    | ']' -> Stack.popn 2 s |> Stack.push (Stack.top2 s |> Tuple2.swap |> Pair)
    | _ -> Stack.push (charToInt c |> Leaf) s
  Seq.fold f Stack.Empty input |> Stack.top

type Explosion =
  { number:Number
    level:int
    addToLeft:int option
    addToRight:int option
    exploded:bool }

let rec applyAdditions (c:Explosion) : Explosion =
  let addValue (va:int option) (v1:int) =
    match va with
    | Some v2 -> v1+v2
    | None -> v1
  match c with
  | { number=Leaf v } ->
    let n = v |> addValue c.addToLeft |> addValue c.addToRight
    { c with number = Leaf n; addToLeft=None; addToRight=None }
  | { number=Pair (n1,n2) } ->
    let a1 = applyAdditions { c with number=n1; addToRight=None }
    let a2 = applyAdditions { c with number=n2; addToLeft=None }
    { c with number = Pair (a1.number,a2.number); addToLeft=None; addToRight=None }

let rec exploder (e:Explosion) : Explosion =

  let visitLeft ((nl,nr):Number*Number) =
    let visited = exploder { e with number=nl; level=e.level+1 }
    if visited.exploded then
      let applied = applyAdditions { e with number=nr; addToLeft=visited.addToRight; addToRight=None; exploded=visited.exploded }
      Some { applied with number=Pair(visited.number,applied.number); addToLeft=visited.addToLeft; }
    else
      None

  let visitRight ((nl,nr):Number*Number) =
    let visited = exploder { e with number=nr; level=e.level+1 }
    if visited.exploded then
      let applied = applyAdditions { e with number=nl; addToRight=visited.addToLeft; addToLeft=None; exploded=visited.exploded }
      Some { applied with number=Pair(applied.number,visited.number); addToRight=visited.addToRight; }
    else
      None
  
  match e with
  | { number=Pair(Leaf vl,Leaf vr); level=4 } -> {
      e with number=Leaf 0; addToLeft=Some vl; addToRight=Some vr; exploded=true
    }
  | { number=Pair(nl,nr) } -> List.choose id [visitLeft (nl,nr); visitRight (nl,nr); Some e] |> List.head
  | _ -> e

let explode (n:Number) : Number*bool =
  let result = exploder { number=n; level=0; addToLeft=None; addToRight=None; exploded=false }
  result.number,result.exploded

let rec splitter ((n,complete):Number*bool) : Number*bool =
  if complete then (n,complete) else
  match n with
  | Leaf v  -> if v >= 10 then (Pair (Leaf (v/2), Leaf (v/2 + v%2)),true) else (n,complete)
  | Pair (nL,nR) ->
    let nLs,complete = splitter (nL,complete)
    if complete then (Pair(nLs,nR),complete) else
      let nRs,complete = splitter (nR,complete)
      (Pair(nLs,nRs),complete)

let split (n:Number) : Number*bool = splitter (n,false)

let orElse (f: Number -> Number*bool) ((n,b):Number*bool) : Number*bool = if b then (n,b) else f n
let action ((n,_):Number*bool) : Number*bool = (n,false) |> orElse explode |> orElse split
let add (n1:Number) (n2:Number) : Number = iterate action (Pair (n1,n2),false) |> rollUntil (snd >> not) |> fst
let sum (ns:Number list) : Number = List.reduce add ns

let rec magnitude (n:Number) : int =
  match n with
  | Leaf v -> v
  | Pair (n1,n2) -> 3*(magnitude n1) + 2*(magnitude n2)

let largestMagnitude (ns:Number list) : int =
  [
    for n1 in ns do
      for n2 in ns do
        yield (add n1 n2 |> magnitude)
  ] |> List.max

