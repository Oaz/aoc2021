module aoc2021.D08

open Tools

type Segment = A | B | C | D | E | F | G
type SegmentSet = Set<Segment>
let all : SegmentSet = Set.ofList [A ; B ; C ; D ; E ; F ; G]
let readSegmentSet (input:string) : SegmentSet =
  let readSegment (c:char) =
    match c with
    | 'a' -> A
    | 'b' -> B
    | 'c' -> C
    | 'd' -> D
    | 'e' -> E
    | 'f' -> F
    | 'g' -> G
    | _ -> failwith $"Unknown wire '{c}'"
  Seq.map readSegment input |> Set.ofSeq

let readSegmentSets (input:string) : SegmentSet list = input |> splitOnSpaces |> List.map readSegmentSet

let segmentsInDisplayWithTurnedOn (size:int) (blocks:SegmentSet list) : SegmentSet =
  List.where (fun (b:SegmentSet) -> b.Count = size) blocks |> Set.unionMany

let segmentsNotInDisplayWithTurnedOn (size:int) (blocks:SegmentSet list) : SegmentSet =
  List.where (fun (b:SegmentSet) -> b.Count = size) blocks
  |> List.map (fun (b:SegmentSet) -> all - b) 
  |> Set.unionMany

type WireMapping = Map<Segment,Segment>
let findWireMapping (mixed:SegmentSet list) : WireMapping =
  let s2 = segmentsInDisplayWithTurnedOn 2 mixed
  let a = segmentsInDisplayWithTurnedOn 3 mixed - s2
  let sn6 = segmentsNotInDisplayWithTurnedOn 6 mixed
  let s4 = segmentsInDisplayWithTurnedOn 4 mixed
  let e = sn6 - s4
  let d = sn6 - e - s2
  let c = sn6 - e - d
  let f = s2 - c
  let b = s4 - s2 - d
  let g = all - s4 - a - e
  let item = Set.minElement
  WireMapping [ (item a, A)
                (item b, B)
                (item c, C)
                (item d, D)
                (item e, E)
                (item f, F)
                (item g, G) ]  

let digit : Map<SegmentSet,int> =
  Map<SegmentSet,int> [
    (Set.ofList [A ; B ; C ; E ; F ; G], 0)
    (Set.ofList [C ; F], 1)
    (Set.ofList [A ; C ; D ; E ; G], 2)
    (Set.ofList [A ; C ; D ; F ; G], 3)
    (Set.ofList [B ; C ; D ; F], 4)
    (Set.ofList [A ; B ; D ; F ; G], 5)
    (Set.ofList [A ; B ; D ; E ; F ; G], 6)
    (Set.ofList [A ; C ; F], 7)
    (Set.ofList [A ; B ; C ; D ; E ; F ; G], 8)
    (Set.ofList [A ; B ; C ; D ; F ; G], 9)
  ]

let digits (wm:WireMapping) (blocks:SegmentSet list) =
  let getDigit (b:SegmentSet) : int = Set.map (flip Map.find wm) b |> flip Map.find digit
  List.map getDigit blocks
  
let decodeEntry (input:string) : int list =
  let entry = splitOnChar '|' input
  let wireMapping : WireMapping = entry.Item 0 |> readSegmentSets |> findWireMapping
  entry.Item 1 |> readSegmentSets |> digits wireMapping

let isEasy (d:int) = d = 1 || d = 4 || d = 7 || d = 8
let asNumber (digits:int list) : int =
  digits |> List.rev |> List.mapi (fun (power:int) (digit:int) -> (digit*(pown 10 power))) |> List.sum
