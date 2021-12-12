module aoc2021.Core.D03

open System
open Tools

let ReadBinary x = Convert.ToInt32(x, 2)
let ReadBinaries xs = List.map ReadBinary xs
let GetBit n x = (x >>> n) &&& 1
let ExtractBits (n:int) : int list -> int list = List.map (GetBit n)
let BitMax (xs:int list) : int = List.max xs |> float |> Math.Log2 |> Math.Floor |> int

type Grouping<'key,'item> = 'key * 'item list
type Sort<'a> = (Grouping<'a,'a> -> int) -> Grouping<'a,'a> list -> Grouping<'a,'a> list
let CommonItem (sort:'a Sort) : 'a list -> 'a*int =
  List.groupBy id
  >> sort (fun (_,l) -> List.length l)
  >> List.head
  >> (fun (v,l) -> (v,List.length l))

let RateByCommonItemSelection (sort:int Sort) (xs:int list) : int =
  let selectValue n = ExtractBits n xs |> CommonItem sort |> fst |> (flip (<<<) n)
  [0..BitMax xs] |> List.map selectValue |> List.sum
let GammaRate : int list -> int = RateByCommonItemSelection List.sortByDescending
let EpsilonRate : int list -> int = RateByCommonItemSelection List.sortBy
let PowerConsumption (xs:int list) : int = (GammaRate xs)*(EpsilonRate xs)

let rec HighlanderIteration (xs: 'a list) (filters:('a list -> 'a list) list) : 'a =
    let filtered = filters.Head xs
    if filtered.Length = 1 then filtered.Head else (HighlanderIteration filtered filters.Tail)
  
let RatingByIterativeSelection (sort:int Sort) (valueOnTie: int) (xs:int list) : int =
  let filterOnNthBit (n:int) (ys:int list) : int list =
    let common,nb = ExtractBits n ys |> CommonItem sort
    let chosen = if nb*2 = ys.Length then valueOnTie else common
    List.filter (fun y -> GetBit n y = chosen) ys
  List.map filterOnNthBit [BitMax xs..(-1)..0] |> HighlanderIteration xs
let OxygenGeneratorRating : int list -> int = RatingByIterativeSelection List.sortByDescending 1
let CO2ScrubberRating : int list -> int = RatingByIterativeSelection List.sortBy 0
let LifeSupportRating (xs:int list) : int = (OxygenGeneratorRating xs)*(CO2ScrubberRating xs)
