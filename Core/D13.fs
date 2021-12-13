module aoc2021.Core.D13

open System
open Tools
open Geom2D

type Dot = Coords
type Sheet = Dot list
type Fold = Sheet -> Sheet

let foldSheet (partition:Dot -> bool) (transform:Dot -> Dot) (s: Sheet) : Sheet =
  let topLeft, bottomRight = List.partition partition s
  let folded = List.map transform bottomRight
  List.append topLeft folded |> List.distinct

let readInstructions (input:string list) : Sheet*Fold list =
  let sheet =
    List.takeWhile (fun (s:string) -> s.Length > 0) input
    |> List.map (fun s -> Dot.ReadFrom s)
  let readFold (s:string) : Fold =
    let items : string list = splitOnSpaces s |> List.skip 2 |> List.head |> splitOnChar '='
    let n = Int32.Parse items.Tail.Head
    match items.Head with
    | "x" -> foldSheet (fun d -> d.X < n) (fun d -> d.SymX n)
    | "y" -> foldSheet (fun d -> d.Y < n) (fun d -> d.SymY n)
    | _ -> failwith $"Unknown fold {s}"
  let folds =
    List.skipWhile (fun (s:string) -> s.Length > 0) input
    |> List.tail
    |> List.map (fun s -> readFold s)
  sheet,folds

let foldAll ((sheet,folds):Sheet*Fold list) : Sheet =
  List.fold (fun s f -> f s) sheet folds

let findLetter (sheet:Sheet) : char =
  let at (c:int*int) : int = if List.contains (Coords c) sheet then 1 else 0
  let points = [(0,0);(0,1);(1,0)]
  let signature = (sheet.Length,List.mapi (fun i p -> (1 <<< i)*(at p)) points |> List.sum)
  match signature with
  | 9,0 -> 'J'
  | 11,7 -> 'F'
  | 12,3 -> 'K'
  | 12,5 -> 'Z'
  | 14,3 -> 'H'
  | 14,7 -> 'E'
  | _ -> ' '
  
let scan (sheet:Sheet) : string =
  let xMax : int = List.map (fun (d:Dot) -> d.X) sheet |> List.max
  let getLetter (x:int) : char =
    Coords(-x,0).Translate sheet
    |> Zone(Coords(0,0),Coords(4,6)).Filter
    |> findLetter
  [0..5..xMax] |> List.map getLetter |> List.toArray |> String
  
let print (s:Sheet) : unit =
  let xMax : int = List.map (fun (d:Dot) -> d.X) s |> List.max
  Zone(Coords(0,0),Coords(xMax,6)).Print s
