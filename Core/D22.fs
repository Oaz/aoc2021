module aoc2021.Core.D22

open System
open Tools
open Tuple2
open FSharp.Stats

type Cuboid =
  { Min : Vector<float>
    Max : Vector<float> }
  member this.Cubes : bigint =
    this.Max - this.Min + Vector.create 3 1 |> Vector.toArray |> Array.map bigint |> Array.reduce (*)
  member this.Intersect (other:Cuboid) : Cuboid option =
    let v f v1 v2 =
      Array.zip (Vector.toArray v1) (Vector.toArray v2)
      |> Array.map (uncurry f)
      |> Vector.ofArray
    let vmin = v max this.Min other.Min
    let vmax = v min this.Max other.Max
    if Vector.forall ((<=) 0) (vmax - vmin) then Some { Min = vmin; Max = vmax } else None 
  member this.Remove (other:Cuboid) : Cuboid list =
    match this.Intersect other with
    | None -> [this]
    | Some inter -> [
      { Min=this.Min; Max=Vector.ofList [this.Max[0];this.Max[1];inter.Min[2]-1.0] }
      { Min=Vector.ofList [this.Min[0];this.Min[1];inter.Max[2]+1.0]; Max=this.Max }
      { Min=Vector.ofList [this.Min[0];this.Min[1];inter.Min[2]]; Max=Vector.ofList [this.Max[0];inter.Min[1]-1.0;inter.Max[2]] }
      { Min=Vector.ofList [this.Min[0];inter.Max[1]+1.0;inter.Min[2]]; Max=Vector.ofList [this.Max[0];this.Max[1];inter.Max[2]] }
      { Min=Vector.ofList [this.Min[0];inter.Min[1];inter.Min[2]]; Max=Vector.ofList [inter.Min[0]-1.0;inter.Max[1];inter.Max[2]] }
      { Min=Vector.ofList [inter.Max[0]+1.0;inter.Min[1];inter.Min[2]]; Max=Vector.ofList [this.Max[0];inter.Max[1];inter.Max[2]] }
    ] |> List.filter (fun (c:Cuboid) -> c.Cubes > 0I)
  member this.RemoveFrom (others:Cuboid list) : Cuboid list =
    match others with
    | [] -> []
    | o::os ->
      let ros = this.RemoveFrom os
      match this.Intersect o with
      | None -> o::ros
      | Some intersection -> (o.Remove intersection)@ros
  member this.AddTo (others:Cuboid list) : Cuboid list = this::(this.RemoveFrom others)

let readInterval(input:string) : int*int =
  let xs = splitOnChar '.' (input.Substring(2))
  (Int32.Parse xs[0],Int32.Parse xs[2])

let readCuboid (input:string) : Cuboid =
  let intervals: (int * int) list = splitOnComma input |> List.map readInterval
  { Min = List.map (fst >> float) intervals |> Vector.ofList
    Max = List.map (snd >> float) intervals |> Vector.ofList }

type Op = On of Cuboid | Off of Cuboid
let readOp (input:string) : Op = let xs = splitOnSpaces input in readCuboid xs[1] |> if xs[0] = "on" then On else Off

let apply (cs:Cuboid list) (o:Op) : Cuboid list =
  match o with
  | On c -> c.AddTo cs
  | Off c -> c.RemoveFrom cs

let sizeOf (cs:Cuboid list) : bigint = List.map (fun (c:Cuboid) -> c.Cubes) cs |> List.sum
