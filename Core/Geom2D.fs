module aoc2021.Core.Geom2D

open Tools
open System

type Coords(x: int, y :int) =
  static member ReadFrom (input:string) : Coords =
    let ns = splitOnComma input |> List.map Int32.Parse in Coords(ns.Head,ns.Tail.Head)
  static member (+) (c1 : Coords, c2: Coords) : Coords = Coords(c1.X + c2.X, c1.Y + c2.Y)
  static member (-) (c1 : Coords, c2: Coords) : Coords = Coords(c1.X - c2.X, c1.Y - c2.Y)
  static member op_Equality (c1 : Coords, c2: Coords) : bool = c1.Equals(c2)
  member this.X = x
  member this.Y = y
  member this.µ : int*int = (x,y)
  member this.SymX(x0: int) : Coords = Coords(2 * x0 - this.X,this.Y)
  member this.SymY(y0: int) : Coords = Coords(this.X,2 * y0 - this.Y)
  member this.Translate (cs : Coords list) : Coords list = List.map (fun c -> c+this) cs
  member this.IsGreaterThan (other:Coords) : bool = this.X >= other.X && this.Y >= other.Y
  member this.IsSmallerThan (other:Coords) : bool = this.X <= other.X && this.Y <= other.Y
  override this.GetHashCode () : int = this.µ.GetHashCode()
  member this.Neighbours (shifts:Coords list): Coords list = List.map (fun s -> this+s) shifts
  member this.AdjacentCoordsHV : Coords list = List.map Coords [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> this.Neighbours
  member this.AdjacentCoordsHVD : Coords list =
    List.allPairs [-1;0;1] [-1;0;1]
    |> List.except [(0,0)]
    |> List.map Coords
    |> this.Neighbours
  override this.Equals(obj:obj) : bool =
    match obj with
    | :? Coords as other -> this.µ=other.µ
    | _ -> failwith $"Cannot compare {this} and {obj}"
  interface IEquatable<Coords> with
    member this.Equals(other) : bool = this.µ=other.µ
  interface IComparable<Coords> with
    member this.CompareTo(other) : int = Operators.compare this.µ other.µ
  interface IComparable with
    member this.CompareTo(obj) : int =
      match obj with
      | :? Coords as other -> Operators.compare this.µ other.µ
      | _ -> failwith $"Cannot compare {this} and {obj}"

type Zone(topLeft : Coords, bottomRight : Coords) =
  member this.InBounds (c:Coords): bool = c.IsGreaterThan topLeft && c.IsSmallerThan bottomRight
  member this.Filter (cs:Coords list) : Coords list = List.filter this.InBounds cs
  member this.Print (cs:Coords list) : unit =
    for y in [topLeft.Y..bottomRight.Y] do
      for x in [topLeft.X..bottomRight.X] do
        match List.tryFind (fun c -> c = Coords(x,y)) cs with
        | Some _ -> printf "█"
        | None -> printf " "
      printfn ""
