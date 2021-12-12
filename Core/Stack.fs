module aoc2021.Core.Stack

type Stack<'t> =
  | Empty
  | Content of 't * Stack<'t>
  member this.Push (x:'t) = Content (x, this)

  member this.Top =
    match this with
    | Empty -> failwith "Empty! No top item!"
    | Content (t, _) -> t

  member this.Pop =
    match this with
    | Empty -> failwith "Empty! Cannot Pop!"
    | Content (_, st) -> st
    
  member this.Fold (f:'s -> 't -> 's) (s0:'s) : 's =
    let rec descend s = function
      | Empty -> s
      | Content (t,st) -> descend (f s t) st
    descend s0 this
  
  member this.ToList : 't list = this.Fold (fun l b -> b :: l) [] |> List.rev
