module aoc2021.Core.Stack

type Stack<'t> =
  | Empty
  | Content of 't * Stack<'t>
  member this.Push (x:'t) = Content (x, this)
    
  member this.Top =
    match this with
    | Empty -> failwith "Empty! No top item!"
    | Content (t, _) -> t

  member this.Top2 =
    match this with
    | Content (t1, Content (t2, _)) -> (t1,t2)
    | _ -> failwith "No top 2 item!"

  member this.Top3 =
    match this with
    | Content (t1, Content (t2, Content (t3, _))) -> (t1,t2,t3)
    | _ -> failwith "No top 3 item!"

  member this.Pop =
    match this with
    | Empty -> failwith "Empty! Cannot Pop!"
    | Content (_, st) -> st

  member this.PopN (n:int) =
    if n = 0 then this else
      match this with
      | Empty -> failwith "Empty! Cannot Pop!"
      | Content (_, st) -> st.PopN (n - 1)
    
  member this.Fold (f:'s -> 't -> 's) (s0:'s) : 's =
    let rec descend s = function
      | Empty -> s
      | Content (t,st) -> descend (f s t) st
    descend s0 this
  
  member this.ToList : 't list = this.Fold (fun l b -> b :: l) [] |> List.rev

  member this.TopN (n:int) : 't list = this.ToList |> List.take n

let top (s:Stack<'t>) = s.Top
let top2 (s:Stack<'t>) = s.Top2
let push (x:'t) (s:Stack<'t>) = s.Push x
let popn (n:int) (s:Stack<'t>) = s.PopN n

