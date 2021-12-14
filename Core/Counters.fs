module aoc2021.Core.Counters

open System

let inline addToCKA (c0:'COUNTER) (counters:Map<'ITEM,'COUNTER>) (k:'ITEM) (add:'COUNTER) : Map<'ITEM,'COUNTER> =
  let currentValue = Map.tryFind k counters |> Option.defaultValue c0
  counters.Add (k,currentValue+add)

let inline addToCAK (c0:'COUNTER) (counters:Map<'ITEM,'COUNTER>) (add:'COUNTER) (k:'ITEM) : Map<'ITEM,'COUNTER> =
  addToCKA c0 counters k add

let inline addToKAC (c0:'COUNTER) (k:'ITEM) (add:'COUNTER) (counters:Map<'ITEM,'COUNTER>) : Map<'ITEM,'COUNTER> =
  addToCKA c0 counters k add

let inline addToAKC (c0:'COUNTER) (add:'COUNTER) (k:'ITEM) (counters:Map<'ITEM,'COUNTER>) : Map<'ITEM,'COUNTER> =
  addToCKA c0 counters k add

let inline addToACK (c0:'COUNTER) (add:'COUNTER) (counters:Map<'ITEM,'COUNTER>) (k:'ITEM) : Map<'ITEM,'COUNTER> =
  addToCKA c0 counters k add
  
let inline incrementCounter (counters:Map<'ITEM,Int64>) (k:'ITEM) : Map<'ITEM,Int64> =
  addToCKA 0L counters k 1L
