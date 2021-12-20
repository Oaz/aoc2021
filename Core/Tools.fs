module aoc2021.Core.Tools

open System
open System.Net
open System.Net.Http

let getAsync (client: HttpClient) (url: string) =
  async {
    let! response = client.GetAsync(url) |> Async.AwaitTask
    response.EnsureSuccessStatusCode() |> ignore

    let! content =
      response.Content.ReadAsStringAsync()
      |> Async.AwaitTask

    return content
  }

let inputForDay (day: int) : string =
  use handler = new HttpClientHandler()
  use httpClient = new HttpClient(handler)

  handler.CookieContainer.Add(
    Uri("https://adventofcode.com"),
    Cookie("session", Environment.GetEnvironmentVariable("SESSION"))
  )

  getAsync httpClient $"https://adventofcode.com/2021/day/{day}/input"
  |> Async.RunSynchronously

let splitOnSpaces (input: string) : string list =
  Array.toList (input.Split(" ",StringSplitOptions.RemoveEmptyEntries+StringSplitOptions.TrimEntries))
let splitOnChar (sep:char) (input: string) : string list =
  input.Trim().Split([| sep |]) |> Array.toList
let splitLines = splitOnChar '\n'
let splitOnComma = splitOnChar ','

let splitOnHeader (h:'t -> bool) (xs:'t list) : 't list list =
  let scanner (s:'t list list) (t:'t) : 't list list =
    if h t then [t]::s else (s.Head@[t])::s.Tail
  Seq.fold scanner List.empty xs |> List.rev

let iterate (f:'a -> 'a) : 'a -> 'a seq = Seq.unfold (fun x -> let y = f x in Some (y,y))
let rollUntil (f:'a -> bool) : 'a seq -> 'a = Seq.head << Seq.skipWhile (f >> not)
let rollUntilPairwise (f:'a*'a -> bool) : 'a seq -> 'a = fst << Seq.head << Seq.skipWhile (f >> not) << Seq.pairwise
let addOption (f: 'a -> 'b option) (t:'a) : ('a*'b) option = Option.bind (fun x -> Some (t,x)) (f t)
let flip f a b = f b a
let charToInt (c:char) = int c - int '0'
let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thd3 (_, _, c) = c