module aoc2021.Tools

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
    new Uri("https://adventofcode.com"),
    new Cookie("session", Environment.GetEnvironmentVariable("SESSION"))
  )

  getAsync httpClient $"https://adventofcode.com/2021/day/{day}/input"
  |> Async.RunSynchronously

let splitOnSpaces (input: string) : string list =
  Array.toList (input.Split(" ",StringSplitOptions.RemoveEmptyEntries+StringSplitOptions.TrimEntries))
let splitOnChar (sep:char) (input: string) : string list =
  input.Trim().Split([| sep |]) |> Array.toList
let splitLines = splitOnChar '\n'
let splitOnComma = splitOnChar ','

let generate (f:'a -> 'a) : 'a -> 'a seq = Seq.unfold (fun x -> let y = f x in Some (y,y))
let rollUntil (f:'a*'a -> bool) : 'a seq -> 'a = fst << Seq.head << Seq.skipWhile (f >> not) << Seq.pairwise
let addOption (f: 'a -> 'b option) (t:'a) : ('a*'b) option = Option.bind (fun x -> Some (t,x)) (f t)
let flip f a b = f b a
