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

let splitLines (input: string) : string list =
  input.Trim().Split([| '\n' |]) |> Array.toList
