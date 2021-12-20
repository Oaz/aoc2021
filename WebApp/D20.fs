module aoc2021.WebApp.D20

open System
open Bolero
open Elmish
open Bolero.Html
open Component
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Formats.Png
open SixLabors.ImageSharp.PixelFormats
open aoc2021.Core.Tools

type Model =
  { puzzleInput: string
    enhancementLevel: int
    bitmap: string option }

type Message =
  | SetPuzzleInput of string
  | SetEnhancement of int
  | Compute

let computeBitmap (svc: Services) (image:aoc2021.Core.D20.Image) =
  svc.Log($"build bitmap {DateTime.Now}")
  let factor = 3
  let center = image.center
  let width = Array2D.length2 center
  let height = Array2D.length1 center
  let black = Rgba32.op_Implicit(Color.Black)
  let white = Rgba32.op_Implicit(Color.White)
  use buffer = new Image<Rgba32>(width*factor, height*factor, black)
  for y in [0..(height*factor-1)] do
    let row = buffer.GetPixelRowSpan(y)
    for x in [0..(width*factor-1)] do
      if center[y/factor,x/factor] = 1 then
        row[x] <- white
  svc.Log($"convert to base 64 {DateTime.Now}")
  let b64 = buffer.ToBase64String(PngFormat.Instance)
  svc.Log($"done {DateTime.Now}")
  b64

type MakeComponent<'globalModel, 'globalMessage>
  (
    mdw: ModelWrapper<Model, 'globalModel>,
    msw: MessageWrapper<Message, 'globalMessage>
  ) =
  inherit Component<Model, Message, 'globalModel, 'globalMessage>(mdw, msw)

  override this.InitialModel =
    { 
      bitmap = None
      enhancementLevel = 10
      puzzleInput =
          "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
  
  #..#.
  #....
  ##..#
  ..#..
  ..###"
    }

  override this.UseDispatch = true

  override this.OnUpdate (services: Services) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetPuzzleInput value -> { model with puzzleInput = value }, Cmd.none
    | SetEnhancement value -> { model with enhancementLevel = value }, Cmd.none
    | Compute ->
      services.Log($"enhance {DateTime.Now}")
      let algorithm,image =
        model.puzzleInput
        |> splitLines
        |> aoc2021.Core.D20.readInstructions
      
      let newImage = aoc2021.Core.D20.mEnhance model.enhancementLevel algorithm image
      let bitmap = computeBitmap services newImage
      { model with bitmap = Some bitmap }, Cmd.none

  override this.View
    (model: Model)
    (dispatch: Message -> unit)
    (showSubView: SubViewDisplay<'globalModel, 'globalMessage>)
    =
    let ocean: Node list =
      match model.bitmap with
      | Some bitmap ->
        [ p [] [
            text $"Enhancement = {model.enhancementLevel}"
          ]
          img [ attr.src bitmap ]
        ]
      | _ -> [ text "Select enhancement level and then click on 'Compute'" ]

    concat [ h3 [ attr.``class`` "title" ] [
               text "Trench Map"
             ]
             p [] [
               textarea [ attr.id "puzzleInput"
                          bind.input.string model.puzzleInput (fun v -> dispatch (SetPuzzleInput v)) ] []
               input [ attr.``type`` "number"
                       attr.id "interval"
                       attr.``class`` "input"
                       bind.input.int model.enhancementLevel (fun v -> dispatch (SetEnhancement v)) ]
               button [ on.click (fun _ -> dispatch Compute)
                        attr.``class`` "button" ] [
                 text "Compute"
               ]      
             ]
             div [] ocean ]
