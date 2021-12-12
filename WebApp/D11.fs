module aoc2021.WebApp.D11

open Bolero
open Elmish
open Bolero.Html
open Component
open aoc2021.Core.Tools
open aoc2021.Core.D11

type Model =
  { puzzleInput: string
    step: int
    running: bool
    interval: int
    frame: Frame option }

type Message =
  | SetPuzzleInput of string
  | SetInterval of int
  | Load
  | Start
  | Stop
  | Step
  | Tick
  | Tock

type MakeComponent<'globalModel, 'globalMessage>
  (
    mdw: ModelWrapper<Model, 'globalModel>,
    msw: MessageWrapper<Message, 'globalMessage>
  ) =
  inherit Component<Model, Message, 'globalModel, 'globalMessage>(mdw, msw)

  override this.InitialModel =
    { frame = None
      step = 0
      running = false
      interval = 100
      puzzleInput =
        "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526" }

  override this.UseDispatch = true

  override this.OnUpdate (services: Services) (message: Message) (model: Model) : Model * Cmd<Message> =
    match message with
    | SetPuzzleInput value -> { model with puzzleInput = value }, Cmd.none
    | SetInterval value -> { model with interval = value }, Cmd.none
    | Load ->
      let frame0 =
        model.puzzleInput
        |> splitLines
        |> readCavernFrom
        |> initialFrame

      { model with
          frame = Some frame0
          step = 0 },
      Cmd.none
    | Start -> { model with running = true }, Cmd.ofMsg Tock
    | Stop -> { model with running = false }, Cmd.none
    | Step -> { model with running = false }, Cmd.ofMsg Tick
    | Tick ->
      match model.frame with
      | Some frame ->
        let newFrame = Some <| nextFrame frame
        let model = { model with frame = newFrame; step = model.step + 1 }
        let flashing = flashingIn newFrame.Value.Cavern |> List.length
        if flashing = 100 then
          { model with running = false }, Cmd.none
        else
          model, Cmd.ofMsg Tock
      | None -> model, Cmd.none
    | Tock ->
      if model.running then
        Async.RunSynchronously
        <| async {
             do! Async.Sleep model.interval
             this.Dispatch <| Tick
           }

        model, Cmd.ofMsg Tick
      else
        model, Cmd.none

  override this.View
    (model: Model)
    (dispatch: Message -> unit)
    (showSubView: SubViewDisplay<'globalModel, 'globalMessage>)
    =
    let cavern: Node list =
      match model.frame with
      | Some frame ->
        [ p [] [
            text $"Step = {model.step} / Total Flashes = {frame.AllFlashes.Length}"
          ]
          div [ attr.``class`` "imgArray" ] [
            for y in [ 0 .. 9 ] do
                for x in [ 0 .. 9 ] do
                  let octopus = frame.Cavern.Item { X = x; Y = y }
                  img [ attr.src $"/assets/octopus{octopus.Level}.png" ]
          ]
        ]
      | None -> [ text "LOAD puzzle input then click on START" ]

    concat [ h3 [ attr.``class`` "title" ] [
               text "Dumbo Octopuses"
             ]
             p [] [
               textarea [ attr.id "puzzleInput"
                          bind.input.string model.puzzleInput (fun v -> dispatch (SetPuzzleInput v)) ] []
               button [ on.click (fun _ -> dispatch Load)
                        attr.``class`` "button" ] [
                 text "LOAD"
               ]
               button [ on.click (fun _ -> dispatch Start)
                        attr.``class`` "button" ] [
                 text "START"
               ]
               button [ on.click (fun _ -> dispatch Stop)
                        attr.``class`` "button" ] [
                 text "STOP"
               ]
               button [ on.click (fun _ -> dispatch Step)
                        attr.``class`` "button" ] [
                 text "STEP"
               ]
               input [ attr.``type`` "number"
                       attr.id "interval"
                       attr.``class`` "input"
                       bind.input.int model.interval (fun v -> dispatch (SetInterval v)) ]
             ]
             div [] cavern ]
