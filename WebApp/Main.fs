module aoc2021.WebApp.Main

open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Component

/// Routing endpoints definition.
type Page =
  | [<EndPoint "/">] D20
  | [<EndPoint "/D11">] D11
 
/// The Elmish application's model.
type Model =
  { menu: RoutingMenu.Model<Page>
    d11: D11.Model
    d20: D20.Model }

/// The Elmish application's update messages.
type Message =
  | MenuMessage of RoutingMenu.Message<Page, Model, Message>
  | D11Message of D11.Message
  | D20Message of D20.Message

/// Connects the routing system to the Elmish application.
let router: Router<Page, Model, Message> =
  Router.infer (RoutingMenu.SetPage >> MenuMessage) (fun (model: Model) -> model.menu.page)

let messageRouting (message: Message) : Message list = []

let d11 =
  D11.MakeComponent<Model, Message>(
    ModelWrapper((fun gm lm -> { gm with d11 = lm }), (fun m -> m.d11)),
    MessageWrapper(
      D11Message,
      (fun msg ->
        match msg with
        | D11Message m -> Some m
        | _ -> None),
      messageRouting
    )
  )

let d20 =
  D20.MakeComponent<Model, Message>(
    ModelWrapper((fun gm lm -> { gm with d20 = lm }), (fun m -> m.d20)),
    MessageWrapper(
      D20Message,
      (fun msg ->
        match msg with
        | D20Message m -> Some m
        | _ -> None),
      messageRouting
    )
  )

let menu =
  RoutingMenu.MakeComponent<Model, Message, Page>(
    ModelWrapper((fun gm lm -> { gm with menu = lm }), (fun m -> m.menu)),
    MessageWrapper(
      MenuMessage,
      (fun msg ->
        match msg with
        | MenuMessage m -> Some m
        | _ -> None),
      messageRouting
    ),
    D20,
    [ (D11, "Day 11", router.HRef D11, [ d11.µ ])
      (D20, "Day 20", router.HRef D20, [ d20.µ ]) ]
  )

let initModel =
  { menu = menu.InitialModel
    d11 = d11.InitialModel
    d20 = d20.InitialModel }

let update (message: Message) : Services -> Message -> Model -> Model * Cmd<Message> =
  match message with
  | MenuMessage _ -> menu.Update
  | D11Message _ -> d11.Update
  | D20Message _ -> d20.Update

let components =
  [ menu.µ
    d11.µ
    d20.µ ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()

  [<Inject>]
  member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

  override this.Program =
    let services: Services =
      { httpClient = this.HttpClient
        jsRuntime = this.JSRuntime }

    let program = 
      Program.mkProgram (fun _ -> initModel, Initialize components) (fun msg -> (update msg) services msg) menu.Show
      |> Program.withRouter router
    let program = List.fold (fun p s -> Program.withSubscription s p) program <| GetSubscriptionsFor components
    program

