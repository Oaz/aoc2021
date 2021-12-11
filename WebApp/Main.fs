module aoc2021.WebApp.Main

open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Component

/// Routing endpoints definition.
type Page =
  | [<EndPoint "/">] D11
 
/// The Elmish application's model.
type Model =
  { menu: RoutingMenu.Model<Page>
    counter: D11.Model }

/// The Elmish application's update messages.
type Message =
  | MenuMessage of RoutingMenu.Message<Page, Model, Message>
  | D11Message of D11.Message

/// Connects the routing system to the Elmish application.
let router: Router<Page, Model, Message> =
  Router.infer (RoutingMenu.SetPage >> MenuMessage) (fun (model: Model) -> model.menu.page)

let messageRouting (message: Message) : Message list = []

let d11 =
  D11.MakeComponent<Model, Message>(
    ModelWrapper((fun gm lm -> { gm with counter = lm }), (fun m -> m.counter)),
    MessageWrapper(
      D11Message,
      (fun msg ->
        match msg with
        | D11Message m -> Some m
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
    D11,
    [ (D11, "Day 11", router.HRef D11, [ d11.µ ]) ]
  )

let initModel =
  { menu = menu.InitialModel
    counter = d11.InitialModel }

let update (message: Message) : Services -> Message -> Model -> Model * Cmd<Message> =
  match message with
  | MenuMessage _ -> menu.Update
  | D11Message _ -> d11.Update

let components =
  [ menu.µ
    d11.µ ]

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

