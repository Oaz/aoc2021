module aoc2021.WebApp.Component

open Elmish
open Bolero
open System.Net.Http
open Microsoft.JSInterop

type Services =
  { httpClient : HttpClient
    jsRuntime : IJSRuntime }
  member this.Log (txt:string) =
    this.jsRuntime.InvokeVoidAsync("console.log", txt) |> ignore
  member this.SetInterval f i =
    this.jsRuntime.InvokeVoidAsync("window.setInterval", [|f;i|]) |> ignore

type NoModel() =
  static member Init() = NoModel()

type MessageRouter<'globalMessage> = 'globalMessage -> 'globalMessage list

let messageRoutingFusion
  (router: MessageRouter<'globalMessage>)
  (message: 'globalMessage)
  (commands: Cmd<'globalMessage> list)
  : Cmd<'globalMessage> =
  router message
  |> List.map Cmd.ofMsg
  |> List.append commands
  |> Cmd.batch

type Fusion<'globalMessage> = 'globalMessage -> Cmd<'globalMessage> list -> Cmd<'globalMessage>

type ModelWrapper<'local, 'glob>(wrap: 'glob -> 'local -> 'glob, unwrap: 'glob -> 'local) =
  member this.Wrap = wrap
  member this.UnWrap = unwrap

type MessageWrapper<'local, 'glob>
  (
    wrap: 'local -> 'glob,
    unwrap: 'glob -> 'local option,
    messageRouting: MessageRouter<'glob>
  ) =
  member this.Wrap = wrap
  member this.UnWrap = unwrap
  member this.Fusion = messageRoutingFusion messageRouting

type IComponent<'globalModel, 'globalMessage> =
  abstract member Show : 'globalModel -> ('globalMessage -> unit) -> Node
  abstract member Update : Services -> 'globalMessage -> 'globalModel -> 'globalModel * Cmd<'globalMessage>
  abstract member Initialization : Cmd<'globalMessage>
  abstract member ExternalSubscription : ('globalModel -> Cmd<'globalMessage>) option
 
let Initialize (components:IComponent<'globalModel, 'globalMessage> list) : Cmd<'globalMessage> =
  List.map (fun (c:IComponent<'globalModel, 'globalMessage>) -> c.Initialization) components |> Cmd.batch
  
let GetSubscriptionsFor (components:IComponent<'globalModel, 'globalMessage> list) : ('globalModel -> Cmd<'globalMessage>) list =
  List.map (fun (c:IComponent<'globalModel, 'globalMessage>) -> c.ExternalSubscription) components |> List.choose id

type SubView<'globalModel, 'globalMessage> = 'globalModel -> ('globalMessage -> unit) -> Node
type SubViewDisplay<'globalModel, 'globalMessage> = SubView<'globalModel, 'globalMessage> -> Node

[<AbstractClass>]
type Component<'localModel, 'localMessage, 'globalModel, 'globalMessage>
  (
    mdw: ModelWrapper<'localModel, 'globalModel>,
    msw: MessageWrapper<'localMessage, 'globalMessage>
  ) =
  member this.µ : IComponent<'globalModel, 'globalMessage> = (this :> IComponent<'globalModel, 'globalMessage>)
  member this.Show = this.µ.Show
  member this.Update = this.µ.Update
  member this.Dispatch (msg:'localMessage) : unit = this.GlobalDispatch <| msw.Wrap msg
  member val GlobalDispatch = Unchecked.defaultof<'globalMessage -> unit> with get, set
  
  abstract member InitialModel : 'localModel
  abstract member View : 'localModel -> ('localMessage -> unit) -> SubViewDisplay<'globalModel, 'globalMessage> -> Node
  abstract member OnUpdate : Services -> 'localMessage -> 'localModel -> 'localModel * Cmd<'localMessage>

  abstract member InitialMessage : 'localMessage option
  default this.InitialMessage = None

  abstract member UseDispatch : bool
  default this.UseDispatch = false

  interface IComponent<'globalModel, 'globalMessage> with
    member this.Initialization : Cmd<'globalMessage> =
      match this.InitialMessage with
      | Some msg -> msw.Wrap msg |> Cmd.ofMsg
      | None -> Cmd.none
      
    member this.ExternalSubscription : ('globalModel -> Cmd<'globalMessage>) option =
      let sub (dispatch:'globalMessage -> unit) = this.GlobalDispatch <- dispatch
      if this.UseDispatch then Some <| (fun _ -> Cmd.ofSub sub) else None
      
    member this.Show (model: 'globalModel) (dispatch: 'globalMessage -> unit) =
      let showSubView (subView: SubView<'globalModel, 'globalMessage>) : Node = subView model dispatch

      this.View(mdw.UnWrap model) (fun msg -> dispatch (msw.Wrap msg)) showSubView

    member this.Update
      (services: Services)
      (message: 'globalMessage)
      (model: 'globalModel)
      : 'globalModel * Cmd<'globalMessage> =
      let mdl, cmd =
        this.OnUpdate services (msw.UnWrap message).Value (mdw.UnWrap model)

      (mdw.Wrap model mdl),
      [ Cmd.map (fun c -> msw.Wrap c) cmd ]
      |> msw.Fusion message
