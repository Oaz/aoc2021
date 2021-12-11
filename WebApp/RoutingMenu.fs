module aoc2021.WebApp.RoutingMenu

open Elmish
open Bolero
open Bolero.Html
open Component

type MenuEntry<'page, 'globalModel, 'globalMessage> =
  'page * string * Attr * (IComponent<'globalModel, 'globalMessage> list)

type Model<'page> = { page: 'page }

type Message<'page, 'globalModel, 'globalMessage> =
  | SetPage of 'page
  | AddPage of MenuEntry<'page, 'globalModel, 'globalMessage>

type MakeComponent<'globalModel, 'globalMessage, 'page when 'page: equality and 'page: comparison>
  (
    mdw: ModelWrapper<Model<'page>, 'globalModel>,
    msw: MessageWrapper<Message<'page, 'globalModel, 'globalMessage>, 'globalMessage>,
    defaultPage: 'page,
    menuDefinition: MenuEntry<'page, 'globalModel, 'globalMessage> list
  ) =
  inherit Component<Model<'page>, Message<'page, 'globalModel, 'globalMessage>, 'globalModel, 'globalMessage>(mdw, msw)
  override this.InitialModel = { page = defaultPage }

  override this.OnUpdate
    (services: Services)
    (message: Message<'page, 'globalModel, 'globalMessage>)
    (model: Model<'page>)
    : Model<'page> * Cmd<Message<'page, 'globalModel, 'globalMessage>> =
    match message with
    | SetPage page ->
      services.Log("SetPage")
      { model with page = page }, Cmd.none
    | AddPage entry ->
      services.Log("AddPage")
      model, Cmd.none

  override this.View
    (model: Model<'page>)
    (dispatch: Message<'page, 'globalModel, 'globalMessage> -> unit)
    (showSubView: SubViewDisplay<'globalModel, 'globalMessage>)
    =
    let menuItem (page: 'page, itemText: string, route: Attr, _) : Node =
      li [] [
        a [ attr.``class`` (
              if model.page = page then
                "is-active"
              else
                ""
            )
            route ] [
          text itemText
        ]
      ]

    let showPage (page: IComponent<'globalModel, 'globalMessage>) = showSubView page.Show

    let menuEntryToPageView ((id, _, _, content): MenuEntry<'page, 'globalModel, 'globalMessage>) : 'page * Node =
      let shows = List.map showPage content
      (id, Concat shows)

    let pageNodes: Map<'page, Node> =
      List.map menuEntryToPageView menuDefinition
      |> Map.ofList

    div [ attr.``class`` "columns" ] [
      aside [ attr.``class`` "column sidebar is-narrow" ] [
        section [ attr.``class`` "section" ] [
          nav [ attr.``class`` "menu" ] [
            ul [ attr.``class`` "menu-list" ] (List.map menuItem menuDefinition)
          ]
        ]
      ]
      div [ attr.``class`` "column" ] [
        cond model.page (fun page -> pageNodes.Item page)
      ]
    ]
