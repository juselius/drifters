module Index

open Elmish
open Feliz
open Feliz.Bulma
open Thoth.Fetch
open Thoth.Json
open Shared

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo


let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }
    let decoder: Decoder<Todo list> = Decode.Auto.generateDecoder ()

    let get () =
        Fetch.fetchAs (url = "/api/getTodos", decoder = decoder)

    let cmd = Cmd.OfPromise.perform get () GotTodos

    model, cmd

let addTodo model =
    let todo = Todo.create model.Input
    let decoder: Decoder<Todo> = Decode.Auto.generateDecoder ()

    let post (data: Todo) =
        Fetch.post (url = "/api/addTodo", data = data, decoder = decoder)

    let cmd = Cmd.OfPromise.perform post todo AddedTodo
    { model with Input = "" }, cmd

let addedTodo model todo =
    { model with Todos = model.Todos @ [ todo ] }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo -> addTodo model
    | AddedTodo todo -> addedTodo model todo

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Drifters"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]