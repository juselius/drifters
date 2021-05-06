module Index

open Thoth.Json
open Thoth.Fetch
open Feliz
open Feliz.Bulma
open Feliz.Router
open Leaflet
open Fable.Core.JsInterop
open Fable.Core
open Shared
open Fable.React.Props

module RL = ReactLeaflet

Leaflet.icon?Default?imagePath <- "//cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/images/"

type Model =
    {
        CurrentUrl: string list
        Input: string
        Particles: Particle array array
        Grid: Grid
    }

type Msg =
    | SetInput of string
    | AddGrid of Grid
    | AddParticles of Particle array

let getGrid dispatch =
    let decoder : Decoder<Grid> = Decode.Auto.generateDecoder ()
    promise {
        let! x = Fetch.fetchAs (url="/api/getGrid", decoder = decoder)
        dispatch (AddGrid x)
    } |> Promise.start

let getParticles dispatch =
    let decoder : Decoder<Particle array> = Decode.Auto.generateDecoder ()
    promise {
        let! x = Fetch.fetchAs (url="/api/getFrame/0", decoder = decoder)
        let! y = Fetch.fetchAs (url="/api/getFrame/100", decoder = decoder)
        let! z = Fetch.fetchAs (url="/api/getFrame/1000", decoder = decoder)
        dispatch (AddParticles x)
        dispatch (AddParticles y)
        dispatch (AddParticles z)
    } |> Promise.start

// let addTodo (input: string) dispatch =
//     let decoder : Decoder<Todo> = Thoth.Json.Decode.Auto.generateDecoder ()
//     let todo = Todo.create input
//     promise {
//         let! res = Fetch.post (url="/api/addTodo", data = todo, decoder = decoder)
//         dispatch (AddTodo res)
//     } |> Promise.start

let init () : Model  =
    {
        CurrentUrl = []
        Input = ""
        Particles = Array.empty
        Grid = { Elem = Array.empty; Nodes = Array.empty }
    }

let update (model: Model) (msg: Msg) : Model=
    match msg with
    | SetInput value -> { model with Input = value }
    | AddGrid grid -> { model with Grid = grid }
    | AddParticles p ->
        { model with Particles = Array.append model.Particles [| p |] }

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [ Html.img [
                prop.src "/favicon.png"
                prop.alt "Logo"
            ] ]
        ]
    ]

let tile =
    RL.tileLayer [
        RL.TileLayerProps.Url "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        RL.TileLayerProps.Attribution "&amp;copy <a href=&quot;http://osm.org/copyright&quot;>OpenStreetMap</a> contributors"
    ] []

let particle (pos: float * float) =
    let p = U3.Case3 pos
    RL.circle [
        RL.CircleProps.Custom ("center", p)
        RL.CircleProps.Radius 50.
        RL.CircleProps.FillColor "blue"
        RL.CircleProps.Fill true
        RL.CircleProps.Weight 0.2
    ] []

let triangle (a, b, c) =
    let p =
        U3.Case1 [|
            U3.Case3 a
            U3.Case3 b
            U3.Case3 c
        |]
    RL.polygon [
        RL.PolygonProps.Positions p
        RL.PolygonProps.FillColor "white"
        RL.PolygonProps.Fill false
        RL.PolygonProps.Weight 1.2
    ] []

let renderGrid (grid : Grid) =
    if grid.Elem.Length > 0 then
        grid.Elem
        |> Array. map (fun (a, b, c) ->
            triangle (grid.Nodes.[a], grid.Nodes.[b], grid.Nodes.[c])
        )
        |> List.ofArray
    else []

let renderParticles (particles: Particle array array) t =
    if particles.Length > 0 then
        particles.[t]
        |> Array.map (fun p -> particle p.Pos)
        |> List.ofArray
    else []


let map (grid : Grid) particles =
    Fable.Core.JS.console.log particles
    let pos = U3.Case3 (68.1, 13.4)
    RL.map [
        RL.MapProps.Zoom 9.
        RL.MapProps.Style [
            Height 600
            MinWidth 400
        ]
        RL.MapProps.Center pos
    ] (
        [
            tile
            particle (68.05, 13.6)
        ]
        // @ renderGrid grid
        @ renderParticles particles 0
    )

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.h1 "Stuff"
        ]
        map model.Grid model.Particles
        Bulma.field.div [
            // field.isGrouped
            prop.children [
                // map
                // Bulma.control.p [
                //     control.isExpanded
                //     prop.children [ Bulma.input.text [
                //        prop.value model.Input
                //        prop.placeholder "What needs to be done?"
                //        prop.onChange (fun x -> SetInput x |> dispatch)
                //     ] ]
                // ]
                // Bulma.control.p [ Bulma.button.a [
                //     color.isPrimary
                //     prop.disabled ( Todo.isValid model.Input |> not)
                //     prop.onClick (fun _ -> addTodo model.Input dispatch)
                //     prop.text "Add"
                // ] ]
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
                Bulma.navbar [ Bulma.container [ navBrand ] ]
            ]
            Bulma.heroBody [ Bulma.container [
                Bulma.column [
                    column.is6
                    column.isOffset3
                    prop.children [
                        Bulma.title [
                            text.hasTextCentered
                            prop.text "foo"
                        ]
                        containerBox model dispatch
                    ]
                ] ]
            ]
        ]
    ]

let app =
    Fable.React.FunctionComponent.Of (fun () ->
        let currentUrl, setUrl =
            React.useState (Router.currentUrl())

        let initialModel = init ()
        let model, dispatch = React.useReducer(update, initialModel)

        React.useEffect ((fun _ -> getGrid dispatch), [||])
        React.useEffect ((fun _ -> getParticles dispatch), [||])

        Html.div [
            React.router [
                router.onUrlChanged (setUrl)
            ]
            match currentUrl with
            | _ -> view model dispatch
        ]
    )