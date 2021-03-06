module Server

open System
open System.IO
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Saturn
open Giraffe
open Serilog

open Shared
open Settings

printfn "Create grid"
let grid = Grid.readGrid appsettings.grid
let gridWGS = { grid with Nodes = grid.Nodes |> Array.map UTM.toLatLon }

let p = UTM.fromLatLon (68.02, 13.50)
let particles = Particle.initParticles grid 100 p

// printfn "Run simulation"
let sim: Advect.Simulation =
    {
        t = 0.0
        dt = appsettings.dt
        stepT = 3600.0
        endT = 200.0 * 3600.0
        grid = grid
        particles = particles
        dispatch = Advect.track
    }

Directory.CreateDirectory "output" |> ignore
// let restart = Advect.runSimulation sim

let getNumFrames () =
    let r = Advect.track.PostAndReply(fun r -> r, Advect.Msg.GetNumFrames)

    match r with
    | Advect.Reply.NumFrames n -> n
    | _ -> failwith "Unexpexted reply"

// printfn "Number of available frames: %d" numFrames

let askFrame n =
    let r = Advect.track.PostAndReply(fun r -> r, Advect.Msg.GetFrame n)

    match r with
    | Advect.Reply.Frame x -> x
    | _ -> failwith "Unexpexted reply"

let private getGrid () = json gridWGS
let private getNFrames () = json (getNumFrames ())
let private getFrame n = json (askFrame n)

let private getFrames () =
    let nf = getNumFrames ()

    [ nf - 1 .. -1 .. 0 ]
    |> List.fold (fun a n -> askFrame n :: a) []
    |> json

let private setSim (next: HttpFunc) (ctx: HttpContext) =
    task {
        // let! sim = ctx.BindJsonAsync<Advect.Simulation> ()
        Advect.track.PostAndReply(fun r -> r, Advect.SetSim(sim, None))
        |> ignore

        return! Successful.OK () next ctx
    }

let private startSim (next: HttpFunc) (ctx: HttpContext) =
    task {
        // let! sim = ctx.BindJsonAsync<Advect.Simulation> ()
        Advect.track.PostAndReply(fun r -> r, Advect.Start sim)
        |> ignore

        return! Successful.OK () next ctx
    }

let private injectParticles (next: HttpFunc) (ctx: HttpContext) =
    task {
        let! latln = ctx.BindJsonAsync<float * float>()
        let pos = UTM.fromLatLon latln
        let p = Particle.initParticles grid 100 pos

        Advect.track.PostAndReply(fun r -> r, Advect.InjectParticles p)
        |> ignore

        return! Successful.OK () next ctx
    }

let private isRunning () =
    match Advect.track.PostAndReply(fun r -> r, Advect.QueryRunning) with
    | Advect.IsRunning running -> json running
    | _ -> failwith "Unexpexted reply"

let private getSim () =
    match Advect.track.PostAndReply(fun r -> r, Advect.GetSim) with
    | Advect.Simulation (sim, _) -> json sim
    | _ -> failwith "Unexpexted reply"

let private pauseSim () =
    Advect.track.PostAndReply(fun r -> r, Advect.Pause)
    |> ignore

    Successful.OK()

let private continueSim () =
    Advect.track.PostAndReply(fun r -> r, Advect.Resume)
    |> ignore

    Successful.OK()

let private resetSim () =
    Advect.track.PostAndReply(fun r -> r, Advect.Reset)
    |> ignore

    Successful.OK()

let private stepFrame () =
    match Advect.track.PostAndReply(fun r -> r, Advect.Step) with
    | Advect.Frame p -> json (p |> Array.map UTM.toLatLon)
    | _ -> failwith "Unexpexted reply"

let webApp =
    choose [
        GET
        >=> choose [
                route "/api/getGrid"
                >=> warbler (fun _ -> getGrid ())
                route "/api/getNumFrames"
                >=> warbler (fun _ -> getNFrames ())
                routef "/api/getFrame/%i" getFrame
                route "/api/stepFrame"
                >=> warbler (fun _ -> stepFrame ())
                route "/api/getFrames"
                >=> warbler (fun _ -> getFrames ())
                route "/api/isRunning"
                >=> warbler (fun _ -> isRunning ())
                route "/api/getSimulation"
                >=> warbler (fun _ -> getSim ())
                route "/api/pauseSimulation"
                >=> warbler (fun _ -> pauseSim ())
                route "/api/resumeSimulation"
                >=> warbler (fun _ -> continueSim ())
                route "/api/resetSimulation"
                >=> warbler (fun _ -> resetSim ())
            ]
        POST
        >=> choose [
                route "/api/setSimulation" >=> setSim
                route "/api/startSimulation" >=> startSim
                route "/api/injectParticles" >=> injectParticles
            ]
    ]

let configureSerilog () =
    LoggerConfiguration()
        // .MinimumLevel.Information()
        .WriteTo
        .Console()
        .CreateLogger()

let serilog (logger: ILoggingBuilder) =
    logger
        .SetMinimumLevel(LogLevel.Warning)
        .AddSerilog()
    |> ignore

let app =
    Log.Logger <- configureSerilog ()

    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
        logging serilog
    }

let test () =
    // let p = 438441.812500, 7548383.500000
    let p = (68.05, 13.6)
    let grid = Grid.readGrid appsettings.grid
    Grid.printBBox grid
    Grid.debug grid

    Particle.initParticles grid 100 p
    |> Array.take 10
    |> Array.iter (printfn "%A")

    ()

// test ()
// Field.test ()
run app