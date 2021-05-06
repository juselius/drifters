module Server

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2
open Saturn
open Giraffe
open Serilog

open Shared
open Settings

// let private addTodo (next: HttpFunc) (ctx: HttpContext) =
//     task {
//         let! todo = ctx.BindJsonAsync<Todo> ()
//         match storage.AddTodo todo with
//         | Ok () -> return! json todo next ctx
//         | Error e -> return! RequestErrors.BAD_REQUEST "fail" next ctx
//     }

let grid = Grid.readGrid appsettings.grid
let tracks = Advect.runSimulation appsettings.dt (25.0f * 3600.0f)

let private getGrid = json grid
let private getTrack n = json tracks.[n]
let private getNTracks = json tracks.Length

let webApp =
    GET >=> choose [
        route "/api/getGrid" >=> getGrid
        route "/api/getNTracks" >=> getNTracks
        routef "/api/getTrack/%i" getTrack
    ]

let configureSerilog () =
    LoggerConfiguration()
        .MinimumLevel.Debug()
        .WriteTo.Console()
        .CreateLogger()

let serilog (logger : ILoggingBuilder) =
    logger
        .SetMinimumLevel(LogLevel.Debug)
        .AddSerilog() |> ignore

let app =
    Log.Logger <- configureSerilog ()

    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
        logging serilog
    }

let test () =
    let p = 438441.812500f, 7548383.500000f
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
