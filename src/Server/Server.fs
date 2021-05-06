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
let frames = Advect.runSimulation appsettings.dt (25.0 * 3600.0)

let private getGrid = json grid
let private getFrame n = json frames.[n]
let private getNumFrames = json frames.Length

let webApp =
    GET >=> choose [
        route "/api/getGrid" >=> getGrid
        route "/api/getNumFrames" >=> getNumFrames
        routef "/api/getFrame/%i" getFrame
    ]

let configureSerilog () =
    LoggerConfiguration()
        .MinimumLevel.Information()
        .WriteTo.Console()
        .CreateLogger()

let serilog (logger : ILoggingBuilder) =
    logger
        .SetMinimumLevel(LogLevel.Information)
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
