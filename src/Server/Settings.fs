module Settings

open System.IO
open Thoth.Json.Net
open Serilog

type Settings = {
    ExampleSetting: string
}

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let appsettings =
    let settings = System.IO.File.ReadAllText "appsettings.json"
    match Decode.Auto.fromString<Settings> settings with
    | Ok s -> s
    | Error e -> failwith e

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let listenAddress =
    "http://0.0.0.0:" + port.ToString ()

sprintf "Here we go again..." |> Log.Information
