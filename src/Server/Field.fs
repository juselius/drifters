module Field

open System.IO
open Serilog

let readUV name t =
    let filename = sprintf "%s-%d.dat" name t
    File.ReadAllText filename
    |> fun s -> s.Split '\n'
    |> Array.map (fun s ->
        s.Split ' '
        |> Array.map single
    )

let test () =
    readUV Settings.appsettings.uvName 1
    |> Array.iter (printfn "%A")
