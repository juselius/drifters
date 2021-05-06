module Field

open System.IO
open Serilog

type Field = (single array) array

let readUV name t : Field =
    let filename = sprintf "%s-%d.dat" name t
    File.ReadAllText filename
    |> fun s -> s.Split '\n'
    |> Array.map (fun s ->
        s.Split ' '
        |> Array.map single
    )

let test () =
    readUV Settings.appsettings.uv 1
    |> Array.take 10
    |> Array.iter (printfn "%A")
