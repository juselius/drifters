module Advect

open Grid
open Field
open Particle
open Settings

let move (grid: Grid) (field: Field) dt (p: Particle) =
    let x, y = p.Pos
    let e = grid.Elem.[p.Elem - 1]
    let u, v = field.[p.Elem - 1] |> fun h -> h.[0], h.[1]
    let pos = x + u * dt, y + v * dt
    if pointInsideElem grid e pos then
        printfn "inside"
        {
            Pos = pos
            Age = p.Age + dt
            Elem = p.Elem
        }
    else
        printfn "pos = %A %A" p.Pos pos
        failwith "offside2"

let advect dt grid particles =
    let f = readUV appsettings.uvName 1
    particles
    |> Array.map (move grid f dt)

let runSimulation () =
    let p = 438441.812500f, 7548383.500000f
    let grid = Grid.readGrid appsettings.gridFile
    let particles = Particle.initParticles grid 100 p
    let dt = 60.0f

    [0..10] |> List.fold (fun a t ->
        single t * dt |> printfn "t=%f s"
        advect dt grid a
    ) particles