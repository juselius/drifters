module Advect

open Serilog

open Grid
open Field
open Particle
open Settings

let rec move (grid: Grid) (field: Field) dt (p: Particle) =
    let x, y = p.Pos
    let e = grid.Elem.[p.Elem]
    let u, v = field.[p.Elem] |> fun h -> h.[0], h.[1]
    let pos = x + u * dt, y + v * dt
    if pointInsideElem grid e pos then
        { p with
            Pos = pos
            Age = p.Age + dt
        }
    elif dt < appsettings.minDt then
        Log.Information "stranded"
        {
            Pos = pos
            Age = -p.Age
            Elem = -1
        }
    else
        getNeighbours e grid
        |> Set.fold (fun a x ->
            let e = grid.Elem.[x]
            if pointInsideElem grid e pos then x else a
        ) -1
        |> fun e ->
            if e < 0 then
                let dt' = dt / 2.0f
                move grid field dt' p
            else
                {
                    Pos = pos
                    Age = p.Age + dt
                    Elem = e
                }

let advect dt grid particles =
    let f = readUV appsettings.uv 1
    particles
    |> Array.filter (fun x -> x.Age >= 0.0f)
    |> Array.map (move grid f dt)

let runSimulation (dt: single) time =
    let p = 438441.812500f, 7548383.500000f
    let grid = Grid.readGrid appsettings.grid
    let particles = initParticles grid 1000 p

    Array.unfold (fun (t, p) ->
        if t <= time then
            printfn "t=%f s" t
            let p' = advect dt grid p
            Some (p', (t + dt, p'))
        else None
    ) (0.0f, particles)