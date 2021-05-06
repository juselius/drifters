module Advect

open Serilog

open System
open Grid
open Field
open Particle
open Settings
open Shared

let fromLonLat (x: float, y: float) =
    let utm = ProjNet.CoordinateSystems.ProjectedCoordinateSystem.WGS84_UTM(33,true)
    let wgs = ProjNet.CoordinateSystems.GeographicCoordinateSystem.WGS84
    let f = ProjNet.CoordinateSystems.Transformations.CoordinateTransformationFactory()
    let transform = f.CreateFromCoordinateSystems(wgs, utm)
    transform.MathTransform.Transform(x, y).ToTuple() |> fun (a,b) -> b, a

let rec move (grid: AdvectionGrid) (field: Field) dt (p: Particle) =
    let x, y = fromLonLat p.Pos
    let e = grid.Elem.[p.Elem]
    let u, v = field.[p.Elem] |> fun h -> h.[0], h.[1]
    let pos = x + u * dt, y + v * dt
    if pointInsideElem grid e pos then
        { p with
            Pos = pos
            Age = p.Age + dt
        }
    elif dt < appsettings.minDt then
        match findElement grid pos with
        | Some x ->
            {
                Pos = pos
                Age = p.Age + dt
                Elem = x
            }
        | None ->
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
                let dt' = dt / 2.0
                move grid field dt' p
            else
                {
                    Pos = pos
                    Age = p.Age + dt
                    Elem = e
                }

let advect uv dt grid particles =
    particles
    |> Array.filter (fun x -> x.Age >= 0.0)
    |> Array.map (move grid uv dt)

let runSimulation (dt: float) time =
    // let p = 438441.812500, 7548383.500000
    let p = (68.05, 13.6)
    let grid = readGrid appsettings.grid
    let particles = initParticles grid 1000 p

    Array.unfold (fun (t, uv, (p: Particle array)) ->
        printfn "%A" p.Length
        let uv' =
            let t' = (t - t % 86400.0) / 84600.0  |> fun x -> t - x * 84600.0
            if (t' % 3600.0) = 0.0 && t' < time then
                let n = t' / 3600.0 |> int
                sprintf "Read %s-%d.dat" appsettings.uv n |> Log.Information
                readUV appsettings.uv n
            else uv
        if t <= time then
            printfn "t=%f s" t
            let p' = advect uv' dt grid p
            Some (p', (t + dt, uv', p'))
        else None
    ) (0.0, [||], particles)
    |> fun x ->
        printfn "%A" x.[0].Length
        printfn "%A" particles.Length
        x
