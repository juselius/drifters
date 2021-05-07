module Particle

open System.IO
open Grid
open Shared

let createParticle grid p =
    let a, e =
        match findElement grid p with
        | Some x -> 0.0, x
        | None -> -1.0, 0
    {
        Pos = p
        Age = a
        Elem = e
    }

let readParicles grid (filename: string) =
    let split (x: string) = x.Split ' '
    let conv (x: string array) = float x.[0], float x.[1]

    File.ReadAllLines filename
    |> Array.map (split >> conv >> createParticle grid)

let initParticles (grid: AdvectionGrid) npart (x, y) =
    let rnd = System.Random()
    Array.unfold (fun n ->
        if n < 1 then None
        else
            let r0 = rnd.NextDouble() * 2500.0 |> float
            let r1 = rnd.NextDouble() * 2500.0 |> float
            let p = x + r0, y + r1
            Some (p, n - 1)) npart
    |> Array.map (createParticle grid)
