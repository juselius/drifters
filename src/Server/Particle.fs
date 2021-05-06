module Particle

open System.IO
open Grid

type Particle = {
    Pos : single * single
    Age : single
    Elem: int
}

let createParticle grid p =
    let a, e =
        match findElement grid p with
        | Some x -> 0.0f, x
        | None -> -1.0f, 0
    {
        Pos = p
        Age = a
        Elem = e
    }

let readParicles grid (filename: string) =
    let split (x: string) = x.Split ' '
    let conv (x: string array) = single x.[0], single x.[1]

    File.ReadAllLines filename
    |> Array.map (split >> conv >> createParticle grid)

let initParticles grid npart (x, y) =
    let rnd = System.Random()
    Array.unfold (fun n ->
        if n < 1 then None
        else
            let r0 = rnd.NextDouble() * 500.0 |> single
            let r1 = rnd.NextDouble() * 500.0 |> single
            let p = x + r0, y + r1
            Some (p, n - 1)) npart
    |> Array.map (createParticle grid)
