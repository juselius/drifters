module Particle

open Grid

type Particle = {
    Pos : single * single
    Age : int
    Elem: int
}

let createParticle grid p =
    {
        Pos = p
        Age = 0
        Elem =
            findElement grid p
            |> Option.defaultValue -1
    }

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
