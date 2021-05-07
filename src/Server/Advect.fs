module Advect

open Serilog

open System
open Grid
open Field
open Particle
open Settings
open UTM
open Shared

[<RequireQualifiedAccess>]
type Msg =
    | GetFrame of int
    | GetNumFrames
    | PutParticles of (float * float) array
    | Reset

[<RequireQualifiedAccess>]
type Reply =
    | Frame of (float * float) array
    | NumFrames of int
    | Empty

type Inbox = MailboxProcessor<AsyncReplyChannel<Reply> * Msg>

let track =
    MailboxProcessor.Start (fun (inbox : Inbox) ->
        let rec loop (particles : (float * float) array array) =
            async {
                match! inbox.Receive () with
                | reply, Msg.GetNumFrames ->
                    reply.Reply (Reply.NumFrames particles.Length)
                    return! loop particles
                | reply, Msg.GetFrame n ->
                    reply.Reply (Reply.Frame particles.[n])
                    return! loop particles
                | reply, Msg.PutParticles p ->
                    reply.Reply Reply.Empty
                    return! loop (Array.append particles [| p |])
                | reply, Msg.Reset ->
                    reply.Reply Reply.Empty
                    return! loop [||]
            }
        loop [||]
    )

let rec move (grid: AdvectionGrid) (field: Field) dt (p: Particle) =
    let x, y = p.Pos
    let e = grid.Elem.[p.Elem]
    let u, v = field.[p.Elem] |> fun h -> h.[0], h.[1]
    let pos = (x + u * dt, y + v * dt)
    // printfn "%A %A %A" p.Pos (x, y) pos
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
                let p' = move grid field dt' p
                if p'.Elem < 0 then
                    p'
                else
                    move grid field dt' p'
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
    let p = fromLatLon (68.05, 13.6)
    let grid = readGrid appsettings.grid
    let particles = initParticles grid 1000 p

    Array.unfold (fun (t, uv, (p: Particle array)) ->
        printfn "%A" p.Length
        let uv' =
            let t' = (t - t % 86400.0) / 84600.0  |> fun x -> t - x * 84600.0
            if (t' % 3600.0) = 0.0 && t' < time then
                let ps =
                    p |> Array.map (fun x->
                        sprintf "%f %f" (fst x.Pos) (snd x.Pos))
                    |> Array.fold (fun a x -> a + x + "\n") ""
                IO.File.WriteAllText (sprintf "pos-%d.dat" (int t),  ps)
                sprintf "Wrote pos-%d.dat" (int t) |> Log.Information
                let pll = p |> Array.map (fun x-> toLatLon x.Pos)
                track.PostAndReply (fun r -> r, Msg.PutParticles pll) |> ignore
                let n = t' / 3600.0 |> int
                sprintf "Read %s-%d.dat" appsettings.uv n |> Log.Information
                readUV appsettings.uv n
            else uv
        if t <= time then
            printfn "t=%f s" t
            let p' = advect uv' dt grid p
            Some (p'.Length, (t + dt, uv', p'))
        else None
    ) (0.0, [||], particles)
    // |> Array.map (fun x ->
    //     Array.map (fun p -> { p with Pos = toLatLon p.Pos }) x)

