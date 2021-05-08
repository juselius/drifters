module Advect

open Serilog

open System
open Grid
open Field
open Particle
open Settings
open UTM
open Shared

// [<RequireQualifiedAccess>]
type Simulation = {
    dt : float
    stepT: float
    startT: float
    endT: float
    particles: Particle array
    grid: AdvectionGrid
    dispatch: Inbox
}
and Msg =
    | GetFrame of int
    | GetNumFrames
    | PutParticles of (float * float) array
    | SetSim of Simulation
    | GetSim
    | Start of Simulation
    | Reset
    | Pause
    | Continue
    | Query
and Reply =
    | Frame of (float * float) array
    | NumFrames of int
    | Simulation of Simulation option
    | IsRunning of bool
    | Empty
and Inbox = MailboxProcessor<AsyncReplyChannel<Reply> * Msg>

let rec move (grid: AdvectionGrid) (field: Field) dt (p: Particle) =
    match p.Elem with
    | None -> p
    | Some elem ->
        let x, y = p.Pos
        let e = grid.Elem.[elem]
        let u, v = field.[elem] |> fun h -> h.[0], h.[1]
        let pos = (x + u * dt, y + v * dt)
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
                    Elem = Some x
                }
            | None ->
                Log.Information "stranded"
                {
                    Pos = pos
                    Age = p.Age
                    Elem = None
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
                    if p'.Elem.IsNone  then p'
                    else move grid field dt' p'
                else
                    {
                        Pos = pos
                        Age = p.Age + dt
                        Elem = Some e
                    }

let advect uv dt grid particles =
    particles
    // |> Array.filter (fun x -> x.Elem < 0)
    |> Array.map (move grid uv dt)

let writePosData p t =
    let ps =
        p |> Array.map (fun x-> sprintf "%f %f" (fst x.Pos) (snd x.Pos))
        |> Array.fold (fun a x -> a + x + "\n") ""
    IO.File.WriteAllText (sprintf "output/pos-%d.dat" (int t),  ps)
    sprintf "Wrote pos-%d.dat" (int t) |> Log.Information

let readUV t step =
    let n = t / step |> int
    sprintf "Read %s-%d.dat" appsettings.uv n |> Log.Information
    readUV appsettings.uv n

let postParticles p (dispatch: Inbox) =
    let pll = p |> Array.map (fun x-> toLatLon x.Pos)
    dispatch.PostAndReply (fun r -> r, Msg.PutParticles pll) |> ignore

let runSimulation (sim: Simulation) =
    Array.unfold (fun (t, uv, (p: Particle array)) ->
        sprintf "Num particles %d" p.Length |> Log.Debug
        let t' = (t - t % 86400.0) / 84600.0  |> fun x -> t - x * 84600.0
        let uv' =
            if (t' % sim.stepT) = 0.0 && t' < sim.endT then
                writePosData p (int t)
                postParticles p sim.dispatch
                readUV t' sim.stepT
            else uv
        let isRunning =
            if (t % sim.stepT) = 0.0 then
                match sim.dispatch.PostAndReply (fun r -> r, Query) with
                | IsRunning yesno -> yesno
                | _ -> failwith "Unexpected reply"
            else
                true
        if t <= sim.endT && isRunning then
            printfn "t=%f s" t
            let p' = advect uv' sim.dt sim.grid p
            let sim' =
                { sim with
                    particles = p'
                    startT = t
                }
            Some ((), (t + sim.dt, uv', p'))
        else
            // sim.dispatch.PostAndReply (fun r -> r, Result sim)
            None
    ) (0.0, [||], sim.particles)

type private State = {
    particles: (float * float) array array
    sim: Simulation option
    running: bool
}

let controller () =
    MailboxProcessor.Start (fun (inbox : Inbox) ->
        let rec loop (state: State) =
            let particles = state.particles
            async {
                let! reply, msg = inbox.Receive ()
                let state' =
                    match msg with
                    | Msg.GetNumFrames ->
                        reply.Reply (Reply.NumFrames particles.Length)
                        state
                    | Msg.GetFrame n ->
                        reply.Reply (Reply.Frame particles.[n])
                        state
                    | Msg.PutParticles p ->
                        reply.Reply Reply.Empty
                        { state with particles = (Array.append particles [| p |]) }
                    | Msg.Continue ->
                        match state.sim with
                        | Some sim -> //inbox.PostAndReply (fun r -> r, (Start sim)) |> ignore
                            async { return runSimulation sim |> ignore } |> Async.Start
                            reply.Reply Reply.Empty
                            { state with running = true }
                        | None ->
                            reply.Reply Reply.Empty
                            state
                    | Msg.Start sim ->
                        if state.running then
                            reply.Reply Reply.Empty
                            state
                        else
                            async { return runSimulation sim |> ignore } |> Async.Start
                            reply.Reply Reply.Empty
                            { state with sim = Some sim; running = true }
                    | Msg.Pause ->
                        reply.Reply Reply.Empty
                        { state with running = false }
                    | Msg.Reset ->
                        reply.Reply Reply.Empty
                        { sim = None; particles = [||]; running = false }
                    | SetSim sim ->
                        reply.Reply Reply.Empty
                        { state with sim = Some sim }
                    | GetSim ->
                        reply.Reply (Reply.Simulation state.sim)
                        state
                    | Query ->
                        reply.Reply (Reply.IsRunning state.running)
                        state
                return! loop state'
            }
        loop {
            particles = Array.empty
            sim = None
            running = false
        }
    )

let track = controller ()