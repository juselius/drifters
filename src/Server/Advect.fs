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
type Simulation =
    {
        t: float
        dt: float
        stepT: float
        endT: float
        particles: Particle array
        grid: AdvectionGrid
        dispatch: Inbox
    }

and Msg =
    | GetFrame of int
    | GetNumFrames
    | InjectParticles of Particle array
    | AppendParticles of Particle array
    | SetSim of Simulation * Field option
    | GetSim
    | Start of Simulation
    | Reset
    | Pause
    | Resume
    | QueryRunning
    | Step
    | RunAsync

and Reply =
    | Frame of (float * float) array
    | NumFrames of int
    | Simulation of Simulation option * Field option
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
            { p with Pos = pos; Age = p.Age + dt }
        elif dt < appsettings.minDt then
            match findElement grid pos with
            | Some x -> { Pos = pos; Age = p.Age + dt; Elem = Some x }
            | None ->
                Log.Information "stranded"
                { Pos = pos; Age = p.Age; Elem = None }
        else
            getNeighbours e grid
            |> Set.fold
                (fun a x ->
                    let e = grid.Elem.[x]

                    if pointInsideElem grid e pos then x else a)
                -1
            |> fun e ->
                if e < 0 then
                    let dt' = dt / 2.0
                    let p' = move grid field dt' p

                    if p'.Elem.IsNone then p' else move grid field dt' p'
                else
                    { Pos = pos; Age = p.Age + dt; Elem = Some e }

let advect uv dt grid particles =
    particles
    // |> Array.filter (fun x -> x.Elem < 0)
    |> Array.map (move grid uv dt)

let writePosData p t =
    let ps =
        p
        |> Array.map (fun x -> sprintf "%f %f" (fst x.Pos) (snd x.Pos))
        |> Array.fold (fun a x -> a + x + "\n") ""

    IO.File.WriteAllText(sprintf "output/pos-%d.dat" (int t), ps)

    sprintf "Wrote pos-%d.dat" (int t)
    |> Log.Information

let readUV t step =
    let n = t / step |> int

    sprintf "Read %s-%d.dat" appsettings.uv n
    |> Log.Information

    readUV appsettings.uv n

let avectStep (sim: Simulation) uv =
    let rec loop t (p: Particle array) =
        sprintf "Num particles %d" p.Length |> Log.Debug
        printfn "t=%f s" t
        let p' = advect uv sim.dt sim.grid p

        if (t % sim.stepT) = 0.0 then
            { sim with particles = p'; t = t + sim.dt }
        else
            loop (t + sim.dt) p'

    loop sim.t sim.particles
// sim.dispatch.PostAndReply (fun r -> r, Result sim)

type private State =
    {
        particles: (float * float) array array
        sim: Simulation option
        running: bool
        uv: Field option
    }

let controller () =
    MailboxProcessor.Start (fun (inbox: Inbox) ->
        let rec loop (state: State) =
            let particles = state.particles

            let doStep sim =
                let t = sim.t

                let t' =
                    (t - t % 86400.0) / 84600.0
                    |> fun x -> t - x * 84600.0

                printfn "t %A" (t, t')
                let uv = readUV t' sim.stepT
                let sim' = avectStep sim uv
                writePosData sim'.particles (int t)
                sim', uv

            async {
                let! reply, msg = inbox.Receive()

                let state' =
                    match msg with
                    | Msg.GetNumFrames ->
                        reply.Reply(Reply.NumFrames particles.Length)
                        state
                    | Msg.GetFrame n ->
                        reply.Reply(Reply.Frame particles.[n])
                        state
                    | Msg.InjectParticles p ->
                        match state.sim with
                        | Some s ->
                            let s' = { s with particles = Array.append s.particles p }
                            reply.Reply Reply.Empty
                            { state with sim = Some s' }
                        | None ->
                            reply.Reply Reply.Empty
                            state
                    | Msg.AppendParticles p ->
                        let p' = p |> Array.map (fun (x: Particle) -> x.Pos)
                        reply.Reply Reply.Empty
                        { state with particles = Array.append state.particles [| p' |] }
                    | Msg.Resume ->
                        match state.running, state.sim with
                        | false, Some sim ->
                            async { inbox.PostAndReply(fun r -> r, RunAsync) |> ignore }
                            |> Async.Start
                        | _ -> ()

                        reply.Reply Reply.Empty
                        { state with running = true }
                    | Msg.RunAsync ->
                        match state.running, state.sim with
                        | true, Some sim ->
                            async {
                                let s, uv = doStep sim

                                inbox.PostAndReply(fun r -> r, SetSim(s, Some uv))
                                |> ignore

                                inbox.PostAndReply(fun r -> r, AppendParticles s.particles)
                                |> ignore

                                inbox.PostAndReply(fun r -> r, RunAsync) |> ignore
                            }
                            |> Async.Start
                        | _ -> ()

                        reply.Reply Reply.Empty
                        state
                    | Msg.Start sim ->
                        let state' =
                            if not state.running then
                                async {
                                    inbox.PostAndReply(fun r -> r, SetSim(sim, None))
                                    |> ignore

                                    inbox.PostAndReply(fun r -> r, RunAsync) |> ignore
                                }
                                |> Async.Start

                                { state with running = true }
                            else
                                state

                        reply.Reply Reply.Empty
                        state'
                    | Msg.Step ->
                        match state.sim with
                        | Some sim ->
                            let s, uv = doStep sim

                            let p =
                                s.particles
                                |> Array.map (fun (x: Particle) -> x.Pos)

                            reply.Reply(Reply.Frame p)

                            { state with
                                particles = Array.append state.particles [| p |]
                                running = false
                                sim = Some s
                                uv = Some uv
                            }
                        | None ->
                            reply.Reply(Reply.Frame [||])
                            state
                    | Msg.Pause ->
                        reply.Reply Reply.Empty
                        { state with running = false }
                    | Msg.Reset ->
                        reply.Reply Reply.Empty

                        { sim = None; particles = [||]; running = false; uv = None }
                    | SetSim (sim, uv) ->
                        reply.Reply Reply.Empty
                        { state with sim = Some sim; uv = uv }
                    | GetSim ->
                        reply.Reply(Reply.Simulation(state.sim, None))
                        state
                    | QueryRunning ->
                        reply.Reply(Reply.IsRunning state.running)
                        state

                return! loop state'
            }

        loop { particles = Array.empty; sim = None; running = false; uv = None })

let track = controller ()