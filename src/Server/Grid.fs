module Grid

open System
open System.IO
open FSharpPlus
open Serilog

type NodeIdx = int
type ElemIdx = int

type Elem = NodeIdx * NodeIdx * NodeIdx
type Node = single * single

type Grid = {
    Elem : Elem array
    Nodes : Node array
    NodeElems : Map<NodeIdx, Set<ElemIdx>>
}

type BBox = {
    minX : single
    maxX : single
    minY : single
    maxY : single
}

let chompLine (l: string) = l.Split ' '

let readHeader (h: string) =
    chompLine h
    |> function
    | [| nele; nnode |] -> int nele, int nnode
    | _ -> failwith "Invalid header"

let readElem (f: string array) =
    let next l =
        chompLine l
        |> function
        | [| a; b; c |] -> int a - 1, int b - 1, int c - 1
        | _ -> failwith "Invalid elem"
    f |> Array.map next

let readNodes (f: string array) =
    let next l =
        chompLine l
        |> function
        | [| x; y |] -> single x, single y
        | _ -> failwith "Invalid node"
    f |> Array.map next

let findNodeElems (elem : Elem array) =
    let f e n m =
        match Map.tryFind n m with
        | Some xs -> Set.add e xs
        | None -> e |> Set.singleton
        |> fun l -> Map.add n l m

    [ 0 .. Array.length elem - 1]
    |> List.fold (fun a e ->
        let x, y, z = elem.[e]
        let a' = f e x a
        let a'' = f e y a'
        f e z a''
    ) Map.empty

let getNeighbours (x, y, z) grid =
    let f n = Map.find n grid.NodeElems
    Set.union (f x) (f y) |> Set.union (f z)

let pointInsideElem grid (x, y, z) p =
    let sign (p1x, p1y) (p2x, p2y) (p3x, p3y) =
        (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)

    let v1 = grid.Nodes.[z]
    let v2 = grid.Nodes.[y]
    let v3 = grid.Nodes.[x]

    let d1 = sign p v1 v2
    let d2 = sign p v2 v3
    let d3 = sign p v3 v1

    let neg = (d1 <= 0.0f) || (d2 <= 0.0f) || (d3 <= 0.0f)
    let pos = (d1 >= 0.0f) || (d2 >= 0.0f) || (d3 >= 0.0f)

    neg && pos |> not

let findElement grid p =
    grid.Elem
    |> Array.fold (fun a e ->
        let n, found = a
        if found then n, true
        elif pointInsideElem grid e p
        then n, true
        else n + 1, false
    ) (0, false)
    |> fun (n, x) -> if x then Some n else None

let getBBox grid =
    grid.Nodes
    |> Array.fold (fun a (x, y) ->
        {
          minX = if x < a.minX then x else a.minX
          maxX = if x > a.maxX then x else a.maxX
          minY = if y < a.minY then y else a.minY
          maxY = if y > a.maxY then y else a.maxY
        }
    ) {
        minX = Single.MaxValue
        maxX = Single.MinValue
        minY = Single.MaxValue
        maxY = Single.MinValue
      }

let readGrid (filename: string) =
    let f = File.ReadAllLines filename

    let hdr, rest = Array.splitAt 1 f
    let nele, nnodes = readHeader hdr.[0]

    let els, nds = Array.splitAt nele rest
    let elem =readElem els

    {
        Elem = elem
        Nodes = readNodes nds
        NodeElems = findNodeElems elem
    }

let debug grid =
    if Log.IsEnabled Events.LogEventLevel.Debug then
        // let e = grid.Elem.[0]
        // let p = 443726.0625f, 7559435.5f
        let e = (7852, 8154, 7851)
        let p = (438578.4688f, 7548861.0f)
        let p' = (438574.3125f, 7548863.5f)
        let e' =
            findElement grid p
            |> Option.map (fun x -> grid.Elem.[x])
            |> Option.defaultValue (0,0,0)
        getNeighbours e grid |> sprintf "%A" |> Log.Debug
        pointInsideElem grid e p |> sprintf "%A %A" e |> Log.Debug
        pointInsideElem grid e p' |> sprintf "%A %A" e' |> Log.Debug
        findElement grid p |> sprintf "%A" |> Log.Debug
    else ()

let printBBox grid = getBBox grid |> printfn "%A"