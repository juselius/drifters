module Grid

open System.IO

type NodeIdx = int
type ElemIdx = int

type Elem = NodeIdx * NodeIdx * NodeIdx
type Node = single * single

type Grid = {
    Nodes : Node array
    Elem : Elem array
    NodeElems : Map<NodeIdx, Set<ElemIdx>>
}

let chompLine (f: StreamReader) =
    f.ReadLine ()
    |> (fun s -> s.Split ' ')

let readHeader (f: StreamReader) =
    chompLine f
    |> function
    | [| nele; nnode |] -> int nele, int nnode
    | _ -> failwith "Invalid header"

let readElem (f: StreamReader) n =
    let next _ =
        chompLine f
        |> function
        | [| a; b; c |] -> int a, int b, int c
        | _ -> failwith "Invalid elem"
    [ 1 .. n ]
    |> List.map next
    |> Array.ofList

let readNodes (f: StreamReader) n =
    let next _ =
        chompLine f
        |> function
        | [| x; y |] -> single x, single y
        | _ -> failwith "Invalid node"
    [ 1 .. n ]
    |> List.map next
    |> Array.ofList

let findNodeElems (elem : Elem array) =
    let f e n m =
        match Map.tryFind n m with
        | Some xs -> Set.add (e + 1) xs
        | None -> e + 1 |> Set.singleton
        |> fun l -> Map.add n l m

    [ 0 .. Array.length elem - 1]
    |> List.fold (fun a e ->
        let x, y, z = elem.[e]
        let a' = f e x a
        let a'' = f e y a'
        f e z a''
    ) Map.empty

let getNeighbours e grid =
    let a, b, c = grid.Elem.[e - 1]
    let f x = Map.find x grid.NodeElems
    Set.union (f a) (f b) |> Set.union (f c)

let pointInsideElem grid e p =
    let det (ux, uy) (vx, vy) = ux * vy - uy * vx

    let x, y, z = grid.Elem.[e - 1]
    let v0 = grid.Nodes.[x]
    let v1 = grid.Nodes.[y]
    let v2 = grid.Nodes.[z]

    let a = (det p v2 - det v0 v2) / det v1 v2
    let b = -(det p v1 - det v0 v1) / det v1 v2
    // printfn "%A %A %A" v0 v1 v2
    // printfn "%f %f" a b
    a >= 0.0f && b >= 0.0f && a - b <= 1.0f

let findElement grid p =
    None

let readGrid () =
    let f = new StreamReader "/data/Buksnes/grid.grd"
    let nele, nnode = readHeader f
    let elem = readElem f nele
    let nodes = readNodes f nnode
    let ne = findNodeElems elem
    printfn "%A" ne
    let grid = {
        Nodes = nodes
        Elem = elem
        NodeElems = ne
    }
    getNeighbours 1 grid |> printfn "%A"
    pointInsideElem grid 1 (424252.125f, 7535351.0f) |> printfn "%A"
    grid