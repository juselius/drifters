namespace Shared

open System

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        {
            Id = Guid.NewGuid()
            Description = description
        }

type NodeIdx = int
type ElemIdx = int

type Elem = NodeIdx * NodeIdx * NodeIdx
type Node = single * single

type Grid = {
    Elem : Elem array
    Nodes : Node array
}

type Particle = {
    Pos : single * single
    Age : single
    Elem: int
}