namespace Shared

type NodeIdx = int
type ElemIdx = int

type Elem = NodeIdx * NodeIdx * NodeIdx
type Node = float * float

type Grid = {
    Elem : Elem array
    Nodes : Node array
}

type Particle = {
    Pos : float * float
    Age : float
    Elem: int
}