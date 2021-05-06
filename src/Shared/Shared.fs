namespace Shared

open System

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