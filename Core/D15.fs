module aoc2021.Core.D15

open SCGraphTheory.AdjacencyList
open SCGraphTheory.Search.Classic
open Tools
open Geom2D

let readCave (input: string list) : Zone * Map<Coords, int> = readZone input (Seq.map charToInt)

let makeItBig ((zone,points):Zone*Map<Coords, int>) : Zone*Map<Coords, int> =
  let bigZone = Zone(zone.TopLeft, zone.TopLeft+(5*zone.Size)-Coords(1,1))
  let biggerRisk (c:Coords) : Coords*int =
    let c0 = Coords(c.X % zone.Size.X, c.Y % zone.Size.Y)
    let r = points[c0] + (c.X / zone.Size.X) + (c.Y / zone.Size.Y)
    c,r-9*((r-1)/9)
  bigZone,Seq.map biggerRisk bigZone.AllCoords |> Map.ofSeq

type Node(i:int,c:Coords) =
  inherit NodeBase<Node, Edge>()
  member this.Id = i
  member this.Coords = c
  override this.ToString() : string = $"ID={i} COORDS={c}"
and Edge(n1:Node, n2:Node, risk:int) =
  inherit EdgeBase<Node, Edge>(n1,n2)
  member this.Risk : float32 = float32 risk
  override this.ToString() : string = $"FROM [{n1}] TO [{n2}] RISK={risk}"
  
let findLowerRisk ((zone,risks):Zone*Map<Coords, int>) : int =
  let index (c:Coords) = c.Y*zone.Size.X+c.X
  let nodes : Node[] =
    Seq.map (fun (c:Coords) -> Node(index c,c)) zone.AllCoords |> Seq.toArray
  let neighbors (n:Node) : seq<Node> =
    n.Coords.AdjacentCoordsHV
    |> List.filter zone.InBounds
    |> Seq.map (fun c -> nodes[index c])
  let graph = Graph<Node,Edge>()
  for node in nodes do
    graph.Add(node)
  for node in nodes do
    for neighbor in neighbors node do
      graph.Add(Edge(node,neighbor,risks[neighbor.Coords]))
  let dijkstra = DijkstraSearch<Node, Edge>(
    nodes[0],
    (fun (n:Node) -> n.Coords = zone.BottomRight),
    (fun (e:Edge) -> e.Risk)
  )
  dijkstra.Complete()
  dijkstra.PathToTarget() |> Seq.map (fun (e:Edge) -> e.Risk) |> Seq.sum |> int

  