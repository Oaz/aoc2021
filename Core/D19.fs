module aoc2021.Core.D19

open System
open Tools
open FSharp.Stats

type Coord = Vector<float>
let readCoord (input: string) : Coord = splitOnComma input |> List.map (Int32.Parse >> float) |> vector

type Distance = float
let distance ((a,b):Coord*Coord) : float = Vector.norm (a-b)

type Scan =
  { id: int
    beacons : Set<Coord>
    distancesByPairs : Map<Coord*Coord,Distance>
    distances : Set<Distance> }

let readScan (input: string list) : Scan =
  let coords = List.map readCoord input.Tail
  let pair (a,b) = if a=b then None else Some ((a,b), distance (a,b))
  let distancesByPairs = List.allPairs coords coords |> List.choose pair |> Map.ofList
  {
    id = (splitOnSpaces input.Head)[2] |> Int32.Parse 
    beacons = Set.ofList coords
    distancesByPairs = distancesByPairs
    distances = distancesByPairs.Values |> Set.ofSeq
  }

let readReport (input: string list) : Scan list =
  List.filter (fun xs -> String.length xs > 0) input
  |> splitOnHeader (fun (h:string) -> h.StartsWith("---"))
  |> List.map readScan

type Transformation = Matrix<float>*Vector<float>

type Overlap =
  {
    scan1 : Scan
    scan2 : Scan
    beacons1 : Set<Coord>
    beacons2 : Set<Coord>
    from2To1 : Transformation
    from1To2 : Transformation
  }
  override this.ToString() = $"({this.scan1.id},{this.scan2.id})"

let barycenter (bs:Set<Coord>) : Coord = Set.toList bs |> List.reduce (+) |> Vector.scale (1.0/(float bs.Count))
let rotX = matrix [ [ 1;  0; 0 ]; [ 0;  0; 1 ]; [ 0; -1; 0 ] ]
let rotY = matrix [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ -1; 0; 0 ] ]
let rotZ = matrix [ [ 0; 1; 0 ]; [ -1; 0; 0 ]; [  0; 0; 1 ] ]
let rotations =
  let basic = List.collect (fun m -> [m;m*m;m*m*m]) [rotX;rotY;rotZ]
  List.allPairs basic basic |> List.collect (fun (a,b) -> [a;a*b]) |> List.distinct

let findTransformation (bs1:Set<Coord>) (bs2:Set<Coord>) : Transformation =
  let b1 = barycenter bs1
  let getRT (rotation:Matrix<float>) : (Matrix<float>*Vector<float>) option =
    let rotated = Set.map (fun b -> rotation*b) bs2
    let translation = b1 - (barycenter rotated)
    let transformed = Set.map (fun b -> b+translation) rotated
    if bs1 = transformed then Some (rotation,translation) else None
  List.choose getRT rotations |> List.head

let findBeaconsInScanProducingTheGivenDistances (size:int) (ds:Set<Distance>) (s:Scan) : Set<Coord> =
  let beaconCandidates =
    Map.filter (fun _ d -> ds.Contains d) s.distancesByPairs
    |> Map.keys
    |> Seq.collect (fun (c1,c2) -> [c1;c2])
    |> Set.ofSeq
  let isInside (b:Coord) =
    let distancesToOthers = Set.map (fun b2 -> distance(b,b2)) beaconCandidates
    (Set.intersect distancesToOthers ds).Count >= size-1
  if beaconCandidates.Count = size then beaconCandidates else Set.filter isInside beaconCandidates

let findOverlaps (size:int) (scans:Scan list) : Overlap list =
  let getOverlap ((s1,s2):Scan*Scan) : Overlap option =
    if s1.id >= s2.id then None else
      match Set.intersect s1.distances s2.distances with
      | ds when ds.Count >= size*(size-1)/2 ->
        let b1 = findBeaconsInScanProducingTheGivenDistances size ds s1
        let b2 = findBeaconsInScanProducingTheGivenDistances size ds s2
        Some { scan1=s1; scan2=s2;
               beacons1=b1; beacons2=b2
               from2To1=findTransformation b1 b2
               from1To2=findTransformation b2 b1 }
      | _ -> None
  List.allPairs scans scans |> List.choose getOverlap |> List.distinct

let getAllTransformations (overlaps:Overlap list) : Map<int,Transformation> =
  let reference = Map.empty.Add(0,(Matrix.identity 3,Vector.zeroCreate 3))
  let compose (r1,t1) (r2,t2) = ((r2*r1),r2*t1+t2)
  let forward (o:Overlap) = (o.from1To2, o.scan1.id, o.scan2.id)
  let backward (o:Overlap) = (o.from2To1, o.scan2.id, o.scan1.id)
  let useOverlap dir (ts:Map<int,Transformation>) = ts.Add((snd3 dir), compose (fst3 dir) ts[thd3 dir])
  let tryOverlap (direction:Overlap -> Transformation*int*int) ((ts,os):Map<int,Transformation>*Set<Overlap>)
      : (Map<int,Transformation>*Set<Overlap>) option =
    let uos = Set.filter (fun (o:Overlap) -> ts.ContainsKey (direction o |> thd3)) os
    if uos.Count > 0 then
      let useOverlap ts o = useOverlap (direction o) ts
      let newTs = Set.fold useOverlap ts uos
      Some (newTs, os - uos)
    else
      None
  let searchOverlap s = List.choose (fun t -> t s) [tryOverlap backward; tryOverlap forward] |> List.head
  iterate searchOverlap (reference,Set.ofList overlaps)
  |> rollUntil (fun (_,os) -> os.Count = 0) |> fst

let round = Vector.map (fun v -> (Math.Round v))
let transform ((r,t):Transformation) (bs:Set<Coord>) : Set<Coord> = Set.map (fun (b:Coord) -> r * b + t |> round) bs
  
let getAllBeacons (ts:Map<int,Transformation>) (report:Scan list) : Set<Coord> =
  List.map (fun (s:Scan) -> transform ts[s.id] s.beacons) report |> Set.unionMany
  
let manhattan ((c1,c2):Coord*Coord) : int = Vector.map abs (c1 - c2) |> Vector.fold (+) 0.0 |> int

let oceanSize (ts:Map<int,Transformation>) : int =
  let scanners : Coord list = Seq.map snd ts.Values |> List.ofSeq
  List.allPairs scanners scanners |> List.map manhattan |> List.max
