module aoc2021.Core.D16

open Tools
open Stack

let readBits (input:string) : bool[] =
  let hexToInt (c:char) : int = if c < 'A' then (int c) - (int '0') else (int c) - (int 'A') + 10
  let intToBin (i:int) : seq<bool> = Seq.unfold (fun x -> Some ((x &&& 8) = 8,x <<< 1)) i |> Seq.take 4
  Seq.collect (hexToInt >> intToBin) input |> Seq.toArray

let readInt (start:int) (length:int) (data:bool[]) : int =
  let addBit i b = if b then i+1 else i
  Seq.skip start data |> Seq.take length |> Seq.fold (fun i b -> addBit (i <<< 1) b) 0

type Packet =
  | Operator of version:int * typeID:int * packets:Packet list
  | Literal of version:int * value:bigint

type Command =
  | StartPacket
  | MakeNumber of value:bigint * again:bool
  | BitPackets of remainingSize:int * processed:int * index:int
  | CountPackets of remainingCount:int * processed:int * index:int
  
type Header = { version:int; typeID:int; index:int }

type Context =
  { data : bool[]
    index : int
    commands : Stack<Command>
    headers : Stack<Header>
    packets : Stack<Packet> }
  member this.Forward (size:int) = { this with index = this.index + size }
  member this.Position = this.index
  member this.Bool : bool = this.data[this.index]
  member this.Int (start:int) (size:int) = readInt (this.index+start) size this.data
  member this.Commands = this.commands
  member this.PushCommand (c : Command) = { this with commands = this.commands.Push c }
  member this.PopCommand (size : int) = { this with commands = this.commands.PopN size }
  member this.Headers = this.headers
  member this.PushHeader (h : Header) = { this with headers = this.headers.Push h }
  member this.PopHeader = { this with headers = this.headers.Pop }
  member this.Packets = this.packets
  member this.PushPacket (p : Packet) = { this with packets = this.packets.Push p }
  member this.PopPacket (size : int) = { this with packets = this.packets.PopN size }

let startPacket (c:Context) : Context =
  let typeID = c.Int 3 3
  let pc = c.Forward(6).PushHeader( { version = c.Int 0 3; typeID = typeID; index = c.Position } )
  if typeID = 4 then
    pc.PushCommand (MakeNumber (bigint 0,true))
  else if pc.Bool then
    pc.PushCommand(CountPackets (pc.Int 1 11, 0, c.index+18)).Forward(12)
  else
    pc.PushCommand(BitPackets (pc.Int 1 15,0, c.index+22)).Forward(16)

let addToNumber (n:bigint) (c:Context) : Context =
  c.Forward(5).PushCommand(MakeNumber ((n <<< 4)+(bigint (c.Int 1 4)),c.Bool))

let mkLiteral (n:bigint) (c:Context) : Context = c.PopHeader.PushPacket(Literal (c.Headers.Top.version,n))

let mkOperator (n:int) (c:Context) : Context =
  let header = c.Headers.Top
  c.PopHeader.PopPacket(n).PushPacket(Operator (header.version,header.typeID,c.Packets.TopN n |> List.rev))

let nextPacket (f:int*int*int -> Command) ((remaining,processed,index):int*int*int) (consumer:int -> int -> int) (c:Context) =
  if c.index > index  then
    let consumed = consumer c.index index
    let pc = c.PushCommand(f (remaining-consumed,processed+1,c.index))
    if remaining = consumed then pc else pc.PushCommand(StartPacket)
  else
    c.PushCommand(f (remaining,processed,c.index)).PushCommand(StartPacket)

let run (c:Context) : Context =
  let pc = c.PopCommand 1
  match c.Commands.Top with
  | StartPacket -> pc |> startPacket
  | MakeNumber (n,true) -> pc |> addToNumber n
  | MakeNumber (n,false) -> pc |> mkLiteral n
  | BitPackets (0,processed,_) -> pc |> mkOperator processed
  | BitPackets (r,p,i) -> pc |> nextPacket BitPackets (r,p,i) (-)
  | CountPackets (0,processed,_) -> pc |> mkOperator processed
  | CountPackets (r,p,i) -> pc |> nextPacket CountPackets (r,p,i) (fun a b -> if a > b then 1 else 0)

let readTransmission (data:bool[]) : Packet =
  { data=data; index=0; commands=Empty.Push(StartPacket); headers=Empty; packets=Empty }
  |> iterate run
  |> rollUntil (fun c -> c.Commands = Empty)
  |> (fun (c:Context) -> c.Packets.Top)

let rec versionSum (p:Packet) : int =
  match p with
  | Literal (version, _) -> version
  | Operator (version, _, packets) -> version + (List.map versionSum packets |> List.sum)

let rec compute (p:Packet) : bigint =
  let mapCompute = List.map compute
  let mapComputeCompare op = mapCompute >> (fun (l:bigint list) -> if op l[0] l[1] then bigint 1 else bigint 0)
  match p with
  | Literal (_, value) -> value
  | Operator (_, 0, packets) -> mapCompute packets |> List.sum
  | Operator (_, 1, packets) -> mapCompute packets |> List.reduce (*)
  | Operator (_, 2, packets) -> mapCompute packets |> List.min
  | Operator (_, 3, packets) -> mapCompute packets |> List.max
  | Operator (_, 5, packets) -> mapComputeCompare (>) packets
  | Operator (_, 6, packets) -> mapComputeCompare (<) packets
  | Operator (_, 7, packets) -> mapComputeCompare (=) packets
  | _ -> failwith "cannot compute"
