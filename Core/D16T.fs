module aoc2021.Core.D16T

open NUnit.Framework
open D16
open Tools

let bools : string -> bool[] = Seq.map (fun c -> c = '1') >> Array.ofSeq

[<Test>]
let ReadBits () =
  Assert.AreEqual(bools "110100101111111000101000", readBits "D2FE28")

[<TestCase("110", 6)>]
[<TestCase("100", 4)>]
[<TestCase("011111100101", 2021)>]
let ReadInt (bs:string) (expected:int)  =
  Assert.AreEqual(expected, bools bs |> readInt 0 bs.Length)

[<Test>]
let TestExample0 () =
  Assert.AreEqual(Literal (6,bigint 2021), "D2FE28" |> readBits |> readTransmission)

[<Test>]
let TestExample1 () =
  Assert.AreEqual(
    Operator (1,6,[Literal (6,bigint 10); Literal (2,bigint 20)]),
    "38006F45291200" |> readBits |> readTransmission
  )
  Assert.AreEqual( 9, "38006F45291200" |> readBits |> readTransmission |> versionSum)

[<Test>]
let TestExample2 () =
  Assert.AreEqual(
    Operator (7,3,[Literal (2,bigint 1); Literal (4,bigint 2); Literal (1,bigint 3)]),
    "EE00D40C823060" |> readBits |> readTransmission
  )
  Assert.AreEqual( 14, "EE00D40C823060" |> readBits |> readTransmission |> versionSum)

[<TestCase("8A004A801A8002F478",16)>]
[<TestCase("620080001611562C8802118E34",12)>]
[<TestCase("C0015000016115A2E0802F182340",23)>]
[<TestCase("A0016C880162017C3686B18A3D4780",31)>]
let TestOtherExamples (input:string) (expectedSum:int) =
  Assert.AreEqual( expectedSum, input |> readBits |> readTransmission |> versionSum)

[<TestCase("C200B40A82",3)>]
[<TestCase("04005AC33890",54)>]
[<TestCase("880086C3E88112",7)>]
[<TestCase("CE00C43D881120",9)>]
[<TestCase("D8005AC2A8F0",1)>]
[<TestCase("F600BC2D8F",0)>]
[<TestCase("9C005AC2F8F0",0)>]
[<TestCase("9C0141080250320F1802104A08",1)>]
let TestOperationExamples (input:string) (expectedSum:int) =
  Assert.AreEqual( bigint expectedSum, input |> readBits |> readTransmission |> compute)

[<Test>]
let TestParts () =
  let bits = (inputForDay 16).Trim() |> readBits
  let packet = readTransmission bits
  Assert.AreEqual(847, versionSum packet)
  Assert.AreEqual(bigint 333794664059L, compute packet)


