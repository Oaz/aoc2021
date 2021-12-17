module aoc2021.Core.D17T

open NUnit.Framework
open D17
open Tools
open Geom2D

[<Test>]
let TestExample () =
  let input = (Coords(20,-10),Coords(30,-5))
  Assert.AreEqual((28,-7), pos (Coords(7,2)) 7)
  Assert.AreEqual((21,-9), pos (Coords(6,3)) 9)
  Assert.AreEqual((30,-6), pos (Coords(9,0)) 4)
  Assert.AreEqual(45, findVelocity input Seq.head)
  Assert.AreEqual(112, findVelocity input Seq.length)

[<Test>]
let TestParts () =
  let input = (Coords(248,-85),Coords(285,-56))
  Assert.AreEqual(3570, findVelocity input Seq.head)
  Assert.AreEqual(1919, findVelocity input Seq.length)
