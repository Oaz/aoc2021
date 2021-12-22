module aoc2021.Core.D21T

open NUnit.Framework
open D21
open Tools

[<Test>]
let TestExample () =
  Assert.AreEqual(739785, deterministicGame 4 8)
  let cu4 = diracWins 4 |> countUniverses
  Assert.AreEqual(
    Map.ofList [
      (1, Ratio (0I,27I))
      (2, Ratio (0I,729I))
      (3, Ratio (4608I,19683I))
      (4, Ratio (249542I,407025I))
      (5, Ratio (3219454I,4252041I))
      (6, Ratio (24905476I,27879849I))
      (7, Ratio (77993473I,80308071I))
      (8, Ratio (62172638I,62494146I))
      (9, Ratio (8678745I,8680716I))
      (10, Ratio (53217I,53217I))
    ], cu4)
  let wu = weavedUniverses cu4 (diracWins 8 |> countUniverses)
  Assert.AreEqual((444356092776315I,341960390180808I), wu)

[<Test>]
let TestParts () =
  Assert.AreEqual(855624, deterministicGame 4 10)
  Assert.AreEqual(
    (187451244607486I,183752194019471I),
    weavedUniverses (diracWins 4 |> countUniverses) (diracWins 10 |> countUniverses)
  )
