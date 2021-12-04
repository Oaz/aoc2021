module aoc2021.D04T

open System
open NUnit.Framework
open aoc2021.D04

[<SetUp>]
let Setup () = ()

[<Test>]
let TestExample () =
  let input: string list =
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
      ""
      "22 13 17 11  0"
      " 8  2 23  4 24"
      "21  9 14 16  7"
      " 6 10  3 18  5"
      " 1 12 20 15 19"
      ""
      " 3 15  0  2 22"
      " 9 18 13 17  5"
      "19  8  7 25 23"
      "20 11 10 24  4"
      "14 21 16 12  6"
      ""
      "14 21 17 24  4"
      "10 16 15  9 19"
      "18  8 23 26 20"
      "22 11 13  6  5"
      " 2  0 12  3  7" ]

  let game = Game.ReadFrom input
  let cell n d = { Number = n; Drawn = d }
  let grid cl = { Cells = cl; Bingo = false }
  let winninggrid cl = { Cells = cl; Bingo = true }
  let initialGame =
    {
      NextDrawn = [ 7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24; 10; 16; 13; 6; 15; 25; 12; 22; 18; 20; 8; 19; 3; 26; 1 ]
      Grids =
        [
          grid [ cell 22 false; cell 13 false; cell 17 false; cell 11 false; cell  0 false;
            cell  8 false; cell  2 false; cell 23 false; cell  4 false; cell 24 false;
            cell 21 false; cell  9 false; cell 14 false; cell 16 false; cell  7 false;
            cell  6 false; cell 10 false; cell  3 false; cell 18 false; cell  5 false;
            cell  1 false; cell 12 false; cell 20 false; cell 15 false; cell 19 false
          ];
          grid [ cell  3 false; cell 15 false; cell  0 false; cell  2 false; cell 22 false;
            cell  9 false; cell 18 false; cell 13 false; cell 17 false; cell  5 false;
            cell 19 false; cell  8 false; cell  7 false; cell 25 false; cell 23 false;
            cell 20 false; cell 11 false; cell 10 false; cell 24 false; cell  4 false;
            cell 14 false; cell 21 false; cell 16 false; cell 12 false; cell  6 false
          ];
          grid [ cell 14 false; cell 21 false; cell 17 false; cell 24 false; cell  4 false;
            cell 10 false; cell 16 false; cell 15 false; cell  9 false; cell 19 false;
            cell 18 false; cell  8 false; cell 23 false; cell 26 false; cell 20 false;
            cell 22 false; cell 11 false; cell 13 false; cell  6 false; cell  5 false;
            cell  2 false; cell  0 false; cell 12 false; cell  3 false; cell  7 false
          ]          
        ]
    }
  Assert.AreEqual(initialGame, game)
  
  Assert.AreEqual(
    {
      NextDrawn = [ 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24; 10; 16; 13; 6; 15; 25; 12; 22; 18; 20; 8; 19; 3; 26; 1 ]
      Grids =
        [
          grid [ cell 22 false; cell 13 false; cell 17 false; cell 11 false; cell  0 false;
            cell  8 false; cell  2 false; cell 23 false; cell  4 false; cell 24 false;
            cell 21 false; cell  9 false; cell 14 false; cell 16 false; cell  7 true;
            cell  6 false; cell 10 false; cell  3 false; cell 18 false; cell  5 false;
            cell  1 false; cell 12 false; cell 20 false; cell 15 false; cell 19 false
          ];
          grid [ cell  3 false; cell 15 false; cell  0 false; cell  2 false; cell 22 false;
            cell  9 false; cell 18 false; cell 13 false; cell 17 false; cell  5 false;
            cell 19 false; cell  8 false; cell  7 true;  cell 25 false; cell 23 false;
            cell 20 false; cell 11 false; cell 10 false; cell 24 false; cell  4 false;
            cell 14 false; cell 21 false; cell 16 false; cell 12 false; cell  6 false
          ];
          grid [ cell 14 false; cell 21 false; cell 17 false; cell 24 false; cell  4 false;
            cell 10 false; cell 16 false; cell 15 false; cell  9 false; cell 19 false;
            cell 18 false; cell  8 false; cell 23 false; cell 26 false; cell 20 false;
            cell 22 false; cell 11 false; cell 13 false; cell  6 false; cell  5 false;
            cell  2 false; cell  0 false; cell 12 false; cell  3 false; cell  7 true
          ]          
        ]
    }, Seq.head <| game.Play())
  
  Assert.AreEqual(
    ({
      NextDrawn = [ 10; 16; 13; 6; 15; 25; 12; 22; 18; 20; 8; 19; 3; 26; 1 ]
      Grids =
        [
          grid [ cell 22 false; cell 13 false; cell 17 true; cell 11 true; cell  0 true;
            cell  8 false; cell  2 true; cell 23 true; cell  4 true; cell 24 true;
            cell 21 true; cell  9 true; cell 14 true; cell 16 false; cell  7 true;
            cell  6 false; cell 10 false; cell  3 false; cell 18 false; cell  5 true;
            cell  1 false; cell 12 false; cell 20 false; cell 15 false; cell 19 false
          ];
          grid [ cell  3 false; cell 15 false; cell  0 true; cell  2 true; cell 22 false;
            cell  9 true; cell 18 false; cell 13 false; cell 17 true; cell  5 true;
            cell 19 false; cell  8 false; cell  7 true;  cell 25 false; cell 23 true;
            cell 20 false; cell 11 true; cell 10 false; cell 24 true; cell  4 true;
            cell 14 true; cell 21 true; cell 16 false; cell 12 false; cell  6 false
          ];
          winninggrid [ cell 14 true; cell 21 true; cell 17 true; cell 24 true; cell  4 true;
            cell 10 false; cell 16 false; cell 15 false; cell  9 true; cell 19 false;
            cell 18 false; cell  8 false; cell 23 true; cell 26 false; cell 20 false;
            cell 22 false; cell 11 true; cell 13 false; cell  6 false; cell  5 true;
            cell  2 true; cell  0 true; cell 12 false; cell  3 false; cell  7 true
          ]          
        ]
    },2), game.ToFirstWinningGrid())
  
  Assert.AreEqual(4512, game.ToFirstWinningGrid() |> Score game)
  Assert.AreEqual(1924, game.ToLastWinningGrid() |> Score game)


[<Test>]
let TestParts () =
  let input =
    Tools.inputForDay 4
    |> Tools.splitLines

  let game = Game.ReadFrom input
  Assert.AreEqual(38913, game.ToFirstWinningGrid() |> Score game)
  Assert.AreEqual(16836, game.ToLastWinningGrid() |> Score game)
