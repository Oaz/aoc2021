module aoc2021.Core.D17

open System
open Geom2D

let pos (v:Coords) (step:int) : int*int =
  let f n k = n*k-n*(n-1)/2
  if step < v.X then (f step v.X,f step v.Y) else (f v.X v.X,f step v.Y)

let inside ((min,max):Coords*Coords) ((x,y):int*int) : bool = x>=min.X && x<=max.X && y>=min.Y && y<=max.Y

let candidates ((min,max):Coords*Coords) : Coords list =
  // Maximum x is reached at step Vx
  // => x at step Vx must be >= target xMin
  // => Vx*Vx-Vx*(Vx-1)/2 >= target xMin
  // => (Vx²+Vx)/2 >= target xMin
  // => Vx >= -0.5 + √(0.25+2*(target xMin))
  let minV = Coords(int <| Math.Ceiling(-0.5 + Math.Sqrt(0.25+2.0*(float min.X))), min.Y)
  // at step 2*Vy+1, y = (2*Vy+1)*Vy-(2*Vy+1)*((2*Vy+1)-1)/2 = (2*Vy+1)*(Vy-(2*Vy)/2) = (2*Vy+1)*(Vy-Vy) = 0
  // => y is always equal to 0 at step 2*Vy+1
  // => at step 2*Vy+2, y must be >= target yMin - if not, it will jump over the target
  // => (2*Vy+2)*Vy-(2*Vy+2)*((2*Vy+2)-1)/2 >= target yMin
  // => (2*Vy+2)*(Vy-((2*Vy+1)/2)) >= target yMin
  // => (2*Vy+2)*(Vy-Vy-1/2) >= target yMin
  // => 2*Vy+2 <= -2*(target yMin)
  // => Vy <= - (target yMin) - 1
  let maxV = Coords( max.X, -min.Y-1)
  [
    for vy in [maxV.Y..(-1)..minV.Y] do
      for vx in [minV.X..maxV.X] do
        yield Coords(vx,vy)
  ]

let findVelocity ((min,max):Coords*Coords) f : int =
  let highestYorNoneIfTargetMissed (v:Coords) : int option =
    Seq.unfold (fun ((step,yTop):int*int) ->
      let x,y = pos v step
      let yBetter = Math.Max(y,yTop)
      if x > max.X || y < min.Y then
        None
      else if inside (min,max) (x,y) then
        Some (Some yBetter,(0,0))
      else
        Some (None,(step+1,yBetter))
    ) (0,0) |> Seq.choose id |> Seq.tryHead
  Seq.choose highestYorNoneIfTargetMissed (candidates (min,max)) |> f
