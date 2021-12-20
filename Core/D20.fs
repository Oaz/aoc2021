module aoc2021.Core.D20

open FSharpPlus

type Pixel = int
let charToPixel p = if p = '#' then 1 else 0
type Image = {
  center: Pixel[,]
  others: Pixel
}
let readImage (input:string list) : Image =
  let image = Array2D.zeroCreate<Pixel> input.Length input.Head.Length
  for y,row in List.mapi tuple2 input do
    for x,pixel in Seq.mapi tuple2 row do
      image[y,x] <- charToPixel pixel
  { center=image; others=0 }

type Algorithm = Pixel[]
let readAlgorithm (input:string) : Algorithm = Seq.map (fun c -> charToPixel c) input |> Array.ofSeq
let readInstructions (input:string list) : Algorithm*Image = readAlgorithm input.Head, readImage input.Tail.Tail

let enhance (a:Algorithm) (image:Image) : Image =
  let width = (Array2D.length2 image.center) + 2
  let height = (Array2D.length1 image.center) + 2
  let biggerImage = Array2D.init<Pixel> (height+2) (width+2) (fun _ _ -> image.others)
  biggerImage[2..(height-1),2..(width-1)] <- image.center
  let inline pix x y : int = biggerImage[y,x]
  let inline code x y =
    (pix x y)*256 + (pix (x+1) y)*128 + (pix (x+2) y)*64
     + (pix x (y+1))*32 + (pix (x+1) (y+1))*16 + (pix (x+2) (y+1))*8
     + (pix x (y+2))*4 + (pix (x+1) (y+2))*2 + (pix (x+2) (y+2))*1
  let newImage = Array2D.zeroCreate<Pixel> height width
  for y in [0..(height-1)] do
    for x in [0..(width-1)] do
      newImage[y,x] <- a[code x y]
  { center=newImage; others=a[image.others*(a.Length-1)] }
  
let mEnhance (n:int) (a:Algorithm) (image:Image) = List.fold (fun x _ -> enhance a x) image [1..n]
let litCount (image:Image) : int = Seq.cast<Pixel> image.center |> Seq.sum
