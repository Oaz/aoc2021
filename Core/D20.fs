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

type Algorithm = Map<int,Pixel>
let readAlgorithm (input:string) : Algorithm = Seq.mapi (fun i c -> (i,charToPixel c)) input |> Map.ofSeq
let readInstructions (input:string list) : Algorithm*Image = readAlgorithm input.Head, readImage input.Tail.Tail

let enhance (a:Algorithm) (image:Image) : Image =
  let width = (Array2D.length2 image.center) + 2
  let height = (Array2D.length1 image.center) + 2
  let biggerImage = Array2D.init<Pixel> (height+2) (width+2) (fun _ _ -> image.others)
  biggerImage[2..(height-1),2..(width-1)] <- image.center
  let newImage = Array2D.zeroCreate<Pixel> height width
  let code x y = biggerImage[y..(y+2),x..(x+2)] |> Seq.cast<Pixel> |> Seq.fold (fun n b -> (n <<< 1)+b) 0
  for y in [0..(height-1)] do
    for x in [0..(width-1)] do
      newImage[y,x] <- a[code x y]
  { center=newImage; others=a[image.others*(a.Count-1)] }

let mEnhance (n:int) (a:Algorithm) (image:Image) = List.fold (fun x _ -> enhance a x) image [1..n]
let litCount (image:Image) : int = Seq.cast<Pixel> image.center |> Seq.sum
