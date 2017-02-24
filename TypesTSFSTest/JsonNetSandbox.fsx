#r "../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

open Newtonsoft.Json

type Rectangle = 
    { Width : int
      Height : int }

type Square = 
    { Length : int }

type Circle = 
    { Radius : int
      Numbers : List<int> }

type Shape = 
    | Rectangle of Rectangle
    | Square of Square
    | MaybeCircle of Circle option

let circleRecord = 
    { Radius = 4
      Numbers = [ 1; 4; 3 ] }

JsonConvert.SerializeObject(circleRecord)

let shapeUnion : Shape = Shape.MaybeCircle (Some circleRecord)

JsonConvert.SerializeObject(shapeUnion)

let square : Shape = Shape.Square { Length = 5 }

JsonConvert.SerializeObject(square)