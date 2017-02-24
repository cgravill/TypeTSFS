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
    | Circle of Circle

let circleRecord = 
    { Radius = 4
      Numbers = [ 1; 4; 3 ] }

JsonConvert.SerializeObject(circleRecord)

let shapeUnion : Shape = Shape.Circle circleRecord

JsonConvert.SerializeObject(shapeUnion)