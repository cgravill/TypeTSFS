[<WebSharper.JavaScript>]
module JSAPI

open WebSharper

type CustomGeneric<'t>() =
    let getone = 1


let something = CustomGeneric<int>()

(*let makeShapes() =
    let rect = Shapes.ShapeWith.Rectangle{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}
    rect*)

let makeRectangle() =
    {Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}

let makeRectangleDiscrimatedUnion() =
    Shapes.ShapeWith.Rectangle {Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}

let makeRectangleShape() =
    Json.Serialize (Shapes.ShapeWith.Rectangle{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3})


let makeRectangles() = 
    [{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}; {Shapes.Rectangle.Width = 4; Shapes.Rectangle.Height = 1}]

(*let makeRectanglesEncode() =
    Json.Encode [{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}; {Shapes.Rectangle.Width = 4; Shapes.Rectangle.Height = 1}]

let makeRectanglesJSON() =
    Json.Serialize [{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}; {Shapes.Rectangle.Width = 4; Shapes.Rectangle.Height = 1}]


let makePossibleRectangles() =
    [Some{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}; None]

let makeRectangleSerialize() =
    Json.Serialize {Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}

let getSingleDU() =
    Json.Encode (Shapes.SingleDU.Single 6)*)

let makeLookUp() =
    Map.empty |> Map.add 0 {Shapes.Rectangle.Width = 4; Shapes.Rectangle.Height = 10}

let shapeLookUp(look:Map<int,Shapes.Rectangle>) =
    look.[0]

let shapeLookUpDict(look:System.Collections.Generic.Dictionary<int,Shapes.Rectangle>) =
    look.[0]