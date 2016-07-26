namespace SampleWS

open WebSharper

[<JavaScript>]
module JSAPI =


    //let listy : List<int> = List.empty()

    //let listy2 = List.empty<Shapes.Square>()

    type CustomGeneric<'t>() =
        let getone = 1


    let something = CustomGeneric<int>()

    let makeShapes() =
        let rect = Shapes.ShapeWith.Rectangle{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}
        JavaScript.Console.Log rect
        //let circle = Shapes.ShapeWith.MaybeCircle (Some {Shapes.Circle.Radius = 7})
        //JavaScript.Console.Log circle


    let makeRectangle() =
        Json.Serialize {Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3}

    let makeRectangleShape() =
        Json.Serialize (Shapes.ShapeWith.Rectangle{Shapes.Rectangle.Width = 2; Shapes.Rectangle.Height = 3})