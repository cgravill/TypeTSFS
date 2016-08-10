namespace SampleWS

open WebSharper

[<ReflectedDefinition>]
module Shapes =

    type Rectangle = {
        Width: int;
        Height: int;
    }

    type Square = {
        Length: int;
    }

    type Circle = {
        Radius: int;
        Numbers: List<int>;
    }

    type Shape = 
        | [<Constant "Rectangle">] Rectangle
        | [<Constant "Square">] Square
        | [<Constant "Circle">] Circle

    [<NamedUnionCases>]
    type SingleDU = Single of Single:int

    [<NamedUnionCases "">]
    type ShapeWith = 
        | [<Name "Rectangle">] Rectangle of Rectangle
        | [<Name "Square">] Square of Square
        | [<Name "MaybeCircle">] MaybeCircle of Circle option