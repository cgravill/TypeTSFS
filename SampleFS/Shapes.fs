namespace SampleFS

type Rectangle = {
    Width: int;
    Height: int;
}

type Square = {
    Length: int;
}

type Shape = 
    | Rectangle
    | Square

type ShapeWith = 
    | Rectangle of Rectangle
    | Square of Square