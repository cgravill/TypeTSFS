module WebSharperTests

open Xunit

[<Fact>]
let ``Record``() =

    let sampleText = """
module Shapes

type Circle = {
    Radius: int;
    Numbers: List<int>;
}
    """

    let entities = ProjectManager.extractEntitites sampleText

    let moduleEntity = entities.[0]

    Assert.Equal("Shapes", moduleEntity.DisplayName)

    let nestedEntities = Explore.findEntities moduleEntity |> Array.ofSeq

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "sample" nestedEntities
    
    let expected =
        """
    export namespace sample {
        export interface Shapes { }
        export interface Circle {
            Radius: number;
            Numbers: Array<number>;
        }
    }"""

    Assert.Equal(expected.[2..], output)

[<Fact>]
let ``Discriminated union``() =

    let sampleText = """
module Shapes

type Circle = 
    { Radius : int
      Numbers : List<int> }

type Shape = 
    | Circle of Circle
    """

    let entities = ProjectManager.extractEntitites sampleText

    let moduleEntity = entities.[0]

    Assert.Equal("Shapes", moduleEntity.DisplayName)

    let nestedEntities = Explore.findEntities moduleEntity |> Array.ofSeq

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "sample" nestedEntities
    
    //WebSharper instance: 

    let expected =
        """
    export namespace sample {
        export interface Shapes { }
        export interface Circle {
            Radius: number;
            Numbers: Array<number>;
        }
        export interface Shape {
            Circle: Shapes.Circle;
        }
    }"""

    Assert.Equal(expected.[2..], output)

[<Fact>]
let ``Units of measure are ignored``() =
    //Units of measure aren't implemented in TypeScript
    //https://github.com/Microsoft/TypeScript/issues/364

    let sampleText = """
module Temperature

[<Measure>] type degFahrenheit // temperature, Fahrenheit

type Reading = {
    time: System.DateTime
    value: float<degFahrenheit^2>
}
    """

    let entities = ProjectManager.extractEntitites sampleText

    let moduleEntity = entities.[0]

    Assert.Equal("Temperature", moduleEntity.DisplayName)

    let nestedEntities = Explore.findEntities moduleEntity |> Array.ofSeq

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "sample" nestedEntities
    
    //WebSharper instance: 

    let expected =
        """
    export namespace sample {
        export interface degFahrenheit { }
        export interface Temperature { }
        export interface Reading {
            time: System.DateTime;
            value: number;
        }
    }"""

    Assert.Equal(expected.[2..], output)

[<Fact>]
let ``Tuples on unions``() =

    let sampleText = """
module Space

type Point =
    | OneD of float
    | TwoD of float * float
    | ThreeD of float * float * float
    | Special of float * bool
    """

    let entities = ProjectManager.extractEntitites sampleText

    let moduleEntity = entities.[0]

    Assert.Equal("Space", moduleEntity.DisplayName)

    let nestedEntities = Explore.findEntities moduleEntity |> Array.ofSeq

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "sample" nestedEntities

    let expected =
        """
    export namespace sample {
        export interface Space { }
        export interface Point {
            OneD?: number;
            TwoD?: number[];
            ThreeD?: number[];
            Special?: (number | boolean)[];
        }
    }"""

    Assert.Equal(expected.[2..], output)