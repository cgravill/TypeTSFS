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

    let nestedEntities = Explore.findEntities moduleEntity

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "bob" (nestedEntities |> Array.ofSeq)
    
    let expected =
        """
    export namespace bob {
        export interface Shapes { }
        export interface Circle {
            Radius: number;
            Numbers: Array<number>;
        }
    }"""

    Assert.Equal(expected.[2..], output)

    ()

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

    let nestedEntities = Explore.findEntities moduleEntity

    let output = EmitTS.entityToString EmitTS.Style.WebSharper "bob" (nestedEntities |> Array.ofSeq)
    
    //WebSharper instance: 

    let expected =
        """
    export namespace bob {
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

    let circleEntity = nestedEntities |> Seq.head

    ()