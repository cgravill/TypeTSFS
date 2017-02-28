module JsonNetTests

open Xunit

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

    let output = EmitTS.entityToString EmitTS.Style.JsonNet "bob" (nestedEntities |> Array.ofSeq)
    
    //Json.Net instance: {"Case":"Circle","Fields":[{"Radius":4,"Numbers":[1,4,3]}]}

    let jsonNetExpected =
        """
    export namespace bob {
        export interface Shapes { }
        export interface Circle {
            Radius: number;
            Numbers: Array<number>;
        }
        export interface Shape {
            Case: "Circle";
            Fields: Array<Shapes.Circle>;
        }
    }"""

    Assert.Equal(jsonNetExpected.[2..], output)

    ()