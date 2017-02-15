module WebSharperTests

open Xunit

[<Fact>]
let ``Discriminated union``() =

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

    let output = EmitTS.entityToString("bob", nestedEntities |> Array.ofSeq)
    
    let expected =
        """
    export namespace bob {
        export interface Shapes { }
        export interface Circle {
            Radius: number;
            Numbers: Array<number>;
        }
        export interface List<T> {
            []?: string;
            ::?: OnlySupportSingleTypeCases;
        }
    }"""

    //TODO: get rid of that List<T>

    Assert.Equal(expected.[2..], output)

    let circleEntity = nestedEntities |> Seq.head

    ()