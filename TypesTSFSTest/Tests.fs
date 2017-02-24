module Tests

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Trace some entities``() =

    let sampleText = """
module M

type C() = 
    member x.P = 1

let xxx = 3 + 4
let fff () = xxx + xxx
    """

    let entities = ProjectManager.extractEntitites sampleText

    entities.Count =! 1

    let entity = entities.[0]

    entity.DisplayName =! "M"

    let entities = Explore.findEntities entity

    //Seq.length entities =! 4

    ()