module Tests

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Empty test``() =

    let sampleText = """
module M

type C() = 
    member x.P = 1

let xxx = 3 + 4
let fff () = xxx + xxx
    """

    let checkResults = ProjectManager.extractEntitites sampleText

    checkResults.AssemblySignature.Entities.Count =! 1

    let entity = checkResults.AssemblySignature.Entities.[0]

    entity.DisplayName =! "M"

    let entities = Explore.findEntities entity

    Seq.length entities =! 4

    ()