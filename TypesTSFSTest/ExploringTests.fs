module ExploringTests

open Xunit
open Swensen.Unquote
open Microsoft.FSharp.Compiler.SourceCodeServices

let find entities named = entities |> Seq.tryFind (fun (entity:FSharpEntity) -> entity.DisplayName = named) |> Option.isSome
let has entities named =
    let res = find entities named
    Assert.True(res, sprintf "Should contain: %s" named)
let misses entities named =
    let res = find entities named
    Assert.False(res, sprintf "Should not contain: %s" named)

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

    let entities = Explore.findEntitites entity

    //Seq.length entities =! 4

    ()

[<Fact>]
let ``Deep generics (lists)``() =

    let sampleText = """
namespace Incept

module Target =
    type A = { x: int}
    type B = { x: int}
    type C = { x: int}
    type NotUsed = { x: int}

module Inception =
    type Deep1 = { d1: Target.A list; d2: Target.B list list; d3: Target.C list list list }
    """

    let entities = ProjectManager.extractEntitites sampleText
    let nestedEntities = Explore.findEntitites entities.[1] |> Seq.cache

    nestedEntities |> has <| "A"
    nestedEntities |> has <| "B"
    nestedEntities |> has <| "C"
    nestedEntities |> misses <| "NotUsed"

[<Fact>]
let ``Higher order functions``() =

    let sampleText = """
namespace Incept

module Target =
    type A = { x: int}
    type NotUsed = { x: int}

module Inception =
    let calculator (progress:Target.A -> unit) = 7
    """

    let entities = ProjectManager.extractEntitites sampleText
    let nestedEntities = Explore.findEntitites entities.[1] |> Seq.cache

    nestedEntities |> has <| "A"
    nestedEntities |> misses <| "NotUsed"

[<Fact>]
let ``Trace through abbreviations``() =

    let sampleText = """
namespace Incept

module Target =
    type A = { x: int}
    type As = A list
    type NotUsed = { x: int}

module Inception =
    type Deep1 = { m1: Target.As }
    """

    let entities = ProjectManager.extractEntitites sampleText
    let nestedEntities = Explore.findEntitites entities.[1] |> Seq.cache

    nestedEntities |> has <| "A"
    nestedEntities |> misses <| "NotUsed"

[<Fact>]
let ``Trace through constructors/members``() =

    let sampleText = """
namespace Incept

module Target =
    type A = { x: int}
    type NotUsed = { x: int}

module Inception =
    type Example(a:Target.A) = class end
    """

    let entities = ProjectManager.extractEntitites sampleText
    let nestedEntities = entities.[1] |> Explore.findEntitites |> Seq.cache

    nestedEntities |> has <| "A"
    nestedEntities |> misses <| "NotUsed"