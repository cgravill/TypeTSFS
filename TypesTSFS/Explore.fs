module Explore

open Microsoft.FSharp.Compiler.SourceCodeServices


let rec unabbreviateType (possiblyAbbreviated:FSharpType) =
    if possiblyAbbreviated.IsAbbreviation then
        unabbreviateType possiblyAbbreviated.AbbreviatedType
    else
        possiblyAbbreviated

let rec unabbreviateEntity (possiblyAbbreviated:FSharpEntity) =
    if possiblyAbbreviated.IsFSharpAbbreviation then
        if possiblyAbbreviated.AbbreviatedType.HasTypeDefinition then
            unabbreviateEntity possiblyAbbreviated.AbbreviatedType.TypeDefinition
        else
            None
    else
        Some possiblyAbbreviated

let findEntitiesIterative (startEntity:FSharpEntity) : seq<FSharpEntity> =
    let entitiesSeen = System.Collections.Generic.HashSet<FSharpEntity>() //prevents rework and is the returned value

    let toExplore = System.Collections.Generic.Queue<FSharpEntity>() //work list

    let add possibleEntity =
        match unabbreviateEntity possibleEntity with
        | Some realEntity when not (entitiesSeen.Contains realEntity) -> 
            entitiesSeen.Add realEntity |> ignore
            toExplore.Enqueue realEntity
        | _ -> ()

    let rec recurisivelyAdd (fsharpType:FSharpType) =

        let actualType = unabbreviateType fsharpType

        if actualType.HasTypeDefinition then
            add actualType.TypeDefinition

        if actualType.HasTypeDefinition || actualType.IsFunctionType then
            actualType.GenericArguments
            |> Seq.iter recurisivelyAdd

    //Seed value for work
    add startEntity

    while toExplore.Count > 0 do

        let head = toExplore.Dequeue()

        head.NestedEntities |> Seq.iter add

        if head.IsFSharpAbbreviation then
            if head.AbbreviatedType.IsFunctionType then
                head.AbbreviatedType.GenericArguments |> Seq.iter recurisivelyAdd

            recurisivelyAdd head.AbbreviatedType
        elif head.IsFSharpRecord then
            head.FSharpFields
            |> Seq.map (fun field -> field.FieldType)
            |> Seq.iter recurisivelyAdd
        elif head.IsFSharpUnion then
            head.UnionCases
            |> Seq.collect (fun unionCase -> unionCase.UnionCaseFields)
            |> Seq.map (fun field -> field.FieldType)
            |> Seq.iter recurisivelyAdd
        elif head.IsFSharpModule then
            head.MembersFunctionsAndValues
            |> Seq.collect (fun module_ -> module_.CurriedParameterGroups)
            |> Seq.collect id
            |> Seq.map (fun parameter -> parameter.Type)
            |> Seq.iter recurisivelyAdd

            head.MembersFunctionsAndValues
            |> Seq.map (fun module_ -> module_.ReturnParameter)
            |> Seq.map (fun parameter -> parameter.Type)
            |> Seq.iter recurisivelyAdd
        elif head.IsFSharpAbbreviation then
            ()

    entitiesSeen :> seq<FSharpEntity>