module Explore

open Microsoft.FSharp.Compiler.SourceCodeServices

let findEntities (startEntity:FSharpEntity) : seq<FSharpEntity> =
    
    //Cache to prevent rework
    let entitiesSeen = System.Collections.Generic.HashSet<FSharpEntity>()

    //Changing this from a sequence expression to a fold with work list would make it easier to debug... TODO
    let rec innerFind (topEntity:FSharpEntity) : seq<FSharpEntity> =

        if entitiesSeen.Contains topEntity then
            Seq.empty
        else
            entitiesSeen.Add topEntity |> ignore

            seq {
                //printfn "%A" topEntity.TryFullName

                yield topEntity

                if topEntity.IsFSharpAbbreviation then
                    if topEntity.AbbreviatedType.IsFunctionType then
                        //How to deal with abbreviated functions?
                        printfn "%A" topEntity
                        ()
                    
                    else
                        let unabbreivated = topEntity.AbbreviatedType

                        if unabbreivated.HasTypeDefinition then
                            yield! unabbreivated.TypeDefinition |> innerFind

                        yield!
                            unabbreivated.GenericArguments
                            |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                            |> Seq.map (fun argument -> argument.TypeDefinition |> innerFind)
                            |> Seq.concat

                else

                    if topEntity.IsFSharpRecord && topEntity.FullName <> "Microsoft.FSharp.Core.FSharpRef`1" then
                        yield!
                            topEntity.FSharpFields
                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                            |> Seq.map (fun field -> field.FieldType)
                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                            |> Seq.map (fun type2 -> type2.TypeDefinition)
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug


                        //1st generic
                        yield!
                            topEntity.FSharpFields
                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                            |> Seq.map (fun field -> field.FieldType)
                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                            |> Seq.collect (fun type2 -> type2.GenericArguments
                                                            |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                            |> Seq.map (fun argument -> argument.TypeDefinition))
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug

                        //2nd generic
                        yield!
                            topEntity.FSharpFields
                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                            |> Seq.map (fun field -> field.FieldType)
                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                            |> Seq.collect (fun type2 -> type2.GenericArguments)
                            |> Seq.filter (fun type3 -> type3.HasTypeDefinition)
                            |> Seq.collect (fun argument -> argument.GenericArguments
                                                            |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                            |> Seq.map (fun argument -> argument.TypeDefinition))
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug
                        
                        //TODO: how to find the nth generic?


                    if topEntity.IsFSharpUnion then
                        
                        
                        //printfn "%A" topEntity.FullName

                        yield!
                            topEntity.UnionCases
                            |> Seq.collect (fun case -> case.UnionCaseFields
                                                        |> Seq.map (fun field -> field.FieldType)
                                                        |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                        |> Seq.map (fun type2 -> type2.TypeDefinition))
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug


                        //1st generic
                        yield!
                            topEntity.UnionCases
                            |> Seq.collect (fun case -> case.UnionCaseFields
                                                        |> Seq.map (fun field -> field.FieldType)
                                                        |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                        |> Seq.collect (fun type2 -> type2.GenericArguments
                                                                                     |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                                     |> Seq.map (fun argument -> argument.TypeDefinition)))
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug

                        //TODO: how to find the nth generic?

                    yield!
                        topEntity.NestedEntities
                        |> Seq.map innerFind
                        |> Seq.concat
                        |> Seq.cache //Debug

                    

                    if (topEntity.IsFSharpModule) then
                        yield!
                            topEntity.MembersFunctionsAndValues
                            |> Seq.collect (fun something -> something.CurriedParameterGroups
                                                            |> Seq.concat
                                                            |> Seq.map (fun parameter -> parameter.Type)
                                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                            |> Seq.map (fun type2 -> type2.TypeDefinition))
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug

                        //Return parameter
                        yield!
                            topEntity.MembersFunctionsAndValues
                            |> Seq.map (fun something -> something.ReturnParameter)
                            |> Seq.map (fun parameter -> parameter.Type)
                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                            |> Seq.map (fun type2 -> type2.TypeDefinition)
                            |> Seq.map innerFind
                            |> Seq.concat
                            |> Seq.cache //Debug

                        //First generic
                        yield!
                            topEntity.MembersFunctionsAndValues
                            |> Seq.collect (fun something -> something.CurriedParameterGroups
                                                            |> Seq.concat
                                                            |> Seq.map (fun parameter -> parameter.Type)
                                                            |> Seq.map (fun type2 -> type2.GenericArguments
                                                                                        |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                                        |> Seq.map (fun argument -> argument.TypeDefinition)))
                            |> Seq.map (fun fields -> fields |> Seq.map innerFind)
                            |> Seq.concat
                            |> Seq.concat
                            |> Seq.cache //Debug

                        //second generic
                        yield!
                            topEntity.MembersFunctionsAndValues
                            |> Seq.collect (fun something -> something.CurriedParameterGroups
                                                            |> Seq.concat
                                                            |> Seq.map (fun parameter -> parameter.Type)
                                                            |> Seq.collect (fun type2 -> type2.GenericArguments)
                                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                            |> Seq.map (fun type2 -> type2.GenericArguments
                                                                                        |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                                        |> Seq.map (fun argument -> argument.TypeDefinition)))
                            |> Seq.map (fun fields -> fields |> Seq.map innerFind)
                            |> Seq.concat
                            |> Seq.concat
                            |> Seq.cache //Debug

                    //TODO: handle functions

                    if topEntity.IsArrayType then
                        () //Generic arguments have been captured above

        }

    innerFind startEntity


let findEntitiesIterative (startEntity:FSharpEntity) : seq<FSharpEntity> =
    
    //Cache to prevent rework
    let entitiesSeen = System.Collections.Generic.HashSet<FSharpEntity>()

    let toExplore = System.Collections.Generic.Queue<FSharpEntity>()

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