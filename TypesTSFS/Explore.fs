module Explore

open Microsoft.FSharp.Compiler.SourceCodeServices

//Cache to prevent rework
let entitiesSeen = System.Collections.Generic.HashSet<FSharpEntity>()

//This isn't intended for abitrary depth so limit to 10 for now to mitigate cycles.
//Changing this from a sequence expression to a fold with work list and cycle detection would be better... TODO
let rec allEntities (depth:int) (topEntity:FSharpEntity) : seq<FSharpEntity> =
        
    if depth = 0 then
        entitiesSeen.Clear()

    if entitiesSeen.Contains topEntity || depth > 10 then
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

                    yield! topEntity.AbbreviatedType.TypeDefinition |> (allEntities (depth + 1))

                    yield!
                        topEntity.AbbreviatedType.GenericArguments
                        |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                        |> Seq.map (fun argument -> argument.TypeDefinition |> allEntities (depth + 1))
                        |> Seq.concat

            else
                    //We only want unabbreviated types, easier to deal with later

                if topEntity.IsFSharpRecord && topEntity.FullName <> "Microsoft.FSharp.Core.FSharpRef`1" then
                    yield! topEntity.FSharpFields
                                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                                            |> Seq.map (fun field -> field.FieldType)
                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                            |> Seq.map (fun type2 -> type2.TypeDefinition |> allEntities (depth + 1))
                                            |> Seq.concat
                                            |> Seq.cache //Debug



                    yield! topEntity.FSharpFields
                                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                                            |> Seq.map (fun field -> field.FieldType)
                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                            |> Seq.map (fun type2 -> type2.GenericArguments
                                                                        |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                        |> Seq.map (fun argument -> argument.TypeDefinition))
                                            |> Seq.map (fun entities -> entities |> Seq.map (allEntities (depth + 1)))
                                            |> Seq.concat
                                            |> Seq.concat
                                            |> Seq.cache //Debug

                    yield! topEntity.FSharpFields
                                            |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                                            |> Seq.map (fun field -> field.FieldType)
                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                            |> Seq.collect (fun type2 -> type2.GenericArguments)
                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                            |> Seq.map (fun argument -> argument.GenericArguments
                                                                            |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                            |> Seq.map (fun argument -> argument.TypeDefinition))
                                            |> Seq.map (fun entities -> entities |> Seq.map (allEntities (depth + 1)))
                                            |> Seq.concat
                                            |> Seq.concat
                                            |> Seq.cache //Debug

                if topEntity.IsFSharpUnion then
                        
                        
                    //printfn "%A" topEntity.FullName

                    yield! topEntity.UnionCases
                            |> Seq.map (fun case -> case.UnionCaseFields
                                                    |> Seq.map (fun field -> field.FieldType)
                                                    |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                    |> Seq.map (fun type2 -> type2.TypeDefinition))
                            |> Seq.map (fun fields -> fields |> Seq.map (allEntities (depth + 1)))
                            |> Seq.concat
                            |> Seq.concat
                            |> Seq.cache //Debug

                yield!
                    topEntity.NestedEntities
                    |> Seq.map (allEntities (depth + 1))
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
                        |> Seq.map (allEntities (depth + 1))
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
                        |> Seq.map (fun fields -> fields |> Seq.map (allEntities (depth + 1)))
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
                                                        |> Seq.map (fun type2 -> type2.GenericArguments
                                                                                    |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                                    |> Seq.map (fun argument -> argument.TypeDefinition)))
                        |> Seq.map (fun fields -> fields |> Seq.map (allEntities (depth + 1)))
                        |> Seq.concat
                        |> Seq.concat
                        |> Seq.cache //Debug

                if topEntity.IsArrayType then
                    () //Generic arguments have been captured above

    }