open Microsoft.FSharp.Compiler.SourceCodeServices

type IList<'a> = System.Collections.Generic.IList<'a>

let checker = FSharpChecker.Create()

let parseAndTypeCheckSingleFile (file, input) = 
    // Get context representing a stand-alone (script) file
    let projOptions = 
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously

    let parseFileResults, checkFileResults = 
        checker.ParseAndCheckFileInProject(file, 0, input, projOptions) 
        |> Async.RunSynchronously

    // Wait until type checking succeeds (or 100 attempts)
    match checkFileResults with
    | FSharpCheckFileAnswer.Succeeded(res) -> parseFileResults, res
    | res -> failwithf "Parsing did not finish... (%A)" res

let file = "/home/user/Test.fsx"

let namedOrNumber name (i:int) =
    match name with
    | "" -> ("p" + i.ToString())
    | _ -> name

let rec typeToTS (fsharpType:FSharpType) =

    if fsharpType.IsAbbreviation then
        typeToTS fsharpType.AbbreviatedType
    else
        if fsharpType.IsGenericParameter then
            fsharpType.GenericParameter.DisplayName
        elif fsharpType.IsFunctionType then

            let inputs = fsharpType.GenericArguments |> Seq.take (fsharpType.GenericArguments.Count - 1)
            let output = fsharpType.GenericArguments |> Seq.skip (fsharpType.GenericArguments.Count - 1) |> Seq.exactlyOne

            let argumentsAsString = inputs |> Seq.mapi (fun i argument -> (namedOrNumber "" i) + ":" + typeToTS argument) |> String.concat ", "
            let stringed = sprintf "(%s) => %s" argumentsAsString (typeToTS output)
            //"(p0:SOMETHING) => SOMETHING"
            stringed
        elif fsharpType.IsTupleType then
            "(SOMETHING,SOMETHING)"
        else
            let stem =
                match fsharpType.TypeDefinition.DisplayName with
                | "int" //caution with ints and JavaScript number
                | "Int32"  
                | "uint32"
                | "UInt32"
                | "float"
                | "Double" -> "number"
                | "bool"
                | "Boolean" -> "boolean"
                | "list" 
                | "List" 
                | "[]" -> "Array"
                | "string"
                | "String" -> "string"
                | "Unit" -> "void"
                | x -> fsharpType.TypeDefinition.AccessPath + "." + x

            if fsharpType.GenericArguments.Count > 0 then
                stem + "<" + (fsharpType.GenericArguments |> Seq.map typeToTS |> String.concat ",") + ">"
            else
                stem
        

[<EntryPoint>]
let main argv =

    let entityToString (namespacename:string, nested:FSharpEntity[]) =
       
        (*let parseFileResults, checkFileResults = 
            parseAndTypeCheckSingleFile(file, moduleText)

        let moduleEntity = checkFileResults.PartialAssemblySignature.Entities.[0]*)


        //let moduleName = topEntity.CompiledName

        //let nested = topEntity.NestedEntities

        

        let records = nested |> Seq.filter (fun entity -> entity.IsFSharpRecord)

        let recordFieldsToString record = String.concat "\n" (record |> Seq.map (fun (recordField:FSharpField) -> sprintf "\t\t%s: %s;" recordField.DisplayName (typeToTS recordField.FieldType)))
        let recordTypeGenericParameters (entity:FSharpEntity) =
            
            if entity.GenericParameters.Count = 0 then
                ""
            else
                "<" + (entity.GenericParameters |> Seq.map (fun param -> param.Name) |> String.concat ",") + ">"
                
        let recordsAsStrings =
            records
            |> Seq.map (fun record -> sprintf "\texport interface %s%s {\n%s\n\t}" record.DisplayName (recordTypeGenericParameters record) (recordFieldsToString record.FSharpFields))

        let unions = nested |> Seq.filter (fun entity -> entity.IsFSharpUnion)

        let (|JustName|Types|) (case:FSharpUnionCase) =
            if (case.UnionCaseFields.Count > 0) then
                Types case.UnionCaseFields.[0].FieldType
            else
                JustName case.DisplayName

        let (|AllJustNames|Mixture|) (cases:IList<FSharpUnionCase>) =
            if cases |> Seq.exists (fun case -> case.UnionCaseFields.Count > 0) then
                Mixture cases
            else
                AllJustNames cases

        let caseAsString (case:FSharpUnionCase) =
            match case with
            | JustName name -> sprintf "\t\t%s?: string;" name
            | Types firstType -> sprintf "\t\t%s?: %s;" case.DisplayName (typeToTS firstType)
        
        let genericParamtersToDisplay (genericParameters:System.Collections.Generic.IList<FSharpGenericParameter>) =
            genericParameters
            |> Seq.map(fun genericParameter -> genericParameter.DisplayName)

        let casesAsString (cases:IList<FSharpUnionCase>) = cases |> Seq.map caseAsString |> String.concat "\n"
        let simpleCasesAsString (cases:IList<FSharpUnionCase>) = cases |> Seq.map (fun case -> "\"" + case.DisplayName + "\"") |> String.concat " | "
        let unionNameToString (union:FSharpEntity) =
            if union.GenericParameters.Count = 0 then
                union.DisplayName
            else
                union.DisplayName + "<" + (String.concat "," (genericParamtersToDisplay union.GenericParameters)) + ">"

        let unionsAsString = unions |> Seq.map (fun union -> match union.UnionCases with
                                                                | Mixture cases -> sprintf "\texport interface %s {\n%s\n\t}" (unionNameToString union) (casesAsString cases)
                                                                | AllJustNames cases -> sprintf "\texport type %s = %s" union.DisplayName (simpleCasesAsString cases))

    

        let allAsStrings = Seq.append recordsAsStrings unionsAsString

        let contents = String.concat "\n" allAsStrings

        let withModuleWrapping = sprintf "namespace %s {\n%s\n}" namespacename contents
        withModuleWrapping


    let projectFile = @"../../../SampleWS/SampleWS.fsproj"

    let x = ProjectCracker.GetProjectOptionsFromProjectFile(projectFile)

    let y = checker.ParseAndCheckProject x |> Async.RunSynchronously

    let entities = y.AssemblySignature.Entities


    let jsAPI = entities |> Seq.filter (fun entity -> entity.CompiledName = "JSAPI") |> Seq.exactlyOne


    let argumentsToString (arguments:IList<IList<FSharpParameter>>) =



        arguments
        |> Seq.mapi(fun i parameterGroup -> (namedOrNumber parameterGroup.[0].DisplayName i) + ":" + (typeToTS parameterGroup.[0].Type))
        |> String.concat ", "


    let entitiesToExport = System.Collections.Generic.HashSet<int>()
    let entitiesExported = System.Collections.Generic.HashSet<int>()

    let namespaceContents = System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()

    //let typesAsString = entityToString jsAPI

    let unabbreviate (possiblyAbbreviated:FSharpEntity) =
        if possiblyAbbreviated.IsFSharpAbbreviation then
            possiblyAbbreviated.AbbreviatedType.TypeDefinition
        else
            possiblyAbbreviated

    let mutable all_yielded = Set.empty

    //This isn't intended for abitrary depth... for now
    //Changing this from a sequence expression to a fold is probably sensible...
    let rec allEntities (depth:int) (topEntity:FSharpEntity) : seq<FSharpEntity> =

        if depth > 10 then
            Seq.empty
        else
            seq {
                printfn "%A" topEntity.TryFullName

                if topEntity.IsFSharpAbbreviation then
                    if topEntity.AbbreviatedType.IsFunctionType then
                        //How to deal with abbreviated functions?
                        yield topEntity
                    
                    else
                        yield! topEntity.AbbreviatedType.TypeDefinition |> (allEntities (depth + 1))
                else
                    yield topEntity //We only want unabbreviated types, easier to deal with later

                    if topEntity.IsFSharpRecord && topEntity.FullName <> "Microsoft.FSharp.Core.FSharpRef`1" then
                        yield! topEntity.FSharpFields
                                                |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                                                |> Seq.map (fun field -> field.FieldType)
                                                |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                |> Seq.map (fun type2 -> type2.TypeDefinition |> allEntities (depth + 1))
                                                |> Seq.concat



                        yield! topEntity.FSharpFields
                                                |> Seq.filter (fun field -> not field.FieldType.IsFunctionType)
                                                |> Seq.map (fun field -> field.FieldType)
                                                |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                |> Seq.map (fun type2 -> type2.GenericArguments
                                                                         |> Seq.filter (fun argument -> argument.HasTypeDefinition)
                                                                         |> Seq.map (fun argument -> argument.TypeDefinition))
                                                |> Seq.map (fun fields -> fields |> Seq.map (allEntities (depth + 1)))
                                                |> Seq.concat
                                                |> Seq.concat

                    if topEntity.IsFSharpUnion && topEntity.FullName <> "Microsoft.FSharp.Collections.FSharpList`1" && topEntity.FullName <> "Microsoft.FSharp.Core.FSharpOption`1" then
                        //Also need to do generic arguments, can we share with the record conditional?
                        
                        yield! topEntity.UnionCases
                                    |> Seq.map (fun case -> case.UnionCaseFields
                                                            |> Seq.map (fun field -> field.FieldType)
                                                            |> Seq.filter (fun type2 -> type2.HasTypeDefinition)
                                                            |> Seq.map (fun type2 -> type2.TypeDefinition))
                                    |> Seq.map (fun fields -> fields |> Seq.map (allEntities (depth + 1)))
                                    |> Seq.concat
                                    |> Seq.concat

                    if topEntity.IsArrayType then
                        printfn "Got array: %A" topEntity
                    //Handle arrays

                    //Handle generic arguments, maybe something like.... topEntity.AbbreviatedType.GenericArguments.Count 

                    //Handle functions

                    if topEntity.GenericParameters.Count > 0 then
                        () //I think generic parameters can be ignored

                    yield! topEntity.NestedEntities |> Seq.map (allEntities 0) |> Seq.concat
        }


    
    let isHiddenFunction (possiblyAbbreviated:FSharpEntity) =
        if possiblyAbbreviated.IsFSharpAbbreviation then
            possiblyAbbreviated.AbbreviatedType.IsFunctionType
        else
            false

    let allOfThem =
        jsAPI
        |> (allEntities 0)
        |> Array.ofSeq

    let lessOfThem =
        jsAPI
        |> (allEntities 0)
        |> Seq.filter(fun entity -> not entity.IsFSharpModule && not (isHiddenFunction entity)) 
        |> Seq.map unabbreviate
        |> Seq.filter(fun entity -> entity.IsFSharpRecord || entity.IsFSharpUnion)
        |> Seq.distinct
        |> Array.ofSeq //While developing convenient to have as an array
        |> Array.groupBy(fun entity -> entity.AccessPath)

    let namespacesAsStrings = lessOfThem |> Array.map entityToString |> String.concat "\n"

    let functionAsStrings =
        jsAPI.MembersFunctionsAndValues
        |> Seq.map(fun value -> sprintf "interface %s {\n\t(%s):boolean\n}\n" value.CompiledName (argumentsToString value.CurriedParameterGroups))
        |> String.concat "\n"

    let all = namespacesAsStrings + "\n\n" + functionAsStrings

    System.IO.File.WriteAllText("output.ts",all)

    0
