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

[<EntryPoint>]
let main argv =

    let moduleToString moduleText =
       
        let parseFileResults, checkFileResults = 
            parseAndTypeCheckSingleFile(file, moduleText)

        let moduleEntity = checkFileResults.PartialAssemblySignature.Entities.[0]


        let moduleName = moduleEntity.CompiledName

        let nested = moduleEntity.NestedEntities

        let typeToTS (fsharpType:FSharpType) =
            if fsharpType.IsGenericParameter then
                fsharpType.GenericParameter.DisplayName
            else
                match fsharpType.TypeDefinition.DisplayName with
                | "int" -> "number"
                | "float" -> "number"
                | "bool" -> "boolean"
                | "list" -> "[]"
                | x -> x

        let records = nested |> Seq.filter (fun entity -> entity.IsFSharpRecord)

        let recordFieldsToString record = String.concat "\n" (record |> Seq.map (fun (recordField:FSharpField) -> sprintf "\t\t%s: %s;" recordField.DisplayName (typeToTS recordField.FieldType)))
        let recordsAsStrings = records |> Seq.map (fun record -> sprintf "\texport interface %s {\n%s\n\t}" record.DisplayName (recordFieldsToString record.FSharpFields))

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

        let withModuleWrapping = sprintf "namespace %s {\n%s\n}" moduleName contents
        withModuleWrapping


    for file in System.IO.Directory.EnumerateFiles("""../../../SampleFs""", "*.fs") do
        printfn "//%s" file
        let contents = System.IO.File.ReadAllText file
        try
            let moduleAsString = moduleToString contents
            printfn "%s" moduleAsString
        with
            | _ as ex -> printfn "//**** FAILED"
        
    0
