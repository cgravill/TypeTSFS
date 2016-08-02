open Microsoft.FSharp.Compiler.SourceCodeServices



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

    let projectFile = @"../../../SampleWS/SampleWS.fsproj"

    let x = ProjectCracker.GetProjectOptionsFromProjectFile(projectFile)

    let y = checker.ParseAndCheckProject x |> Async.RunSynchronously

    let entities = y.AssemblySignature.Entities


    let jsAPI = entities |> Seq.filter (fun entity -> entity.CompiledName = "JSAPI") |> Seq.exactlyOne




    let namespaceContents = System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()

    let unabbreviate (possiblyAbbreviated:FSharpEntity) =
        if possiblyAbbreviated.IsFSharpAbbreviation then
            possiblyAbbreviated.AbbreviatedType.TypeDefinition
        else
            possiblyAbbreviated

    


    
    let isHiddenFunction (possiblyAbbreviated:FSharpEntity) =
        if possiblyAbbreviated.IsFSharpAbbreviation then
            possiblyAbbreviated.AbbreviatedType.IsFunctionType
        else
            false

    let allOfThem =
        jsAPI
        |> Explore.findEntities
        |> Seq.distinct
        |> Seq.sortBy(fun element -> element.DisplayName)
        |> Array.ofSeq

    let fewerOfThem =
        jsAPI
        |> Explore.findEntities
        |> Seq.distinct
        |> Seq.filter(fun entity -> not entity.IsFSharpModule && not (isHiddenFunction entity)) 
        |> Seq.filter(fun entity -> not (entity.AccessPath.Contains "System.Collections.Generic") && not (entity.AccessPath.Contains "Microsoft.FSharp.Collections"))
        |> Seq.map unabbreviate
        |> Seq.distinct
        |> Seq.sortBy(fun element -> element.DisplayName) //Debugging
        |> Array.ofSeq //While developing convenient to have as an array
        

    

    let groupedByNamespace = fewerOfThem |> Array.groupBy(fun entity -> entity.AccessPath)

    //Debugging

    (*let debugEntities =
        allOfThem
        |> Array.filter (fun element -> element.TryFullName.IsSome && element.TryFullName.Value.Contains  "bob" )
       
    let debugEntity = debugEntities |> Array.exactlyOne

    let trial =
        allEntities 5 debugEntity
        |> Array.ofSeq*)

    let namespacesAsStrings = groupedByNamespace |> Array.map EmitTS.entityToString |> String.concat "\n\n"


    //let wasFSharpMapUsed = allOfThem |> Seq.choose (fun element -> element.TryFullName) |> Seq.exists (fun fullName -> fullName.Contains "Microsoft.FSharp.Collections.FSharpMap")

    let opaqueNamespaces =
        "\texport namespace Opaque {
\t\texport interface FSharpMap<K,V> {}
\t\texport interface Dictionary<K,V> {}
}"

    let functionAsStrings = EmitTS.functionAsStrings jsAPI.MembersFunctionsAndValues

    let warning = "//These interfaces are code generated from F#, any changes to this file will be lost."
    let topNamespace = "export namespace ExperimentalGeneratedInterfaces {"

    let all = warning + "\n" + topNamespace + "\n\n" + namespacesAsStrings + "\n\n" + opaqueNamespaces + "\n\n" + functionAsStrings + "\n}"

    System.IO.File.WriteAllText("output.ts",all)

    0
