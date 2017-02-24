module Transformer

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

let unabbreviate (possiblyAbbreviated:FSharpEntity) =
    if possiblyAbbreviated.IsFSharpAbbreviation then
        if possiblyAbbreviated.AbbreviatedType.HasTypeDefinition then
            possiblyAbbreviated.AbbreviatedType.TypeDefinition
        else
            possiblyAbbreviated //e.g. Tuple abbreviated...
    else
        possiblyAbbreviated

let isHiddenFunction (possiblyAbbreviated:FSharpEntity) =
    if possiblyAbbreviated.IsFSharpAbbreviation then
        possiblyAbbreviated.AbbreviatedType.IsFunctionType
    else
        false

let fromFSharpViaWebSharperToTypeScript projectFile moduleTargetName outputPath =

    let normalisedProjectPath = System.IO.Path.GetFullPath projectFile

    if not (System.IO.File.Exists normalisedProjectPath) then failwithf "Project not found: %s" normalisedProjectPath

    let projectOptions = ProjectCracker.GetProjectOptionsFromProjectFile(normalisedProjectPath)

    let checkResults = checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    let entities = checkResults.AssemblySignature.Entities

    let jsAPI = entities |> Seq.filter (fun entity -> entity.CompiledName = moduleTargetName) |> Seq.exactlyOne

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
        

    //Debugging

    (*let debugEntities =
        allOfThem
        |> Array.filter (fun element -> element.TryFullName.IsSome && element.TryFullName.Value.Contains  "bob" )
       
    let debugEntity = debugEntities |> Array.exactlyOne

    let fewerOfThem =
        Explore.findEntities debugEntity
        |> Array.ofSeq*)
        
    //Debugging
    

    let groupedByNamespace = fewerOfThem |> Array.groupBy(fun entity -> entity.AccessPath)

    let enityToString = EmitTS.entityToString EmitTS.Style.WebSharper

    let namespacesAsStrings = groupedByNamespace |> Array.map (fun (a,b) -> enityToString a b) |> String.concat "\r\n\r\n"


    //TODO: test which of these are needed, also what do they get transformed to in WebSharper?

    //let wasFSharpMapUsed = allOfThem |> Seq.choose (fun element -> element.TryFullName) |> Seq.exists (fun fullName -> fullName.Contains "Microsoft.FSharp.Collections.FSharpMap")

    let opaqueNamespaces =
        "    export namespace Opaque {\r\n" +
        "        export interface FSharpMap<K, V> { }\r\n" +
        "        export interface Dictionary<K, V> { }\r\n" +
        "        export interface FSharpTuple { }\r\n" +
        "    }"

    let functionAsStrings = EmitTS.functionAsStrings jsAPI.MembersFunctionsAndValues

    let warning = "//These interfaces are code generated from F#, any changes to this file will be lost."
    let topNamespace = "export namespace ExperimentalGeneratedInterfaces {"

    let all = warning + "\r\n" + topNamespace + "\r\n\r\n" + namespacesAsStrings + "\r\n\r\n" + opaqueNamespaces + "\r\n\r\n" + functionAsStrings + "\r\n}"

    System.IO.File.WriteAllText(outputPath,all)