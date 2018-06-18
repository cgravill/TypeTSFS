module Transformer

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Xml.Linq
open System.IO
open System.Reflection

let checker = FSharpChecker.Create()

let parseAndTypeCheckSingleFile (file, input) = 
    // Get context representing a stand-alone (script) file
    let projOptions, _ = 
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

//The options were previously created via the ProjectCracker, however, this is now not maintained and fails
//https://github.com/fsharp/FSharp.Compiler.Service/issues/624#issuecomment-328497857
//Instead we do by hand for now, consider using https://github.com/daveaglick/Buildalyzer
let projectOptions normalisedProjectPath =

    let text = System.IO.File.ReadAllText normalisedProjectPath

    let directory = System.IO.Path.GetDirectoryName normalisedProjectPath
    
    let exeDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let document = XDocument.Parse text

    let files =
        document.Descendants()
        |> Seq.filter (fun node ->
            node.Name.LocalName = "Compile")
        |> Seq.map (fun compileNode ->
            let includeAttribute = compileNode.Attributes() |> Seq.find (fun (attribute:XAttribute) -> attribute.Name.LocalName = "Include")
            Path.GetFullPath(System.IO.Path.Combine(directory, includeAttribute.Value)))
    
    let projects =
        document.Descendants()
        |> Seq.filter (fun node ->
            node.Name.LocalName = "ProjectReference")
        |> Seq.map (fun compileNode ->
            let includeAttribute = compileNode.Attributes() |> Seq.find (fun (attribute:XAttribute) -> attribute.Name.LocalName = "Include")
            Path.GetFullPath(System.IO.Path.Combine(directory, includeAttribute.Value)))

    let projectsReferences =
        document.Descendants()
        |> Seq.filter (fun node ->
            node.Name.LocalName = "ProjectReference")
        |> Seq.map (fun compileNode ->
            let includeAttribute = compileNode.Attributes() |> Seq.find (fun (attribute:XAttribute) -> attribute.Name.LocalName = "Include")
            let projPath = Path.GetFullPath(System.IO.Path.Combine(directory, includeAttribute.Value))
            let projectName = Path.GetFileNameWithoutExtension projPath
            let projDirectory = Path.GetDirectoryName(projPath)
            projDirectory + "\\bin\\debug\\" + projectName + ".dll"
            )
    
    projectsReferences
    |> Seq.filter( not << System.IO.File.Exists)
    |> Seq.iter (printfn "Missing file: %s")
    
    //http://fsharp.github.io/FSharp.Compiler.Service/project.html#Analyzing-multiple-projects
    //options.ReferencedProjects

    let sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            // Hardcoded framework, should be read instead
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\" + nm + ".dll"
        else
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let (++) a b = System.IO.Path.Combine(a,b)
            sysDir ++ nm + ".dll" 

    let localLib name =
        Path.GetFullPath(Path.Combine(exeDirectory, name) + ".dll")
  
    let references =
                [ sysLib "mscorlib" 
                  sysLib "System"
                  sysLib "System.Core"
                  localLib "FSharp.Core"
                ]
    
    //JavaScript

    let options =
        checker.GetProjectOptionsFromCommandLineArgs(
            normalisedProjectPath,
            [| //yield "--simpleresolution" 
               yield "--noframework" 
               yield "--debug:full" 
               yield "--define:DEBUG" 
               yield "--define:JavaScript" //TODO: get this via reading project 
               yield "--optimize-" 
               yield "--out:" + "XYZ.dll"
               yield "--doc:test.xml" 
               yield "--warn:3" 
               yield "--fullpaths" 
               yield "--flaterrors" 
               yield "--target:library" 
               for project in projectsReferences do
                   yield "-r:" + project
               for file in files do
                   yield file
               for r in references do 
                   yield "-r:" + r

            |])

    options

let fromFSharpToTypeScript style projectFile moduleTargetName outputPath =

    let normalisedProjectPath = System.IO.Path.GetFullPath projectFile

    if not (System.IO.File.Exists normalisedProjectPath) then failwithf "Project not found: %s" normalisedProjectPath

    //let projectOptions = ProjectCracker.GetProjectOptionsFromProjectFile(normalisedProjectPath)

    let projectOptions = projectOptions normalisedProjectPath

    let checkResults = checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    //checkResults.Errors |> Array.iter (printfn "%O")

    let entities = checkResults.AssemblySignature.Entities

    let jsAPI = entities |> Seq.filter (fun entity -> entity.CompiledName = moduleTargetName) |> Seq.exactlyOne

    let allOfThem =
        jsAPI
        |> Explore.findEntitites
        |> Seq.distinct
        |> Seq.sortBy(fun element -> element.DisplayName)
        |> Array.ofSeq

    let fewerOfThem =
        jsAPI
        |> Explore.findEntitites
        |> Seq.distinct
        |> Seq.filter(fun entity -> not entity.IsFSharpModule && not (isHiddenFunction entity)) 
        |> Seq.filter(fun entity -> not (entity.AccessPath.Contains "System.Collections.Generic") && not (entity.AccessPath.Contains "Microsoft.FSharp.Collections"))
        |> Seq.map unabbreviate
        |> Seq.distinct
        |> Seq.sortBy(fun element -> element.DisplayName) //Debugging
        |> Array.ofSeq //While developing convenient to have as an array
        

    //Debugging

    let debugEntities =
        allOfThem
        |> Array.filter (fun element -> element.TryFullName.IsSome && element.TryFullName.Value.Contains  "test_crn_deterministic")
       
    (*let debugEntity = debugEntities |> Array.exactlyOne*)

    (*let fewerOfThem =
        Explore.findEntities debugEntity
        |> Array.ofSeq*)
        
    //Debugging
    

    let groupedByNamespace = fewerOfThem |> Array.groupBy(fun entity -> entity.AccessPath)

    let enityToString = EmitTS.entityToString style

    let namespacesAsStrings = groupedByNamespace |> Array.map (fun (a,b) -> enityToString a b) |> String.concat "\r\n\r\n"


    //TODO: test which of these are needed, also what do they get transformed to in WebSharper?

    //let wasFSharpMapUsed = allOfThem |> Seq.choose (fun element -> element.TryFullName) |> Seq.exists (fun fullName -> fullName.Contains "Microsoft.FSharp.Collections.FSharpMap")

    let opaqueNamespaces =
        "    export namespace Opaque {\r\n" +
        "        export interface FSharpMap<K, V> { }\r\n" +
        "        export interface Dictionary<K, V> { }\r\n" +
        "        export interface FSharpTuple { }\r\n" +
        "    }\r\n" +
        "    export namespace System {\r\n" +
        "        export interface Object { }\r\n" +
        "    }"

    let functionAsStrings = EmitTS.functionAsStrings jsAPI.MembersFunctionsAndValues

    let warning = "//These interfaces are code generated from F#, any changes to this file will be lost."
    let topNamespace = 
        match style with
        | EmitTS.Style.WebSharper -> "export namespace WebSharperGeneratedInterfaces {"
        | EmitTS.Style.JsonNet -> "export namespace JsonNetGeneratedInterfaces {"


    let all = warning + "\r\n" + topNamespace + "\r\n\r\n" + namespacesAsStrings + "\r\n\r\n" + opaqueNamespaces + "\r\n\r\n" + functionAsStrings + "\r\n}"

    System.IO.File.WriteAllText(outputPath,all)


let fromFSharpViaWebSharperToTypeScript = fromFSharpToTypeScript EmitTS.WebSharper
let fromFSharpViaJsonNetToTypeScript = fromFSharpToTypeScript EmitTS.JsonNet