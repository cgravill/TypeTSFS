open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance 
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


let input2 = 
      """
[<System.CLSCompliant(true)>]
let foo(x, y) = 
    let msg = String.Concat("Hello"," ","world")
    if true then 
        printfn "x = %d, y = %d" x y 
        printfn "%s" msg

type C() = 
    member x.P = 1

let listO = List<int>()

      """
let parseFileResults, checkFileResults = 
    parseAndTypeCheckSingleFile(file, input2)

[<EntryPoint>]
let main argv =

    let partialAssemblySignature = checkFileResults.PartialAssemblySignature

    printfn "%A" argv
    0 // return an integer exit code
