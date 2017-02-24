#r "../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../packages/FSharp.Compiler.Service.ProjectCracker/lib/net45/FSharp.Compiler.Service.ProjectCracker.dll"

#r "bin/TypesTSFS.exe"

//Patch FCS deployment... had an issue if the cracker .exe isn't in the dll's directory, see issue report
//https://github.com/fsharp/FSharp.Compiler.Service/issues/629
let copyit filename =
    System.IO.File.Copy(
        System.IO.Path.GetFullPath( __SOURCE_DIRECTORY__ + @"/../packages/FSharp.Compiler.Service.ProjectCracker/utilities/net45/" + filename),
        System.IO.Path.GetFullPath(__SOURCE_DIRECTORY__ +  @"/bin/" + filename),
        overwrite = true)

copyit "FSharp.Compiler.Service.ProjectCrackerTool.exe"
copyit "FSharp.Compiler.Service.ProjectCrackerTool.exe.config"
copyit "FSharp.Compiler.Service.ProjectCrackerTool.pdb"

let projectFile = __SOURCE_DIRECTORY__ + @"/../SampleWS/SampleWS.fsproj"
let moduleTargetName = "JSAPI"
let outputPath = __SOURCE_DIRECTORY__ + "\GeneratedInterfaces.ts"

Transformer.fromFSharpViaJsonNetToTypeScript projectFile moduleTargetName outputPath