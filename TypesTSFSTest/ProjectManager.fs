module ProjectManager

open System
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices
open System.IO
open System.Reflection

let extractEntitites sourceText =

    let checker = FSharpChecker.Create()

    let base1 = Path.GetTempFileName()
    let fileName1 = Path.ChangeExtension(base1, ".fs")
    let base2 = Path.GetTempFileName()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    File.WriteAllText(fileName1, sourceText)

    let directory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let projectOptions =

        //TODO: all these references should be determined from the project under test, not this

        let sysLib nm = 
            if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
                // file references only valid on Windows
                let settings = NuGet.Configuration.Settings.LoadDefaultSettings(null)
                let nugetPath = NuGet.Configuration.SettingsUtility.GetGlobalPackagesFolder(settings)

                let fileName = nm + ".dll"

                let referencePath = Path.Combine(nugetPath, """netstandard.library\2.0.3\build\netstandard2.0\ref\""")

                if not (Directory.Exists referencePath) then
                    failwithf "Cannot find '%s' ...has the version been upgraded?" referencePath

                Path.Combine(referencePath, fileName)
            else
                printfn "fall back for files, not well tested"
                
                let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
                let (++) a b = System.IO.Path.Combine(a,b)
                sysDir ++ nm + ".dll" 
        
        
        let localLib name =
            Path.Combine(directory, name) + ".dll"

        let references =
            [
                sysLib "mscorlib"
                sysLib "netstandard" 
                sysLib "System"
                sysLib "System.Core"
                sysLib "System.Runtime"
                localLib "FSharp.Core"
            ]

        checker.GetProjectOptionsFromCommandLineArgs
           (projFileName,
            [| yield "--simpleresolution" 
               yield "--noframework" 
               yield "--debug:full" 
               yield "--define:DEBUG" 
               yield "--optimize-" 
               yield "--out:" + dllName
               yield "--doc:test.xml" 
               yield "--warn:3" 
               yield "--fullpaths" 
               yield "--flaterrors" 
               yield "--target:library" 
               yield fileName1

               for r in references do 
                     yield "-r:" + r |])

    let wholeProjectResults = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

    wholeProjectResults.AssemblySignature.Entities