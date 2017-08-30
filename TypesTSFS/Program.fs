open Argu

let moduleNameDefault = "JSAPI"

type CLIArguments =
    | [<Mandatory>] Style of EmitTS.Style
    | [<Mandatory>] ProjectFilePath of path:string
    | ModuleTargetName of moduleName:string
    | [<Mandatory>] OutputPath of path:string

with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Style _ -> "style of output, affects TypeScript"
            | ProjectFilePath _ -> "full path to .fsproj"
            | ModuleTargetName _ -> sprintf "F# module name, defaults to: %s" moduleNameDefault
            | OutputPath _ -> "full path, including .ts to output"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(errorHandler=Argu.ProcessExiter())

    let arguments = parser.ParseCommandLine(inputs=argv)

    let style = arguments.GetResult(<@ Style @>)
    let projectFilePath = arguments.GetResult(<@ ProjectFilePath @>)
    let moduleTargetName = arguments.GetResult(<@ ModuleTargetName @>, defaultValue = moduleNameDefault)
    let outputPath = arguments.GetResult(<@ OutputPath @>)

    Transformer.fromFSharpToTypeScript style projectFilePath moduleTargetName outputPath

    0
