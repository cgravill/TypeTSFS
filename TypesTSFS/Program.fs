[<EntryPoint>]
let main argv =

    let projectFile = @"../../../SampleWS/SampleWS.fsproj"
    let moduleTargetName = "JSAPI"
    let outputPath = "output.ts"

    Transformer.fromFSharpViaWebSharperToTypeScript projectFile moduleTargetName outputPath

    0
