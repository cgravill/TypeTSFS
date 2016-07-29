module EmitTS

open Microsoft.FSharp.Compiler.SourceCodeServices
type IList<'a> = System.Collections.Generic.IList<'a>

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
                | "Dictionary" -> "Opaque.Dictionary"
                | "Map" -> "Opaque.FSharpMap"
                | x -> fsharpType.TypeDefinition.AccessPath + "." + x

            if fsharpType.GenericArguments.Count > 0 then
                stem + "<" + (fsharpType.GenericArguments |> Seq.map typeToTS |> String.concat ",") + ">"
            else
                stem

let entityToString (namespacename:string, nested:FSharpEntity[]) =
       
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
    let entityNameToString (entity:FSharpEntity) =
        if entity.GenericParameters.Count = 0 then
            entity.DisplayName
        else
            entity.DisplayName + "<" + (String.concat "," (genericParamtersToDisplay entity.GenericParameters)) + ">"

    let unionsAsString = unions |> Seq.map (fun union -> match union.UnionCases with
                                                            | Mixture cases -> sprintf "\texport interface %s {\n%s\n\t}" (entityNameToString union) (casesAsString cases)
                                                            | AllJustNames cases -> sprintf "\texport type %s = %s" union.DisplayName (simpleCasesAsString cases))

        
    let classes = nested |> Seq.filter (fun entity -> entity.IsFSharp && (entity.IsClass || entity.IsInterface))

    let classesAsString = classes |> Seq.map (fun class_ -> sprintf "\texport interface %s {}" (entityNameToString class_))

    let allAsStrings = [classesAsString; recordsAsStrings; unionsAsString] |> Seq.concat

    let contents = String.concat "\n" allAsStrings

    let withModuleWrapping = sprintf "namespace %s {\n%s\n}" namespacename contents
    withModuleWrapping

let argumentsToString (arguments:IList<IList<FSharpParameter>>) =
    arguments
    |> Seq.mapi(fun i parameterGroup -> (namedOrNumber parameterGroup.[0].DisplayName i) + ":" + (typeToTS parameterGroup.[0].Type))
    |> String.concat ", "

let functionAsStrings (functions:seq<FSharpMemberOrFunctionOrValue>) =
    functions
    |> Seq.map(fun value -> sprintf "interface %s {\n\t(%s):boolean\n}\n" value.CompiledName (argumentsToString value.CurriedParameterGroups))
    |> String.concat "\n"