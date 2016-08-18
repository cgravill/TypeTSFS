module EmitTS

open Microsoft.FSharp.Compiler.SourceCodeServices
type IList<'a> = System.Collections.Generic.IList<'a>

let namedOrNumber name i =
    match name with
    | "" -> ("p" + string i)
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
            stringed
        elif fsharpType.IsTupleType then
            "Opaque.FSharpTuple" //Not currently supported
        else

            if
                (fsharpType.TypeDefinition.DisplayName = "Dictionary" ||
                 fsharpType.TypeDefinition.DisplayName = "Map") &&
                fsharpType.GenericArguments.[0].HasTypeDefinition &&
                fsharpType.GenericArguments.[0].TypeDefinition.DisplayName = "string" then
                
                sprintf "{ [key: string]: %s }" (typeToTS fsharpType.GenericArguments.[1])
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
                    | "set"
                    | "Set"
                    | "IEnumerable" //TODO: check this really goes to array
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

let parameterAndTypeToTS parameterName (fsharpType:FSharpType) =

    //We have to check it's not some of these or will throw on accessing TypeDefinition
    //TODO: handle abbreviations
    let isOption =
        fsharpType.IsAbbreviation &&
        not fsharpType.IsGenericParameter &&
        not fsharpType.IsFunctionType &&
        not fsharpType.IsTupleType &&
        fsharpType.TypeDefinition.DisplayName = "option" //Bit much to assume this is the right "option"

    if isOption then
        sprintf "%s?: %s" parameterName (typeToTS fsharpType.GenericArguments.[0])
    else
        sprintf "%s: %s" parameterName (typeToTS fsharpType)

let genericParamtersToDisplay (genericParameters:System.Collections.Generic.IList<FSharpGenericParameter>) =
    genericParameters
    |> Seq.map(fun genericParameter -> genericParameter.DisplayName)


let inline displayName (x:^t) =
    (^t: (static member DisplayName: ^t -> string) (x))


let nameAndParametersToString name parameters =
    if Seq.isEmpty parameters then
        name
    else
        name + "<" + (String.concat "," (genericParamtersToDisplay parameters)) + ">"

let entityNameToString (entity:FSharpEntity) = nameAndParametersToString entity.DisplayName entity.GenericParameters
let memberNameToString (entity:FSharpMemberOrFunctionOrValue) = nameAndParametersToString entity.DisplayName entity.GenericParameters

let entityToString (namespacename:string, nested:FSharpEntity[]) =
      
    //Records

    let records = nested |> Seq.filter (fun entity -> entity.IsFSharpRecord)

    let recordFieldsToString record = String.concat "\n" (record |> Seq.map (fun (recordField:FSharpField) -> sprintf "\t\t\t%s;" (parameterAndTypeToTS recordField.DisplayName recordField.FieldType)))
    let recordTypeGenericParameters (entity:FSharpEntity) =
            
        if entity.GenericParameters.Count = 0 then
            ""
        else
            "<" + (entity.GenericParameters |> Seq.map (fun param -> param.Name) |> String.concat ",") + ">"
                
    let recordsAsStrings =
        records
        |> Seq.map (fun record -> sprintf "\t\texport interface %s%s {\n%s\n\t\t}" record.DisplayName (recordTypeGenericParameters record) (recordFieldsToString record.FSharpFields))


    //Unions

    let unions = nested |> Seq.filter (fun entity -> entity.IsFSharpUnion)

    let isNumber (type_:FSharpType) =

        let reallyType = if type_.IsAbbreviation then type_.AbbreviatedType else type_

        reallyType.HasTypeDefinition &&
        not reallyType.IsAbbreviation &&
        not reallyType.IsGenericParameter &&
        not reallyType.IsFunctionType &&
        not reallyType.IsTupleType &&
        (reallyType.TypeDefinition.DisplayName = "Double" || //Bit much to assume this is the right "option"
         reallyType.TypeDefinition.DisplayName = "int32")

    let (|JustName|Type|Types|) (case:FSharpUnionCase) =

        match case.UnionCaseFields.Count with
        | 0 -> JustName case.DisplayName
        | 1 -> Type case.UnionCaseFields.[0].FieldType
        | _ -> Types case.UnionCaseFields.[0]

    let (|AllJustNames|NamesAndNumbers|ComplexTypes|) (cases:IList<FSharpUnionCase>) =
        if cases |> Seq.exists (fun case -> case.UnionCaseFields.Count > 0) then
            if cases |> Seq.collect (fun case -> case.UnionCaseFields) |> Seq.exists (fun field -> not (isNumber field.FieldType)) then
                ComplexTypes cases
            else
                NamesAndNumbers cases
        else
            AllJustNames cases

    let caseAsString (all_cases) (case:FSharpUnionCase) =
        let optional = if (Seq.length all_cases) = 1 then "" else "?" 
        match case with
        | JustName name -> sprintf "\t\t\t%s%s: string;" name optional
        | Type singleType -> sprintf "\t\t\t%s%s: %s;" case.DisplayName optional (typeToTS singleType)
        | Types types -> sprintf "\t\t\t%s%s: %s;" case.DisplayName optional "OnlySupportSingleTypeCases"

    let nameOrNumberToString (case:FSharpUnionCase) =
        if case.UnionCaseFields.Count = 1 && isNumber case.UnionCaseFields.[0].FieldType then
            sprintf "{ %s: number }" case.DisplayName
        else
            "\"" + case.DisplayName + "\""

    let casesAsString (cases:IList<FSharpUnionCase>) = cases |> Seq.map (cases |> caseAsString) |> String.concat "\n"
    
    let namesAndNumbersCasesAsString = Seq.map nameOrNumberToString >> String.concat " | "

    let simpleCasesAsString = Seq.map nameOrNumberToString >> String.concat " | "


    let unionsAsString = unions |> Seq.map (fun union -> match union.UnionCases with
                                                            | ComplexTypes cases -> sprintf "\t\texport interface %s {\n%s\n\t\t}" (entityNameToString union) (casesAsString cases)
                                                            | NamesAndNumbers cases -> sprintf "\t\texport type %s = %s" union.DisplayName (namesAndNumbersCasesAsString cases)
                                                            | AllJustNames cases -> sprintf "\t\texport type %s = %s" union.DisplayName (simpleCasesAsString cases))

    //Classes

    let classes = nested |> Seq.filter (fun entity -> entity.IsFSharp && (entity.IsClass || entity.IsInterface))

    let classesAsString = classes |> Seq.map (fun class_ -> sprintf "\t\texport interface %s {}" (entityNameToString class_))


    //Concat them

    let allAsStrings = [classesAsString; recordsAsStrings; unionsAsString] |> Seq.concat

    let contents = String.concat "\n" allAsStrings

    let withModuleWrapping = sprintf "\texport namespace %s {\n%s\n\t}" namespacename contents
    withModuleWrapping

let argumentsToString (arguments:IList<IList<FSharpParameter>>) =
    arguments
    |> Seq.mapi(fun i parameterGroup -> (namedOrNumber parameterGroup.[0].DisplayName i) + ":" + (typeToTS parameterGroup.[0].Type))
    |> String.concat ", "

let functionAsStrings (functions:seq<FSharpMemberOrFunctionOrValue>) =
    functions
    |> Seq.map(fun value -> sprintf "\texport interface %s {\n\t\t(%s):%s\n\t}\n" (memberNameToString value) (argumentsToString value.CurriedParameterGroups) (typeToTS value.ReturnParameter.Type))
    |> String.concat "\n"