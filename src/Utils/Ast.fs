(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

module Utils.Ast

open Types
open Utils
open FsToolkit.ErrorHandling
open Types.Base
open Misc

#nowarn "3391"

let transpileChannelType (input: ChannelType) =
    match input with
    | SendOnly -> "chan<-"
    | ReceiveOnly -> "<-chan"
    | BiDirectional -> "chan"


/// Takes a type and produces a "transpiled" version with the generic instantiation. The type name will be  
/// capitalized if the type has been exported. 
let rec transpileType (input: ResolvedAst.ResolvedType) =
    match input with
    | ResolvedAst.ResolvedType.ArrayType({ Length = length; Type = t }) ->
        let transpiledArrayLength = $"[{length}]"
        $"{transpiledArrayLength}{transpileType t}"
    | ResolvedAst.ResolvedType.ChanType(ct, t) -> (transpileChannelType ct) + " " + (transpileType t)
    //| MethodType(parameterTypes, returnType)
    | ResolvedAst.ResolvedType.FunctionType({ Parameters = parameterTypes
                                              ReturnType = returnType }) ->
        parameterTypes
        |> List.map (fun { Type = t } ->
            $"_ {transpileType t}" (* We don't care about the parameter name *) )
        |> String.concat ", "
        |> fun x -> $"func({x}) {transpileType returnType}"
    | ResolvedAst.ResolvedType.MapType(data) -> $"map[{transpileType data.KeyType}]{transpileType data.ValueType}"
    | ResolvedAst.ResolvedType.PointerType(t) -> $"*{transpileType t}"
    | ResolvedAst.ResolvedType.EnumType -> "map[int]any"
    | ResolvedAst.ResolvedType.SliceType({ Type = t }) -> $"[]{transpileType t}"
    | ResolvedAst.ResolvedType.TupleType(ts) -> "(" + (ts |> List.map (transpileType) |> String.concat ", ") + ")"
    | ResolvedAst.ResolvedType.UnitType -> "()"
    | ResolvedAst.ResolvedType.ModuleType moduleName -> moduleName
    | ResolvedAst.ResolvedType.BooleanType -> "bool"
    | ResolvedAst.ResolvedType.NullType -> "null"
    | ResolvedAst.ResolvedType.IntegerType(ByteType) -> "byte"
    | ResolvedAst.ResolvedType.IntegerType(Complex128Type) -> "complex128"
    | ResolvedAst.ResolvedType.IntegerType(Complex64Type) -> "complex64"
    | ResolvedAst.ResolvedType.IntegerType(Float32Type) -> "float32"
    | ResolvedAst.ResolvedType.IntegerType(Float64Type) -> "float64"
    | ResolvedAst.ResolvedType.IntegerType(Int8Type) -> "int8"
    | ResolvedAst.ResolvedType.IntegerType(Int16Type) -> "int16"
    | ResolvedAst.ResolvedType.IntegerType(Int32Type) -> "int32"
    | ResolvedAst.ResolvedType.IntegerType(Int64Type) -> "int64"
    | ResolvedAst.ResolvedType.QualifiedType(data) ->
        data.ModuleName + "." + data.TypeName
    | ResolvedAst.ResolvedType.IntegerType(IntType) -> "int"
    | ResolvedAst.ResolvedType.StringType -> "string"
    | ResolvedAst.ResolvedType.IntegerType(Uint16Type) -> "uint16"
    | ResolvedAst.ResolvedType.IntegerType(Uint32Type) -> "uint32"
    | ResolvedAst.ResolvedType.IntegerType(Uint64Type) -> "uint64"
    | ResolvedAst.ResolvedType.IntegerType(Uint8Type) -> "uint8"
    //| ResolvedAst.ResolvedType.DerivedFromModuleType({AttributeName = name}) -> $"Unknown (type that comes from '{name}' method or module)"
    | ResolvedAst.ResolvedType.StructType(data) ->
        transpileTypeReference data.ExportedStatus data.StructName data.GenericsInstantiation
    | ResolvedAst.ResolvedType.AnyType -> "any"
    | ResolvedAst.ResolvedType.UnknownType -> "Unknown type"
    | ResolvedAst.ResolvedType.ErrorType -> "error"
    //| CustomType(id) -> transpileCustomTypeIdentifier id astData
    | ResolvedAst.ResolvedType.ImportedType(data) -> data.ModuleName + "." + data.TypeName
    | ResolvedAst.ResolvedType.GenericType(typeName) -> typeName.Name
    | ResolvedAst.ResolvedType.AliasType(data) -> adjustName data.ExportedStatus data.AliasName
    | ResolvedAst.ResolvedType.InterfaceType(data) ->
        transpileTypeReference data.ExportedStatus data.InterfaceName data.GenericsInstantiation
    | ResolvedAst.ResolvedType.WrapperTypeType(data) ->
        transpileTypeReference data.ExportedStatus data.WrapperTypeName data.GenericsInstantiation
    | ResolvedAst.ResolvedType.MethodType(
                             { Name = name
                               ReceiverParameterName = receiverParameterName
                               ReceiverType = ResolvedAst.WrapperTypeReceiver({Name = typeName}, _)
                               ParameterTypes = parameterTypes
                               ReturnType = rt })
    | ResolvedAst.ResolvedType.MethodType({
                               Name = name
                               ReceiverParameterName = receiverParameterName
                               ReceiverType = ResolvedAst.StructReceiver({Name = typeName}, _)
                               ParameterTypes = parameterTypes
                               ReturnType = rt }) ->
        let formattedParams =
            parameterTypes
            |> List.map (fun { Name = name; Type = t } -> $"{name} {transpileType t}")
            |> String.concat ", "

        $"func ({receiverParameterName} {typeName}) {name}({formattedParams}) {transpileType rt}"


and transpileGenericsInstantiation (input:ResolvedAst.GenericsInstantiationData) = 
    input
    |> List.map transpileType
    |> String.concat ", "
    |> fun x -> if x = "" then "" else $"[{x}]"

and transpileOptionalGenericsInstantiation (input:ResolvedAst.GenericsInstantiationData option) = 
    input
    |> Option.map transpileGenericsInstantiation
    |> Option.defaultValue ""

and transpileTypeReference (exportStatus:Base.ExportedStatus) (typeName:string) (genericsInstantiation:ResolvedAst.GenericsInstantiationData) = 
    adjustName exportStatus typeName + $"{transpileGenericsInstantiation genericsInstantiation}"


let getTypeFromNumber (n:string) (property':Base.NumberProperty option) =
    let isWithinRange (n: System.Int128) (min: System.Int128) (max: System.Int128) = n >= min && n <= max
    let number = System.Int128.Parse n

    match property' with
    | Some Base.NumberProperty.Unsigned ->
        match number with
        | _ when number <= 255 -> Uint8Type
        | _ when number <= 65535 -> Uint16Type
        | _ when number <= 4294967295L -> Uint32Type
        | _ when number <= (System.Int128.Parse "18446744073709551615") -> Uint64Type
        | _ -> failwith $"Value '{number}' is not within valid range of an unsigned integer"

    | Some Base.NumberProperty.Imaginary ->
        let rangeValue64Bit = 3.4028235 * (float (pown 10 38))
        let rangeValue128Bit = 1.7976931348623157 * (float (pown 10 308))

        match number with
        | _ when float number <= rangeValue64Bit -> Complex64Type
        | _ when float number <= rangeValue128Bit -> Complex128Type
        | _ -> failwith $"'{number}' is not within the valid range of an int"

    | None ->

        match number with
        | _ when isWithinRange number -128 127 -> Int8Type
        | _ when isWithinRange number -32768 32767 -> Int16Type
        | _ when isWithinRange number -2147483648 2147483647 -> Int32Type
        | _ when isWithinRange number -9223372036854775808L 9223372036854775807L -> Int64Type
        | _ -> failwith $"'{number}' is not within valid range of an int"

let getTypeFromFloat (float':float) (isImaginary:IsImaginary) =
    let isWithinRange (n: float) (min: float) (max: float) = n >= min && n <= max
    let float32RangeNeg, float32Range = float -2147483648, float 2147483647

    let float64RangeNeg, float64Range =
        float -9223372036854775808L, float 9223372036854775807L

    let complex64RangeNeg, complex64Range = float32RangeNeg, float32Range
    let complex128RangeNeg, complex128Range = float64RangeNeg, float64Range

    if isImaginary then

        match float with
        | _ when isWithinRange float' complex64RangeNeg complex64Range -> Complex64Type
        | _ when isWithinRange float' complex128RangeNeg complex128Range -> Complex128Type
        | _ -> failwith $"'{float'}' is not within range of complex128"
    else
        match float with
        | _ when isWithinRange float' float32RangeNeg float32Range -> Float32Type
        | _ when isWithinRange float' float64RangeNeg float64Range -> Float64Type
        | _ -> failwith $"'{float'}' is not within range of complex128"


let rec getTypeFromConst (constants:Map<string, ResolvedAst.ConstDeclarationData>) (input:ResolvedAst.ConstExpression) = 
    match input with
    | ResolvedAst.ConstExpression.Boolean(b) -> ResolvedAst.ResolvedType.BooleanType
    | ResolvedAst.ConstExpression.Float(floatData) -> ResolvedAst.ResolvedType.IntegerType(getTypeFromFloat floatData.Float floatData.IsImaginary)
    (* | ResolvedAst.ConstExpression.Iota ->
        ResolvedAst.ResolvedType.IntegerType(IntType.IntType) *)
    | ResolvedAst.ConstExpression.Const(constData) ->
        constants
        |> Map.find constData.IdentifierName
        |> _.Expression
        |> getTypeFromConst constants
    | ResolvedAst.ConstExpression.Number(numberData) -> ResolvedAst.ResolvedType.IntegerType(getTypeFromNumber numberData.StringRepresentation numberData.Property)
    | ResolvedAst.ConstExpression.String(_) -> ResolvedAst.ResolvedType.StringType

let mkASTData (ast: Ast.AST) : Ast.ASTData =

    let mutable (functions: Ast.FunctionData list) = []
    // Add the built-in ones
    let mutable (structs: Ast.StructData list) = []
    let mutable (interfaces: Ast.InterfaceData list) = []
    let mutable (consts: Ast.ConstData list) = []
    let mutable (wrapperTypes: Ast.WrapperTypeData list) = []
    let mutable (importedModuleIdentifiers: (ModuleName * IdentifierName) list) = []
    let mutable (aliases: Ast.AliasData list) = []
    let mutable (enums: Ast.EnumData list) = []
    //let mutable (structReferences: StructReference list) = []
    //let mutable (interfaceRefereces: InterfaceReference list) = []

    let mutable (methods: ( (* CustomTypeIdentifier *  *) Ast.MethodData) list) = []



    // Populate lists
    for astConstruct in ast do
        match astConstruct with
        | Ast.Const(data) ->
            (* let (varData:Ast.IdentifierData) =
                { VarName = name
                  Type = getTypeFromConst consts constExpr
                  ExportStatus = Exportable exportStatus } *)

            consts <- Misc.listAdd consts data
        | Ast.ToplevelDeclaration.Function(data) -> functions <- Misc.listAdd functions data
        | Ast.ToplevelDeclaration.Enum(enum) ->
            enums <- Misc.listAdd enums enum 
        | Ast.ToplevelDeclaration.Struct(_, structName, _, constructs as data) -> structs <- Misc.listAdd structs data
        (* for construct in constructs do
                structFields <- Misc.listAdd structFields (structName, construct) *)
        | Ast.ToplevelDeclaration.Interface(_, interfaceName, _, constructs as data) ->
            interfaces <- Misc.listAdd interfaces data
        (* for construct in constructs do
                interfaceMethods <- Misc.listAdd interfaceMethods (interfaceName, construct) *)
        | Ast.Import(moduleName, moduleIdentifierName) ->
            importedModuleIdentifiers <- Misc.listAdd importedModuleIdentifiers (moduleName, moduleIdentifierName)
        | Ast.ToplevelDeclaration.Method(_, _, (receiverType, _), _, _ as methodData) ->
            methods <- Misc.listAdd methods methodData
        | Ast.WrapperType(data) -> wrapperTypes <- Misc.listAdd wrapperTypes data
        | Ast.Alias(data) -> aliases <- Misc.listAdd aliases data


    let mutable (astData: Ast.ASTData) =
        { Methods = methods |> List.map(fun ((_, methodName, _, _, _) as d)-> methodName, d) |> Map.ofList
          ImportedModules = importedModuleIdentifiers |> List.map(fun ((_, identifierName) as d) -> identifierName, d) |> Map.ofList
          Functions = functions |> List.map(fun ((_, functionName, _, _, _, _) as d)-> functionName, d) |> Map.ofList
          Interfaces = interfaces |> List.map(fun ((_, interfaceName, _, _) as d)-> interfaceName, d) |> Map.ofList
          WrapperTypes = wrapperTypes |> List.map(fun ((_, wrapperTypeName, _, _) as d)-> wrapperTypeName, d) |> Map.ofList
          Structs = structs |> List.map(fun ((_, structName, _, _) as d)-> structName, d) |> Map.ofList
          Consts = consts |> List.map(fun ((_, name, _) as d) -> name, d) |> Map.ofList
          RawAST = ast
          Aliases = aliases |> List.map(fun ((_, aliasName, _, _) as d)-> aliasName, d) |> Map.ofList
          Enums = enums |> List.map(fun ((enumName, _) as d)-> enumName, d) |> Map.ofList}


    astData




/// Takes an accessor and retrives the type from it 
let getTypeFromExprArgAccessor (declarations:ResolvedAst.ResolvedDeclarations) (accessor:ResolvedAst.AccessorItemExprArgs) = 
    match accessor with
    | ResolvedAst.AccessorItemExprArgs.InterfaceDeclaration({InterfaceDeclaration = {Name = DeclaredInterface declarations interfaceData}; GenericsInstantiation = genericsInstantiation}) ->
        ResolvedAst.ResolvedType.InterfaceType({ InterfaceName = interfaceData.Name; GenericsInstantiation = genericsInstantiation; ExportedStatus = interfaceData.ExportedStatus})
    | ResolvedAst.AccessorItemExprArgs.InterfaceMethod(data, _, None) ->
        ResolvedAst.ResolvedType.FunctionType({Parameters = parametersToFunctionTypeParameter data.Parameters; ReturnType = data.ReturnType})
    | ResolvedAst.AccessorItemExprArgs.InterfaceMethod(data, _, Some(_)) ->
        data.ReturnType
    | ResolvedAst.AccessorItemExprArgs.Method(data, _, None) ->
        ResolvedAst.ResolvedType.FunctionType({Parameters = parametersToFunctionTypeParameter data.Parameters; ReturnType = data.ReturnType})
    | ResolvedAst.AccessorItemExprArgs.Method(data, _, Some _) ->
        data.ReturnType
    | ResolvedAst.AccessorItemExprArgs.StructDeclaration({StructDeclaration = {Name = DeclaredStruct declarations structData}; GenericsInstantiation = genericsInstantiation}) ->
        ResolvedAst.ResolvedType.StructType({StructName = structData.Name; GenericsInstantiation = genericsInstantiation; ExportedStatus = structData.ExportedStatus})
    | ResolvedAst.AccessorItemExprArgs.StructField(data, _, None) ->
        data.Type
    | ResolvedAst.AccessorItemExprArgs.StructField({Type = ResolvedAst.ResolvedType.FunctionType({ReturnType = rt})}, _, Some _) ->
        rt
//    | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name) ->
//        ResolvedAst.ResolvedType.DerivedFromModuleType({AttributeName = name})
    | ResolvedAst.AccessorItemExprArgs.StructField({Type = _}, _, Some _) ->
        failwith "Will never get here."
        //failwith "Will never get here. Case taken care of in 'accessorItemWithoutCallDataToAccessorItem'"

        // I COULD make this case unrepresentable, but it would be too
        // much work just for this


/// Takes an accessor and retrives the type from it 
let getTypeFromPipeArgAccessor (declarations:ResolvedAst.ResolvedDeclarations) (accessor:ResolvedAst.AccessorItemPipeArgs) = 
    match accessor with
    | ResolvedAst.AccessorItemPipeArgs.InterfaceDeclaration({InterfaceDeclaration = {Name = DeclaredInterface declarations interfaceData}; GenericsInstantiation = genericsInstantiation}) ->
        ResolvedAst.ResolvedType.InterfaceType({ InterfaceName = interfaceData.Name; GenericsInstantiation = genericsInstantiation; ExportedStatus = interfaceData.ExportedStatus})
    | ResolvedAst.AccessorItemPipeArgs.InterfaceMethod(data, _, _) ->
        data.ReturnType
    | ResolvedAst.AccessorItemPipeArgs.Method(data, _, _) ->
        data.ReturnType
    | ResolvedAst.AccessorItemPipeArgs.StructDeclaration({StructDeclaration = {Name = DeclaredStruct declarations structData}; GenericsInstantiation = genericsInstantiation}) ->
        ResolvedAst.ResolvedType.StructType({StructName = structData.Name; GenericsInstantiation = genericsInstantiation; ExportedStatus = structData.ExportedStatus})
    | ResolvedAst.AccessorItemPipeArgs.StructField({Type = ResolvedAst.ResolvedType.FunctionType({ReturnType = rt})}, _, _) ->
        rt
//    | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name) ->
//        ResolvedAst.ResolvedType.DerivedFromModuleType({AttributeName = name})
    | ResolvedAst.AccessorItemPipeArgs.StructField({Type = _}, _, _) ->
        failwith "Will never get here."
        // I COULD make this case unrepresentable, but it would be too
        // much work just for this


let createStructTypeFromDeclaration (input:ResolvedAst.StructDeclarationData) (genericsInstantiation:ResolvedAst.GenericsInstantiationData) = 
    ResolvedAst.ResolvedType.StructType({StructName = input.Name; ExportedStatus = input.ExportedStatus; GenericsInstantiation = genericsInstantiation})

let createInterfaceTypeFromDeclaration (input:ResolvedAst.InterfaceDeclarationData) (genericsInstantiation:ResolvedAst.GenericsInstantiationData) =
    ResolvedAst.ResolvedType.InterfaceType({InterfaceName = input.Name; ExportedStatus = input.ExportedStatus; GenericsInstantiation = genericsInstantiation})

let createWrapperTypeFromDeclaration (input:ResolvedAst.WrapperTypeDeclarationData) (genericsInstantiation:ResolvedAst.GenericsInstantiationData) = 
    ResolvedAst.ResolvedType.WrapperTypeType({WrapperTypeName = input.Name; ExportedStatus = input.ExportedStatus; GenericsInstantiation = genericsInstantiation})
