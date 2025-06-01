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

(* /// Gets the return type of a function in the AST syntax tree
let getFunctionReturnType (functionName: string) (ast:AST) =
    ast
    |> List.tryPick (function
    | ToplevelDeclarationType(Function(_, (name, _, _, returnType, _))) when name = functionName ->
        Some returnType
    | _ -> None) *)

(* let rec getZeroValue (t: UnresolvedType) =
    match t with
    | BooleanType(_) -> Ast.Expression.Boolean false
    | TupleType(tps) -> Ast.Expression.Tuple(List.map getZeroValue tps)
    | Uint8Type
    | Uint16Type
    | Uint32Type
    | Uint64Type
    | IntType
    | Int8Type
    | Int16Type
    | Int32Type
    | Int64Type
    | Float32Type
    | Float64Type
    | Complex64Type
    | Complex128Type
    | ByteType
    | RuneType -> Ast.Expression.Number(false, "0", false, false)
    | StringType -> Ast.Expression.String ""
    | PointerType(_)
    | FunctionType(_)
    | SliceType(_)
    | ChanType(_)
    | AnyType
    | ArrayType(_)
    | ErrorType(_)
    | UnitType
    | ImportedType(_)
    //| MethodType(_)
    | NullType
    | MapType(_) -> Ast.Null

 *)


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

// let transpileGenericsInstantiation = transpileGenericsInstantiation'

(* and transpileFunctionGenericArgs (input: GenericsInstantiationData) =
    if input.Length = 0 then
        ""
    else
        input
        |> List.map (transpileTypeName astData)
        |> String.concat ", "
        |> (fun x -> $"[{x}]")

and transpileCustomTypeIdentifier ((typeName, genericsInstantiation): CustomTypeIdentifier) (astData: Ast.ASTData) =
    let isExported =
        astData.CustomTypes
        |> List.tryPick (fun (isExported, tn, _) -> if typeName = tn then Some Exported else None)
        |> Misc.expect $"The type '{typeName}' was not found"

    let transpiledG = transpileFunctionGenericArgs astData genericsInstantiation
    Misc.adjustName isExported typeName + transpiledG


/// Seperate the struct constructs into two groups
let seperateStructConstructs (input: Ast.StructConstruct list) =
    let mutable ((fields: Ast.StructConstruct list), (embeddedStructs: Ast.StructConstruct list)) =
        [], []

    for construct in input do
        match construct with
        | Ast.Field _ as f -> fields <- List.append [ f ] fields
        | Ast.EmbeddedStruct _ as e -> embeddedStructs <- List.append [ e ] embeddedStructs


    fields, embeddedStructs
 *)
(* let getTypeMethods (typeName:string) (astData:ASTData) = 
    astData.FieldsData.Methods
    |> List.filter(fun ((typeReceiverName, _), (_, (_, _, _, _))) ->
        Misc.compareLowercase typeName typeReceiverName)
    |> List.map(fun ((_, _), (_, (methodName, parameters, rt, _))) ->
        methodName, FunctionType(Misc.makeFunctionTypeData parameters rt)
    ) *)

(* let rec getTypeAttributes (t:Type) (astData:ASTData): (AttributeName * Type) list = 
    match t with
    | Uint8Type
    | Uint16Type
    | Uint32Type
    | Uint64Type
    | IntType
    | Int8Type
    | Int16Type
    | Int32Type
    | Int64Type
    | Float32Type
    | Float64Type
    | Complex64Type
    | Complex128Type
    | NullType
    | PointerType(_)
    | SliceType(_)
    | StringType
    | TupleType(_)
    | UnitType
    | UnknownType
    | ByteType
    | RuneType
    | Complex128Type
    | ChanType(_)
    | ByteType
    | BooleanType
    | AnyType
    | ErrorType
    | GenericType(_)
    | ImportedType(_)
    | MapType(_)
    | MethodType(_)
    | FunctionType(_)
    | ArrayType(_) -> []
    | ModuleType(_) -> [] // We don't know
    | CustomType(typeName, _) ->
        // Start with methods
        let mutable attributes: (AttributeName * Type) list = getTypeMethods typeName astData
        match typeName with
        | Struct astData (_, (_, _, constructs)) ->
            for construct in constructs do
                match construct with
                | Field(_, (attributeType, attributeName), _) ->
                    attributes <- Misc.listAdd attributes (attributeName, attributeType)
                | EmbeddedStruct(structRef) ->
                    attributes <- List.append attributes (getTypeAttributes structRef astData)
            attributes
        | Interface astData (_, (_, _, constructs)) ->
            for construct in constructs do
                match construct with
                | InterfaceConstruct.InterfaceMethod(_, (methodName, parameters, rt)) ->
                    attributes <- Misc.listAdd attributes (methodName, FunctionType(List.map (fun (a, b, c) -> a, b) parameters, rt))
                | EmbeddedInterface(interfaceRef) ->
                    attributes <- List.append attributes (getTypeAttributes interfaceRef astData)
            attributes
        | t -> failwith $"Unknown type '{t}'"
 *)


// TODO
(* let (|StructField|_|) (targetStructName:string) (name:string) (astData:ASTData) = 
    astData.FieldsData.StructFields
    |> List.tryFind(fun (structName, structConstruct) ->
        let isCorrectStruct = Misc.compareLowercase targetStructName structName
        if isCorrectStruct then
            match structConstruct with
            | Field(_, (_, fieldName), _) -> Misc.compareLowercase name fieldName
            | EmbeddedStruct((name, _)) ->
                match name with
                | Struct astData (_, _, _, constructs) ->
                    List.exists (match constructs with) constructs
                    Misc.compareLowercase name fieldName
        else
            false
    ) *)



(* let (|Method|_|) (name:string) (receiverTypeName:string) (astData:Ast.ASTData) = 
    astData.FieldsData.Methods
    |> List.tryFind(fun ((typeIdentifierName, _), (_, methodName, _, _, _)) ->
        let isCorrectReceiver = Misc.compareLowercase receiverTypeName typeIdentifierName
        let isCorrectMethod = Misc.compareLowercase name methodName
        isCorrectReceiver && isCorrectMethod
    ) *)


(* let (|InterfaceField|_|) (targetInterfaceName:string) (name:string) (astData:Ast.ASTData) = 
    astData.FieldsData.InterfaceFields
    |> List.tryFind(fun (structName, structConstruct) ->
        let isCorrectStruct = Misc.compareLowercase targetInterfaceName structName
        if isCorrectStruct then
            match structConstruct with
            | Ast.InterfaceConstruct.InterfaceMethod(_, (methodName, _, _)) -> Misc.compareLowercase name methodName
            | Ast.InterfaceConstruct.EmbeddedType(t) -> false // Embedded types in interfaces are only for generics
        else
            false
    ) *)

(* 

let getAttributeData
    (baseType: UnresolvedType)
    (attributeName: string)
    (astData: Ast.ASTData)
    : UnresolvedType * ExportedStatus =
    let baseTypeName = transpileTypeName astData baseType
    let cl = Misc.compareLowercase
    // Look through struct fields, interface fields, and methods and see if there's a match

    let structAttributeData' =
        astData.FieldsData.StructFields
        |> List.tryPick (fun (structName, (fieldType, fieldName)) ->

            let isCorrectStruct = cl structName baseTypeName

            match fieldType with
            | Field(isExported, (fieldType, fieldName), _) when isCorrectStruct && cl attributeName fieldName ->
                Some(fieldType, isExported)
            | Ast.StructConstruct.EmbeddedStruct(targetStructName, _ as customTypeIdentifier) when
                isCorrectStruct && cl attributeName targetStructName
                ->
                // We must find the struct and see if it's exported.
                // The attribute's name is always the name of the embedded's struct's name
                astData.FieldsData.Structs
                |> List.tryPick (fun (isExported, (structName, _, _)) ->
                    if cl structName targetStructName then
                        Some isExported
                    else
                        None)
                |> Option.map (fun isExported -> CustomType(customTypeIdentifier), isExported)
            | _ -> None)

    let interfaceAttributeData' =
        astData.FieldsData.InterfaceFields
        |> List.tryPick (fun (interfaceName, constructType) ->
            let isCorrectInterface = baseTypeName = interfaceName

            match constructType with
            | StructEmbed((typeName, _) as identifierData) when isCorrectInterface && cl attributeName typeName ->
                // TODO: The 'false' below is a placeholder until I actually get it to check if another type is exported
                Some(CustomType identifierData, false)
            | InterfaceMethod(isExported, (methodName, _, rt)) when isCorrectInterface && cl attributeName methodName ->
                Some(rt, isExported)
            | _ -> None)

    let methodAttributeData' =
        astData.FieldsData.Methods
        |> List.tryPick (fun ((receiverTypeName, _), (isExported, methodName, parameters, rt, _)) ->
            let isCorrectReceiver = cl receiverTypeName baseTypeName

            if isCorrectReceiver && cl methodName attributeName then
                Some(FunctionType(List.map (fun (a, b, _) -> a, b) parameters, rt), isExported)
            else
                None)

    match baseType with
    | ModuleType(_) -> UnknownType, Exported
    | _ ->
        structAttributeData'
        |> Misc.optionOr interfaceAttributeData'
        |> Misc.optionOr methodAttributeData'
        |> Misc.expect $"Type {baseTypeName} has no attribute {attributeName}"
 *)
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

(* let findReferenceType (identifierName: IdentifierName) (variablesInScope: ScopeVariables) (astData: Ast.ASTData) =

    let asFunction' =
        astData.Functions
        |> List.tryPick (fun (_, functionName, _, parameters, rt, _) ->
            if functionName = identifierName then
                Some(FunctionType(List.map (fun (hE, t, _) -> hE, t) parameters, rt))
            else
                None)

    let asImportedModule' =
        astData.ImportedModuleIdentifiers
        |> List.tryPick (fun moduleIdentifierName ->
            if identifierName = moduleIdentifierName then
                Some(ModuleType moduleIdentifierName)
            else
                None)

    let asVariable' =
        variablesInScope
        |> List.tryPick (fun { VarName = name; VarType = t } -> if name = identifierName then Some t else None)

    asFunction' |> Misc.optionOr asImportedModule' |> Misc.optionOr asVariable'

 *)
(* let rec getTypeFromExpression
    (expression: Ast.Expression)
    (astData: Ast.ASTData)
    (variablesInScope: ScopeVariables)
    : UnresolvedType =
    match expression with
    | Ast.MapLookup(mapReference, _) ->
        let referenceType = getTypeFromExpression mapReference astData variablesInScope

        match referenceType with
        | MapType(_, t) -> t
        | _ -> failwith $"Type '{referenceType}' is not a map type"
    | Ast.Expression.Number(numberData) -> getTypeFromNumber numberData
    | Ast.Expression.Accessors(accessorData) ->
        mapAccessorsToTypes<Ast.Expression> accessorData astData variablesInScope
        |> List.last
    | Ast.Expression.ArrayLiteral(l, t, _) -> ArrayType(l, t)
    | Ast.Expression.Boolean(_) -> BooleanType
    | Ast.Expression.ChannelRecieve(identName) ->
        getTypeFromExpression (Ast.Expression.Identifier(identName)) astData variablesInScope
    | Ast.Expression.CompositeLiteral(UnresolvedStructReference(t), _) -> UnresolvedType(t)
    | Ast.Expression.DereferenceOf(identName) ->
        getTypeFromExpression (Ast.Expression.Identifier(identName)) astData variablesInScope
    | Ast.Expression.Float(floatData) -> getTypeFromFloat floatData
    | Ast.Expression.FunctionCall(possibleFunctionReference, _) ->
        match possibleFunctionReference with
        | Ast.PossibleFunctionReference.Identifier i ->
            match getTypeFromExpression (Ast.Expression.Identifier(i)) astData variablesInScope with
            | FunctionType(_, rt) -> rt
            | t -> failwith $"Type '{t}' cannot be called."
        | Ast.PossibleFunctionReference.Lambda(_, returnType, _) -> returnType
        | Ast.PossibleFunctionReference.Parentheses(insideExpr) ->
            match getTypeFromExpression insideExpr astData variablesInScope with
            | FunctionType(_, rt) -> rt
            | t -> failwith $"Type '{t}' cannot be called."
    | Ast.Expression.Identifier(identifierName) ->
        findReferenceType identifierName variablesInScope astData
        |> Misc.expect $"Identifier '{identifierName}' not found"
    | Ast.Expression.Index(indexableReference, _) ->
        match indexableReference with
        | Ast.IndexableReference.ArrayLiteral(_, t, _) -> t
        | Ast.IndexableReference.Identifier(identifier) ->
            getTypeFromExpression (Ast.Expression.Identifier(identifier)) astData variablesInScope
        | Ast.IndexableReference.MapLiteral(_, valueType, _) -> valueType
        | Ast.IndexableReference.Parentheses(innerExpr) -> getTypeFromExpression innerExpr astData variablesInScope
        | Ast.IndexableReference.PointerTo(identName) ->
            getTypeFromExpression (Ast.Expression.Identifier(identName)) astData variablesInScope
        | Ast.IndexableReference.SliceLiteral(t, _) -> t
        | Ast.IndexableReference.String(_) -> StringType
    | Ast.Expression.Lambda(parameters, rt, _) -> FunctionType(List.map (fun (hE, t, _) -> hE, t) parameters, rt)
    | Ast.Expression.MakeFunction(t, _) -> t
    | Ast.Expression.MapLiteral(keyType, valueType, _) -> MapType(keyType, valueType)
    | Ast.Expression.NegatedExpression(_) -> BooleanType
    | Ast.Expression.NewFunction(t) -> t
    | Ast.Expression.WrapperTypeLiteral((NewTypeLiteralTypeReference(t)), _) -> UnresolvedType(t)
    | Ast.Expression.Null -> NullType
    | Ast.Expression.OperatorSequence(firstExpr, _) -> getTypeFromExpression firstExpr astData variablesInScope
    | Ast.Expression.Parentheses(innerExpr) -> getTypeFromExpression innerExpr astData variablesInScope
    | Ast.Expression.PipeOperator(_, pipes) ->
        pipes
        |> List.last
        |> function
            | Ast.PipeCall.FunctionCall(possibleFunctionReference, _) ->
                match possibleFunctionReference with
                | Ast.PossibleFunctionReference.Identifier(identifier) ->
                    let tarfiableType =
                        List.pick
                            (fun { VarName = name; VarType = t } -> if name = identifier then Some t else None)
                            variablesInScope

                    match targetVariableType with
                    | FunctionType(_, rt) -> rt
                    | t -> let tn = transpileTypeName astData t in failwith $"Type '{tn}' is not callable"
                | Ast.PossibleFunctionReference.Lambda(_, rt, _) -> rt
                | Ast.PossibleFunctionReference.Parentheses(innerExpr) ->
                    let targetVariableType = getTypeFromExpression innerExpr astData variablesInScope

                    match targetVariableType with
                    | FunctionType(_, rt) -> rt
                    | t -> let tn = transpileTypeName astData t in failwith $"Type '{tn}' is not callable"
            | Ast.PipeCall.AccessorCall(accessorData) ->
                mapAccessorsToTypes<Ast.PipeFunctionCallArgument> accessorData astData variablesInScope
                |> List.last
    | Ast.Expression.PointerTo(identifier) ->
        PointerType(getTypeFromExpression (Ast.Expression.Identifier(identifier)) astData variablesInScope)
    | Ast.Slice(sliceableReference, _, _) ->
        match sliceableReference with
        | Ast.SliceableReference.ArrayLiteral(_, t, _) -> SliceType(t)
        | Ast.SliceableReference.SliceLiteral(t, _) -> SliceType(t)
        | Ast.SliceableReference.String(_) -> SliceType(StringType)
    | Ast.Expression.SliceLiteral(t, _) -> SliceType(t)
    | Ast.Expression.String(_) -> StringType
    | Ast.Expression.StructLiteral(UnresolvedStructReference(ti), _) -> UnresolvedType(ti)
    | Ast.Expression.SwitchCase(_, rt, _, _) -> rt
    | Ast.Expression.TernaryOperator(t, _, _, _) -> t
    | Ast.Expression.Tuple(exprs) ->
        exprs
        |> List.map (fun e -> getTypeFromExpression e astData variablesInScope)
        |> TupleType
    | Ast.Expression.TypeAssertion(_, t) -> t
    | Ast.Expression.TypeConversion(t, _) -> t
    | Ast.Expression.UnaryOperator(e, _, _) -> getTypeFromExpression e astData variablesInScope
    | Ast.Expression.Unit -> UnitType

and getTypeFromConst (constExpr: Ast.ConstExpression) (previousConsts: ScopeVariables) =
    match constExpr with
    | Ast.ConstExpression.Number(_, number, endsWithU, isImaginary) -> getTypeFromNumber number endsWithU isImaginary
    | Ast.ConstExpression.Boolean(_) -> BooleanType
    | Ast.ConstExpression.Float(isNegative, f, isImaginary) -> getTypeFromFloat f isImaginary
    | Ast.ConstExpression.String(_) -> StringType
    | Ast.ConstExpression.Identifier(targetIdentifier) ->
        previousConsts
        |> List.tryPick
            (fun
                { VarName = previousIdentiferName
                  VarType = t } ->
                if previousIdentiferName = targetIdentifier then
                    Some t
                else
                    None)
        |> Misc.expect $"Const '{targetIdentifier}' is not defined"

and getTypeFromResolvedConst (constExpr: ResolvedAst.ConstExpression) (previousConsts: ScopeVariables) =
        
    match constExpr with
    | ResolvedAst.ConstExpression.Number({IsNegative = isNegative; StringRepresentation = sr; EndsWithU = endsWithU; IsImaginary = isImaginary}) -> getTypeFromNumber sr endsWithU isImaginary
    | ResolvedAst.ConstExpression.Boolean(_) -> BooleanType
    | ResolvedAst.ConstExpression.Float({IsNegative = isNegative; Float = f; IsImaginary = isImaginary}) -> getTypeFromFloat f isImaginary
    | ResolvedAst.ConstExpression.String(_) -> StringType
    | ResolvedAst.ConstExpression.Identifier(targetIdentifier) ->
        previousConsts
        |> List.tryPick
            (fun
                { VarName = previousIdentiferName
                  VarType = t } ->
                if previousIdentiferName = targetIdentifier then
                    Some t
                else
                    None)
        |> Misc.expect $"Const '{targetIdentifier}' is not defined"

 *)

(* and getExportStatus (identifierName: IdentifierName) ((astData, _, _): Context) =
    astData.Consts
    |> List.tryFind (fun (_, { VarName = n }) -> identifierName = n)
    |> Option.map (fun (isExported, _) -> Exportable isExported)
    |> Option.defaultValue NonExportable *)

(* and toIdentifierData
    (identifierName: IdentifierName)
    (exportable: ExportStatus)
    ((astData, _, scopeData) as context: Ast.Context)
    =
    { VarType = getTypeFromExpression (Ast.Expression.Identifier(identifierName)) astData scopeData.Variables
      VarName = identifierName
      ExportStatus = exportable }

and mapAccessorsToTypes<'T>
    ((initalExpr, accessors): Ast.AccessorsData<'T>)
    (astData: Ast.ASTData)
    (variablesInScope: ScopeVariables)
    : UnresolvedType list =
    let mutable previousType = getTypeFromExpression initalExpr astData variablesInScope

    let mutable types = []

    for accessor in accessors do
        match accessor with
        | Ast.FieldAccessor(fieldName, None) ->
            let fieldType, _ = getAttributeData previousType fieldName astData
            previousType <- fieldType
            types <- Misc.listAdd types fieldType
        | Ast.FieldAccessor(fieldName, Some(_)) ->
            let targetVariableType, _ = getAttributeData previousType fieldName astData

            match targetVariableType with
            | FunctionType(_, rt) ->
                previousType <- rt
                types <- Misc.listAdd types rt
            | t -> let tn = transpileTypeName astData t in failwith $"Type '{tn}' is not callable"
        | Ast.MethodAccessor(methodName, None) ->
            let fieldType, _ = getAttributeData previousType methodName astData
            previousType <- fieldType
            types <- Misc.listAdd types fieldType
        | Ast.MethodAccessor(methodName, Some(_)) ->
            let targetVariableType, _ = getAttributeData previousType methodName astData

            match targetVariableType with
            | FunctionType(_, rt) ->
                previousType <- rt
                types <- Misc.listAdd types rt
            | t -> let tn = transpileTypeName astData t in failwith $"Type '{tn}' is not callable"


    List.rev types

let (|ScopeDeclaration|CasesDeclaration|VariableDeclaration|NonDeclaratory|) (scopeConstruct: Ast.ScopeConstruct) =
    match scopeConstruct with
    | Ast.ScopedDeclaration(Ast.NormalVar(variablePattern, expr)) -> VariableDeclaration(variablePattern, expr)
    | Ast.ScopedDeclaration(Ast.ZeroVar(variablePattern, t)) -> VariableDeclaration(variablePattern, getZeroValue t)
    | Ast.ScopedSatement(Ast.ForLoop(style, rawScopeData)) ->
        match style with
        | Ast.ShortHand(variablePattern, expr) -> ScopeDeclaration(rawScopeData, Some(variablePattern, expr))
        | Ast.Traditional((variablePattern, expr), _, _) -> ScopeDeclaration(rawScopeData, Some(variablePattern, expr))
    | Ast.ScopedSatement(Ast.Select(cases, defaultCase')) ->
        let mutable mapped = cases |> List.map (fun (_, _, rawScopeData) -> rawScopeData)

        match defaultCase' with
        | Some(Ast.DefaultCase(rawScopeData)) -> mapped <- Misc.listAdd mapped rawScopeData
        | None -> ()

        CasesDeclaration mapped
    | Ast.ScopedSatement(Ast.When(_, cases, defaultCase')) ->
        let mutable mapped = cases |> List.map (fun (_, rawScopeData) -> rawScopeData)

        match defaultCase' with
        | Some(Ast.DefaultCase(rawScopeData)) -> mapped <- Misc.listAdd mapped rawScopeData
        | None -> ()

        CasesDeclaration mapped
    | Ast.ScopedSatement(Ast.Try(identifiers', expr, _)) ->
        match identifiers' with
        | Some(var1, var2) -> VariableDeclaration([ var1; var2 ], expr)
        | None -> NonDeclaratory
    | Ast.ScopedSatement(Ast.Force(identifier', expr, _)) ->
        match identifier' with
        | Some(var) -> VariableDeclaration([ var ], expr)
        | None -> NonDeclaratory
    | Ast.ScopedSatement(Ast.If(_, rawScopeData))
    | Ast.ScopedSatement(Ast.Defer(rawScopeData))
    | Ast.ScopedSatement(Ast.Go(rawScopeData))
    | Ast.ScopedExpression(Ast.Expression.Lambda(_, _, rawScopeData))
    | Ast.ScopedSatement(Ast.WhileLoop(_, rawScopeData)) -> ScopeDeclaration(rawScopeData, None)
    | Ast.ScopedSatement(Ast.Return(_))
    | Ast.ScopedSatement(Ast.Break)
    | Ast.ScopedExpression(Ast.Expression.Number(_))
    | Ast.ScopedExpression(Ast.Expression.Float(_))
    | Ast.ScopedExpression(Ast.Expression.String(_))
    | Ast.ScopedExpression(Ast.Expression.FunctionCall(_))
    | Ast.ScopedExpression(Ast.Expression.Identifier(_))
    | Ast.ScopedExpression(Ast.Expression.Boolean(_))
    | Ast.ScopedExpression(Ast.Null)
    | Ast.ScopedExpression(Ast.OperatorSequence(_))
    | Ast.ScopedExpression(Ast.Expression.Parentheses(_))
    | Ast.ScopedExpression(Ast.TernaryOperator(_))
    | Ast.ScopedExpression(Ast.StructLiteral(_))
    | Ast.ScopedExpression(Ast.Expression.ArrayLiteral(_))
    | Ast.ScopedExpression(Ast.Expression.SliceLiteral(_))
    | Ast.ScopedExpression(Ast.Expression.MapLiteral(_))
    | Ast.ScopedExpression(Ast.DereferenceOf(_))
    | Ast.ScopedExpression(Ast.Expression.PointerTo(_))
    | Ast.ScopedExpression(Ast.ChannelRecieve(_))
    | Ast.ScopedExpression(Ast.MapLookup(_))
    | Ast.ScopedExpression(Ast.SwitchCase(_))
    | Ast.ScopedExpression(Ast.CompositeLiteral(_))
    | Ast.ScopedExpression(Ast.PipeOperator(_))
    | Ast.ScopedExpression(Ast.Tuple(_))
    | Ast.ScopedExpression(Ast.TypeAssertion(_))
    | Ast.ScopedExpression(Ast.MakeFunction(_))
    | Ast.ScopedExpression(Ast.NewFunction(_))
    | Ast.ScopedExpression(Ast.Expression.WrapperTypeLiteral(_))
    | Ast.ScopedExpression(Ast.Accessors(_))
    | Ast.ScopedExpression(Ast.NegatedExpression(_))
    | Ast.ScopedExpression(Ast.TypeConversion(_))
    | Ast.ScopedExpression(Ast.UnaryOperator(_))
    | Ast.ScopedExpression(Ast.Index(_))
    | Ast.ScopedExpression(Ast.Slice(_))
    | Ast.ScopedExpression(Ast.Unit) -> NonDeclaratory

let (|TypeDeclaration|_|) (toplevelDeclaration: Ast.ToplevelDeclaration) =
    match toplevelDeclaration with
    | Ast.Struct(isExported, structName, generics, _) -> Some(isExported, structName, Some generics)
    | Ast.ToplevelDeclaration.Interface(isExported, interfaceName, generics, _) ->
        Some(isExported, interfaceName, Some generics)
    | Ast.WrapperType(isExported, newTypeName, generics, _) -> Some(isExported, newTypeName, Some generics)
    | Ast.Alias(isExported, aliasName, _) -> Some(isExported, aliasName, None)
    | Ast.ToplevelDeclaration.Function(_)
    | Ast.ToplevelDeclaration.Method(_)
    | Ast.Import(_)
    | Ast.Const(_) -> None

let indexScopes (ast: Ast.AST) (astData: Ast.ASTData) =

    let mutable toplevelScopes = []

    let addVariables (input: ResolvedAst.ScopeData) (newVariables: IdentifierData list) =
        input.Variables <- List.append input.Variables newVariables

    let addToAboveScope (aboveScopeData: ResolvedAst.ScopeData) (newScope: ResolvedAst.ScopeData) =
        aboveScopeData.Scopes <- Misc.listAdd aboveScopeData.Scopes newScope

    let convertToVariableData (input: IdentifierName) (t: Base.UnresolvedType) (exportedStatus: ExportStatus) =
        { VarName = input
          VarType = t
          ExportStatus = exportedStatus }

    let convertAllToVariableData (input: VariablePattern) (t: Base.UnresolvedType) (exportedStatus: ExportStatus) =
        input |> List.map (fun n -> convertToVariableData n t exportedStatus)

    let makeScope (scopeId: ScopeId) (aboveVars: (IdentifierData list) option) : ResolvedAst.ScopeData =
        { Id = scopeId
          Variables = Option.defaultValue [] aboveVars
          Scopes = [] }


    let rec indexScope (scopeContent: Ast.ScopeContent) (currentScopeData: ResolvedAst.ScopeData) =

        for scopeConstruct in scopeContent do
            match scopeConstruct with
            | ScopeDeclaration((scopeContent, scopeId), Some((variablePattern, expr))) ->
                let convertedDeclaredVariables =
                    let exprType = getTypeFromExpression expr astData currentScopeData.Variables
                    convertAllToVariableData variablePattern exprType NonExportable

                let predefinedVariables =
                    List.append currentScopeData.Variables convertedDeclaredVariables

                indexAndAttachScope scopeId currentScopeData scopeContent (Some predefinedVariables)
            | ScopeDeclaration((scopeContent, scopeId), None) ->
                indexAndAttachScope scopeId currentScopeData scopeContent (Some currentScopeData.Variables)
            | CasesDeclaration(casesScopeData) ->
                for (scopeContent, scopeId) in casesScopeData do
                    indexAndAttachScope scopeId currentScopeData scopeContent (Some currentScopeData.Variables)
            | VariableDeclaration(variablePattern, expr) ->
                let variablesType = getTypeFromExpression expr astData currentScopeData.Variables
                // Sometimes, a variable will declare a new scope (e.g. a lambda)
                // As such, we need to scan for that too
                match Ast.ScopedExpression(expr) with
                | ScopeDeclaration((scopeContent, scopeId), Some((variablePattern, expr))) ->
                    let convertedDeclaredVariables =
                        let exprType = getTypeFromExpression expr astData currentScopeData.Variables
                        convertAllToVariableData variablePattern exprType NonExportable

                    let predefinedVariables =
                        List.append currentScopeData.Variables convertedDeclaredVariables

                    indexAndAttachScope scopeId currentScopeData scopeContent (Some predefinedVariables)
                | ScopeDeclaration((scopeContent, scopeId), None) ->
                    indexAndAttachScope scopeId currentScopeData scopeContent (Some currentScopeData.Variables)
                | _ -> ()

                let variablesData =
                    convertAllToVariableData variablePattern variablesType NonExportable

                addVariables currentScopeData variablesData
            | NonDeclaratory -> ()

    and indexAndAttachScope
        (newScopeId: ScopeId)
        (currentScopeData: ResolvedAst.ScopeData)
        (scopeContent: Ast.ScopeContent)
        (existingVariables': (IdentifierData list) option)
        =
        // Create the scope
        let thisScope = makeScope newScopeId existingVariables'
        // Index it
        indexScope scopeContent thisScope
        // Attach it to the above (current) scope
        addToAboveScope currentScopeData thisScope

    for astConstruct in ast do
        match astConstruct with
        | Ast.ToplevelDeclaration.Method(_,
                                         _,
                                         (UnresolvedMethodReceiverParameter((typeName, _ as recvTypeInfo), _),
                                          parameters),
                                         _,
                                         (scopeContent, scopeId)) ->
            let mappedParameters =
                parameters
                |> List.append [ (false, UnknownType(recvTypeInfo), typeName) ]
                |> List.map (fun (_, t, identiferName) ->
                    { VarType = t
                      VarName = identiferName
                      ExportStatus = NonExportable })

            let thisScope = makeScope scopeId (Some mappedParameters)
            indexScope scopeContent thisScope
            toplevelScopes <- Misc.listAdd toplevelScopes thisScope
        | Ast.ToplevelDeclaration.Function(_, _, _, parameters, _, (scopeContent, scopeId)) ->
            let mappedParameters =
                parameters
                |> List.map (fun (_, t, identiferName) ->
                    { VarType = t
                      VarName = identiferName
                      ExportStatus = NonExportable })

            let thisScope = makeScope scopeId (Some mappedParameters)
            indexScope scopeContent thisScope
            toplevelScopes <- Misc.listAdd toplevelScopes thisScope

        | Ast.Const(_)
        | (Ast.Alias(_) | Ast.WrapperType(_) | Ast.Const(_))
        | Ast.Import(_)
        | Ast.ToplevelDeclaration.Interface(_)
        | Ast.ToplevelDeclaration.Struct(_) -> ()

    toplevelScopes


 *)
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
    //let mutable (structReferences: StructReference list) = []
    //let mutable (interfaceRefereces: InterfaceReference list) = []

    let mutable (methods: ( (* CustomTypeIdentifier *  *) Ast.MethodData) list) = []


    (* let rec getStructFields (structs:Ast.StructData list) ((_, _, _, structConstructs):Ast.StructData): Ast.StructFieldData list =
        let mutable (fields: Ast.StructFieldData list) = []
        for construct in structConstructs do
            match construct with
            | Ast.StructConstruct.Field(_, fieldData, _) -> 
                fields <- Misc.listAdd fields fieldData
            | Ast.StructConstruct.EmbeddedStruct(Struct structs embeddedStructData) ->
                fields <- List.append fields (getStructFields structs embeddedStructData)
        fields
        
    let rec getInterfaceMethods (interfaces:Ast.InterfaceData list) ((_, _, _, interfaceConstructs):Ast.InterfaceData): Ast.InterfaceMethodData list =
        let mutable (methods: Ast.InterfaceMethodData list) = []
        for construct in interfaceConstructs do
            match construct with
            | Ast.InterfaceConstruct.Method(data) -> 
                methods <- Misc.listAdd methods data
            | Ast.EmbeddedInterface(Interface interfaces interfaceData) ->
                methods <- List.append methods (getInterfaceMethods interfaces interfaceData)
        methods *)


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
(* 
    let mutable (customTypes: Ast.TypeDeclarationData list) = []

    // Populate custom types
    for astConstruct in ast do
        match astConstruct with
        | TypeDeclaration(typeDeclarationData) -> customTypes <- Misc.listAdd customTypes typeDeclarationData
        | _ -> () *)


    (* // Verify that all embedded types (supposed structs and interfaces) indeed exist
    structFields
    |> Misc.satisfiesAllOrError (fst >> structExists) (fun (nonexistantStructName, _) -> $"Struct '{nonexistantStructName}' does not exist")
    |> ignore

    interfaceMethods
    |> Misc.satisfiesAllOrError (fst >> interfaceExists) (fun (nonexistantInterfaceName, _) -> $"Interface '{nonexistantInterfaceName}' does not exist")
    |> ignore *)


    let mutable (astData: Ast.ASTData) =
        { Methods = methods |> List.map(fun ((_, methodName, _, _, _) as d)-> methodName, d) |> Map.ofList
          ImportedModules = importedModuleIdentifiers |> List.map(fun ((_, identifierName) as d) -> identifierName, d) |> Map.ofList
          Functions = functions |> List.map(fun ((_, functionName, _, _, _, _) as d)-> functionName, d) |> Map.ofList
          Interfaces = interfaces |> List.map(fun ((_, interfaceName, _, _) as d)-> interfaceName, d) |> Map.ofList
          WrapperTypes = wrapperTypes |> List.map(fun ((_, wrapperTypeName, _, _) as d)-> wrapperTypeName, d) |> Map.ofList
          Structs = structs |> List.map(fun ((_, structName, _, _) as d)-> structName, d) |> Map.ofList
          Consts = consts |> List.map(fun ((_, name, _) as d) -> name, d) |> Map.ofList
          RawAST = ast
          Aliases = aliases |> List.map(fun ((_, aliasName, _, _) as d)-> aliasName, d) |> Map.ofList }


    astData

(* // For use in the function that makes ResolvedAST
let (|AttributeDeclarer|) (input:ResolvedAst.ToplevelDeclaration) =



    let getAllAttributes (input:ResolvedAst.ConstructCapableOfHavingAttribute): ResolvedAst.AttributeData list = 
        let mutable (attributes: ResolvedAst.AttributeData list) = []
        match input with
        | ResolvedAst.ConstructCapableOfHavingAttribute.Interface({Name = interfaceName; Interfaces = embeddedInterfaces; MethodFields = methodFields; Structs = embeddedStructs}) as c ->
            // Add the name of the inteface
            for embeddedInterface in embeddedInterfaces do
                attributes <- Misc.listAdd attributes ({
                BaseType = c
                Name = embeddedInterface.Name
                })
                // Add the attributes of the interface that it inherits
                //for embeddedAttributeInEmbeddedInterface in embeddedInterface
                attributes <- Misc.listAdd attributes  
            attributes


    match input with
    | ResolvedAst.ToplevelDeclaration.Import(data) ->
        ResolvedAst.ModuleImport(data)
    | ResolvedAst.ToplevelDeclaration.Interface(interfaceData) ->
        ResolvedAst.Attribute({ BaseType = ResolvedAst.ConstructCapableOfHavingAttribute.Interface({Interfaces = interfaces; Methods})})
 *)
(* let mkResolvedASTData (ast: ResolvedAst.ToplevelDeclaration list) : ResolvedAst.ResolvedASTData =

    let mutable (functions: ResolvedAst.FunctionData list) = []
    let mutable (structs: ResolvedAst.StructData list) = []
    let mutable (interfaces: ResolvedAst.InterfaceData list) = []
    let mutable (consts: IdentifierData list) = []
    let mutable (wrapperTypes: ResolvedAst.WrapperTypeData list) = []
    let mutable (imported: ResolvedAst.ImportData list) = []
    let mutable (aliases: ResolvedAst.AliasData list) = []
    //let mutable (structReferences: StructReference list) = []
    //let mutable (interfaceRefereces: InterfaceReference list) = []

    let mutable (methods: ( (* CustomTypeIdentifier *  *) ResolvedAst.MethodData) list) = []

    (* let rec getStructFields (structs:Ast.StructData list) ((_, _, _, structConstructs):Ast.StructData): Ast.StructFieldData list =
        let mutable (fields: Ast.StructFieldData list) = []
        for construct in structConstructs do
            match construct with
            | Ast.StructConstruct.Field(_, fieldData, _) -> 
                fields <- Misc.listAdd fields fieldData
            | Ast.StructConstruct.EmbeddedStruct(Struct structs embeddedStructData) ->
                fields <- List.append fields (getStructFields structs embeddedStructData)
        fields
        
    let rec getInterfaceMethods (interfaces:Ast.InterfaceData list) ((_, _, _, interfaceConstructs):Ast.InterfaceData): Ast.InterfaceMethodData list =
        let mutable (methods: Ast.InterfaceMethodData list) = []
        for construct in interfaceConstructs do
            match construct with
            | Ast.InterfaceConstruct.Method(data) -> 
                methods <- Misc.listAdd methods data
            | Ast.EmbeddedInterface(Interface interfaces interfaceData) ->
                methods <- List.append methods (getInterfaceMethods interfaces interfaceData)
        methods *)


    // Populate lists
    for astConstruct in ast do
        match astConstruct with
        | ResolvedAst.ToplevelDeclaration.Const({ExportedStatus = exportedStatus; IdentifierName = name; Expression = constExpr}) ->
            let varData =
                { VarName = name
                  VarType = getTypeFromResolvedConst constExpr consts
                  ExportStatus = Exportable exportedStatus }

            consts <- Misc.listAdd consts varData
        | ResolvedAst.ToplevelDeclaration.Function(data) -> functions <- Misc.listAdd functions data
        | ResolvedAst.ToplevelDeclaration.Struct(data) -> structs <- Misc.listAdd structs data
        (* for construct in constructs do
                structFields <- Misc.listAdd structFields (structName, construct) *)
        | ResolvedAst.ToplevelDeclaration.Interface(data) ->
            interfaces <- Misc.listAdd interfaces data
        (* for construct in constructs do
                interfaceMethods <- Misc.listAdd interfaceMethods (interfaceName, construct) *)
        | ResolvedAst.ToplevelDeclaration.Import(data) ->
            imported <- Misc.listAdd imported data
        | ResolvedAst.ToplevelDeclaration.Method(data) ->
            methods <- Misc.listAdd methods data
        | ResolvedAst.ToplevelDeclaration.WrapperType(data) -> wrapperTypes <- Misc.listAdd wrapperTypes data
        | ResolvedAst.ToplevelDeclaration.Alias(data) -> aliases <- Misc.listAdd aliases data

    (* let mutable (customTypes: Ast.TypeDeclarationData list) = []

    // Populate custom types
    for astConstruct in ast do
        match astConstruct with
        | TypeDeclaration(typeDeclarationData) -> customTypes <- Misc.listAdd customTypes typeDeclarationData
        | _ -> () *)


    (* // Verify that all embedded types (supposed structs and interfaces) indeed exist
    structFields
    |> Misc.satisfiesAllOrError (fst >> structExists) (fun (nonexistantStructName, _) -> $"Struct '{nonexistantStructName}' does not exist")
    |> ignore

    interfaceMethods
    |> Misc.satisfiesAllOrError (fst >> interfaceExists) (fun (nonexistantInterfaceName, _) -> $"Interface '{nonexistantInterfaceName}' does not exist")
    |> ignore *)


    let mutable (resolvedAstData: ResolvedAst.ResolvedASTData) =
        { Imported = imported
          Functions = functions
          Interfaces = interfaces
          WrapperTypes = wrapperTypes
          Structs = structs
          Consts = consts
          BuiltInFunctions = builtInFunctions
          Aliases = aliases }


    resolvedAstData
 *)





(* let resolveAst (ast:Ast.AST) (astData:Ast.ASTData): ResolvedAst.ToplevelDeclaration list = 
    
    let scopes = indexScopes ast astData


    let getStruct (structName:string) = 
        astData.Structs
        |> List.tryFind(fun (_, name, _, _) -> Misc.compareLowercase structName structName)

    let getInterface (interfaceName:string) = 
        astData.Interfaces
        |> List.tryFind(fun (_, name, _, _) ->
            Misc.compareLowercase name interfaceName)
            
    let getWrapperType (wrapperType:string) = 
        astData.WrapperTypes
        |> List.tryFind (fun (_, wrapperTypeName, _, _) -> wrapperTypeName = wrapperType)

   // let rec resolveInterface 

    let rec resolveStruct ((exportedStatus, structName, generics, structConstructs):Ast.StructData) = 

        let (|AnotherStruct|Interface|Unresolvable|) (TypeEmbeddableInStruct(typeName, _)) = 
            match getInterface typeName with
            | Some x -> Interface x
            | None ->
                match getStruct typeName with
                | Some x -> AnotherStruct x
                | None -> Unresolvable typeName
        
        
        let mutable fields = []
        let mutable embeddedStructs = []
        let mutable embeddedInterfaces = []
        
        for construct in structConstructs do
            match construct with
            | Ast.StructConstruct.Field(exportedStatus, fieldData, tag') ->
                fields <- Misc.listAdd fields (ResolvedAst.StructFieldData(exportedStatus, fieldData, tag'))
            | Ast.StructConstruct.Embedded(AnotherStruct structData) ->
                embeddedStructs <- Misc.listAdd embeddedStructs (resolveStruct structData)
            | Ast.StructConstruct.Embedded(Interface interfaceData) ->
                embeddedInterfaces <- Misc.listAdd embeddedInterfaces (resolveInterface interfaceData)
            | Ast.StructConstruct.Embedded(Unresolvable typeName) ->
                Misc.error $"'{typeName}' is not an interface or a struct"

        {
            ExportedStatus = exportedStatus
            Name = structName
            Generics = generics
            Fields = fields
            EmbeddedStructs = embeddedStructs
            EmbeddedInterfaces = embeddedInterfaces
        }:ResolvedAst.StructData

    and resolveInterface ((exportedStatus, interfaceName, generics, interfaceConstructs):Ast.InterfaceData) = 

        let (|Struct|AnotherInterface|Unresolvable|) (TypeEmbeddableInInterface(typeName, _)) = 
            match getInterface typeName with
            | Some x -> AnotherInterface x
            | None ->
                match getStruct typeName with
                | Some x -> Struct x
                | None -> Unresolvable

        let mutable methods = []
        let mutable embeddedStructs = []
        let mutable embeddedInterfaces = []

        for interfaceConstruct in interfaceConstructs do
            match interfaceConstruct with
            | Ast.InterfaceConstruct.Method(exportedStatus, methodName, parameters, rt) ->
                methods <- Misc.listAdd methods (ResolvedAst.InterfaceMethodData(exportedStatus, methodName, parameters, rt))
            | Ast.InterfaceConstruct.Embedded(Struct structData) ->
                embeddedStructs <- Misc.listAdd embeddedStructs (resolveStruct structData)
            | Ast.InterfaceConstruct.Embedded(AnotherInterface interfaceData) ->
                embeddedInterfaces <- Misc.listAdd embeddedInterfaces (resolveInterface interfaceData)
            | Ast.InterfaceConstruct.Embedded(Unresolvable typeName) ->
                Misc.error $"'{typeName}' is not an interface or a struct"

        {
            ExportedStatus = exportedStatus
            Name = interfaceName
            Generics = generics
            Interfaces = embeddedInterfaces
            Methods = methods
            Structs = embeddedStructs
        }:ResolvedAst.InterfaceData

    let resolveWrapperType ((exportedStatus, wrapperTypeName, generics, t):Ast.WrapperTypeData) =
        {
            ExportedStatus = exportedStatus
            Name = wrapperTypeName
            Generics = generics
            Type = t
        }: ResolvedAst.WrapperTypeData

    let toResolvedAst (astItem:Ast.ToplevelDeclaration) = 
        match astItem with
        | Ast.ToplevelDeclaration.Function(exportStatus, functionName, generics, parameters, returnType, (_, scopeId)) ->
            ResolvedAst.ToplevelDeclaration.Function({
                ExportedStatus = exportStatus
                Name = functionName
                Generics = generics
                Parameters = parameters
                ReturnType = returnType
                ScopeData = getToplevelScope scopes scopeId
            })
        | Ast.ToplevelDeclaration.Import(moduleName, identifierName) ->
            ResolvedAst.ToplevelDeclaration.Import({
                ModuleName = moduleName
                IdentifierName = identifierName
            })
        | Ast.ToplevelDeclaration.Struct(s) ->
            ResolvedAst.ToplevelDeclaration.Struct(resolveStruct s)
        | Ast.ToplevelDeclaration.Alias(exportedStatus, aliasName, t) ->
            ResolvedAst.ToplevelDeclaration.Alias({
                ExportedStatus = exportedStatus
                Name = aliasName
                Type = t
            })
        | Ast.ToplevelDeclaration.Const(exportStatus, identifierName, constExpression) ->
            
            let toResolvedConstExpression (input:Ast.ConstExpression) = 
                match input with
                | Ast.ConstExpression.Boolean(b) ->
                    ResolvedAst.ConstExpression.Boolean({
                        Value = b
                    })
                | Ast.ConstExpression.Float(isNeg, f, isImaginary) ->
                    ResolvedAst.ConstExpression.Float({
                        IsNegative = isNeg
                        Float = f
                        IsImaginary = isImaginary
                    })
                | Ast.ConstExpression.Identifier(identifierName) ->
                    ResolvedAst.ConstExpression.Identifier(identifierName)
                | Ast.ConstExpression.String(str) ->
                    ResolvedAst.ConstExpression.String(str)
                | Ast.ConstExpression.Number(isNegative, str, endsWithU, isImaginary) ->
                    ResolvedAst.ConstExpression.Number({
                        IsNegative = isNegative
                        StringRepresentation = str
                        EndsWithU = endsWithU
                        IsImaginary = isImaginary
                    })

            ResolvedAst.ToplevelDeclaration.Const({
                ExportedStatus = exportStatus
                IdentifierName = identifierName
                Expression = toResolvedConstExpression constExpression
            })
        | Ast.ToplevelDeclaration.Interface(data) ->
           ResolvedAst.ToplevelDeclaration.Interface (resolveInterface data)
        | Ast.ToplevelDeclaration.Method(exportedStatus, methodName, (unresolvedReceiverType, parameters), returnType, (_, scopeId)) ->
            let toMethodReceiverType ((UnresolvedMethodReceiverType(typeName, receiverGenerics))) = 
                match getWrapperType typeName, getStruct typeName with
                | Some(exportedStatus, wrapperTypeName, generics, t), None ->
                    ResolvedAst.WrapperTypeReceiver (({
                        ExportedStatus = exportedStatus
                        Name = wrapperTypeName
                        Generics = generics
                        Type = t
                    }: ResolvedAst.WrapperTypeData), receiverGenerics)
                | None, Some(structData) ->
                    ResolvedAst.StructReceiver (resolveStruct structData, receiverGenerics)
                | _, _ -> Misc.error $"Type ''"

            ResolvedAst.ToplevelDeclaration.Method({
                ExportedStatus = exportedStatus
                Name = methodName
                ReceiverType = toMethodReceiverType unresolvedReceiverType
                Parameters = parameters
                ReturnType = returnType
                ScopeData = getToplevelScope scopes scopeId
            })
        | Ast.ToplevelDeclaration.WrapperType(wrapperTypeData) ->
            ResolvedAst.ToplevelDeclaration.WrapperType(resolveWrapperType wrapperTypeData)

    ast
    |> List.map (fun a -> toResolvedAst a) *)

// The below is the above, just changed




(* let toTranspiled (astData: Ast.ASTData) (toplevelResolved: ResolvedAst.ToplevelDeclaration) =
    match toplevelResolved with
    | ResolvedAst.ToplevelDeclaration.Alias({ ExportedStatus = es
                                              Name = name
                                              Type = t }) ->
        $"type {Misc.adjustName es name} = {transpileTypeName astData t}"
    | ResolvedAst.ToplevelDeclaration.Const({ ExportedStatus = es
                                              IdentifierName = name
                                              Expression = constExpr }) ->
        $"const {Misc.adjustName es name} = {transpileConstExpression constExpr}" *)

(* /// This function checks both the .GetAttribute() method on the respective type,
/// AND all of the separately-defined methods. 
let getAccessorReference (previousReferenceType:ResolvedAst.AccessorSupportedType) (context:ResolvedAst.Context) (nextReferenceName:string) = 
    let fromElementType' = 
        match previousReferenceType with
        | ResolvedAst.AccessorSupportedType.InterfaceType(interfaceTypeData) ->
            context.ResolvedDeclarations.Interfaces
            |> Map.find interfaceTypeData.InterfaceName
            |> _.GetAttribute(nextReferenceName)
        | ResolvedAst.AccessorSupportedType.WrapperType(_) -> 
            Error [$"Named types do not support attributes except for methods"] 
        | ResolvedAst.AccessorSupportedType.StructType(structTypeData) ->
            context.ResolvedDeclarations.Structs
            |> Map.find structTypeData.StructName
            |> _.GetAttribute(nextReferenceName)

    let methodReceiverType = 
        match previousReferenceType with
        | ResolvedAst.AccessorSupportedType.InterfaceType(interfaceTypeData) ->
            Error [$"Interfaces, like '{interfaceTypeData.InterfaceName}', do not support methods"]
        | ResolvedAst.AccessorSupportedType.WrapperType(wrapperTypeData) ->
            Ok <| ResolvedAst.MethodReceiverType.WrapperTypeReceiver(Map.find wrapperTypeData.WrapperTypeName context.ResolvedDeclarations.WrapperTypes, wrapperTypeData.GenericsInstantiation)
        | ResolvedAst.AccessorSupportedType.StructType(structTypeData) ->
            Ok <| ResolvedAst.MethodReceiverType.StructReceiver(Map.find structTypeData.StructName context.ResolvedDeclarations.Structs, structTypeData.GenericsInstantiation)

    let fromMethod' =
        match methodReceiverType with
        | Ok t ->
            context.ResolvedDeclarations.Methods
            |> Map.tryFind (t, nextReferenceName)
            |> Option.map (fun x -> ResolvedAst.AccessorItemWithoutCallData.Method(Declaration = x))
        | Error e ->
            None
    
    let previousReferenceTypeName = 
        match previousReferenceType with
        | ResolvedAst.AccessorSupportedType.InterfaceType(interfaceTypeData) ->
            interfaceTypeData.InterfaceName
        | ResolvedAst.AccessorSupportedType.WrapperType(wrapperTypeTypeData) -> 
            wrapperTypeTypeData.WrapperTypeName
        | ResolvedAst.AccessorSupportedType.StructType(structTypeData) ->
            structTypeData.StructName
        

    fromMethod'
    |> Option.orElse (Result.toOption fromElementType')
    |> Misc.fromOptionToResult [$"Type '{previousReferenceTypeName}' does not have attribute or method named '{nextReferenceName}'"]
 *)    

//    | ResolvedAst.Member.MemberDerivedFromModule(attributeName), callData' ->
//        Ok <| ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = attributeName, CallData = callData')
    




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

    (* let (|CallableAccessor|_|) (accessor:ResolvedAst.AccessorItemWithoutCallData) = 
    match accessor with
    | ResolvedAst.AccessorItemWithoutCallData.InterfaceMethod(_) -> true
    | ResolvedAst.AccessorItemWithoutCallData.Method(_) -> true
    | ResolvedAst.AccessorItemWithoutCallData.StructField({Type = ResolvedAst.ResolvedType.FunctionType(_)}) -> true
    | ResolvedAst.AccessorItemWithoutCallData.StructField({Type = _}) -> false
    | ResolvedAst.AccessorItemWithoutCallData.InterfaceDeclaration(_) -> false
    | ResolvedAst.AccessorItemWithoutCallData.StructDeclaration(_) -> false *)