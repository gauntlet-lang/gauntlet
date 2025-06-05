(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

module Resolvers.BaseResolvers

open Types
open Types.Base
open Utils
open FsToolkit.ErrorHandling
open Utils.Ast
open Misc

let (|CallableType|_|) (inputType:ResolvedAst.ResolvedType) = 
    // Even though there is only one type that is callable, I have put it in a
    // AP so that, should there be more callable types in the future,
    // I can just change it though here
    match inputType with
    | ResolvedAst.ResolvedType.FunctionType({Parameters = p; ReturnType = rt}) -> Some {|
        Parameters = p
        ReturnType = rt
        |}
    | _ -> None


/// Takes an unresolved type and, if it exists, maps it to it's "resolved" DU
let rec tryResolveType (astData: Ast.ASTData) (surroundingGenerics:Generics) (input: UnresolvedType) : Result<ResolvedAst.ResolvedType, string list> =

    let (|StructReference|EnumReference|InterfaceReference|WrapperTypeReference|AliasReference|GenericReference|Unresolved|) (typeName: string) =
        match typeName with
        | AstInterface astData data -> InterfaceReference data
        | AstStruct astData data -> StructReference data
        | AstWrapperType astData data -> WrapperTypeReference data
        | AstAlias astData data -> AliasReference data
        | Enum astData data -> EnumReference data
        | tn ->
            (* print "List:"
            List.iter print surroundingGenerics
            print "tn:"
            print tn
            print "Result:" *)
            //print (List.tryFind (fun (_, genericTypeName) -> tn = genericTypeName) surroundingGenerics)
            match List.tryFind (fun (_, genericTypeName) -> tn = genericTypeName) surroundingGenerics with
            | Some x -> GenericReference x
            | None -> Unresolved tn

    match input with
    | SliceType(t) ->
        tryResolveType astData surroundingGenerics t
        |> Result.map (fun r -> ResolvedAst.ResolvedType.SliceType({ Type = r }))
    | AnyType -> ResolvedAst.ResolvedType.AnyType |> Ok
    | ArrayType(length, t) ->
        tryResolveType astData surroundingGenerics t
        |> Result.map (fun r -> ResolvedAst.ResolvedType.ArrayType({ Length = length; Type = r }))
    | BooleanType -> ResolvedAst.ResolvedType.BooleanType |> Ok
    | EnumType -> Ok ResolvedAst.ResolvedType.EnumType
    | IntegerType(ByteType) -> ResolvedAst.ResolvedType.IntegerType(ByteType) |> Ok
    | ChanType(channelType, t) ->
        tryResolveType astData surroundingGenerics t
        |> Result.map (fun r -> ResolvedAst.ResolvedType.ChanType(channelType, r))
    | IntegerType(Complex128Type) -> ResolvedAst.ResolvedType.IntegerType(Complex128Type) |> Ok
    | IntegerType(Complex64Type) -> ResolvedAst.ResolvedType.IntegerType(Complex64Type) |> Ok
    | ErrorType -> ResolvedAst.ResolvedType.ErrorType |> Ok
    | IntegerType(Float32Type) -> ResolvedAst.ResolvedType.IntegerType(Float32Type) |> Ok
    | IntegerType(Float64Type) -> ResolvedAst.ResolvedType.IntegerType(Float64Type) |> Ok
    | FunctionType(parameterTypes, rt) ->
        result {
            let! mappedParams =
                parameterTypes
                |> List.map (fun (hasEllipsis, t) ->
                    tryResolveType astData surroundingGenerics t
                    |> Result.map (fun r ->
                        ({ HasEllipsis = hasEllipsis
                           Type = r })
                        : ResolvedAst.FunctionTypeParameter))
                |> List.sequenceResultA
                |> Result.mapError flattenList

            let! resolvedReturnType = tryResolveType astData surroundingGenerics rt

            return
                ResolvedAst.ResolvedType.FunctionType(
                    { Parameters = mappedParams
                      ReturnType = resolvedReturnType }
                )
        }

    | ImportedType(moduleName, typeName) -> ResolvedAst.ResolvedType.ImportedType({| ModuleName = moduleName; TypeName =typeName |}) |> Ok
    | IntegerType(Int16Type) -> ResolvedAst.ResolvedType.IntegerType(Int16Type) |> Ok
    | IntegerType(Int32Type) -> ResolvedAst.ResolvedType.IntegerType(Int32Type) |> Ok
    | IntegerType(Int64Type) -> ResolvedAst.ResolvedType.IntegerType(Int64Type) |> Ok
    | IntegerType(Int8Type) -> ResolvedAst.ResolvedType.IntegerType(Int8Type) |> Ok
    | IntegerType(IntType) -> ResolvedAst.ResolvedType.IntegerType(IntType) |> Ok
    | MapType(keyType, valueType) ->
        result {
            let! resolvedKeyType = tryResolveType astData surroundingGenerics keyType
            let! resolvedValueType = tryResolveType astData surroundingGenerics valueType
            return ResolvedAst.ResolvedType.MapType({KeyType = resolvedKeyType; ValueType = resolvedValueType})
        }
    | NullType -> ResolvedAst.ResolvedType.NullType |> Ok
    | QualifiedType(modName, tName) -> Ok <| ResolvedAst.ResolvedType.QualifiedType({ModuleName = modName; TypeName = tName})
    | PointerType(t) -> tryResolveType astData surroundingGenerics t |> Result.map (fun r -> ResolvedAst.ResolvedType.PointerType(r))
    | StringType -> ResolvedAst.ResolvedType.StringType |> Ok
    | TupleType(types) ->
        types
        |> List.map (tryResolveType astData surroundingGenerics)
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
        |> Result.map ResolvedAst.ResolvedType.TupleType
    | IntegerType(Uint16Type) -> ResolvedAst.ResolvedType.IntegerType(Uint16Type) |> Ok
    | IntegerType(Uint32Type) -> ResolvedAst.ResolvedType.IntegerType(Uint32Type) |> Ok
    | IntegerType(Uint64Type) -> ResolvedAst.ResolvedType.IntegerType(Uint64Type) |> Ok
    | IntegerType(Uint8Type) -> ResolvedAst.ResolvedType.IntegerType(Uint8Type) |> Ok
    | UnknownType -> ResolvedAst.ResolvedType.UnknownType |> Ok
    | UnitType -> ResolvedAst.ResolvedType.UnitType |> Ok
    | PendingResolutionType(StructReference (exportedStatus, structName, _, _), genericsInstantiationData) ->
        tryResolveGenericsInstantiation astData surroundingGenerics genericsInstantiationData
        |> Result.map (fun r -> ResolvedAst.ResolvedType.StructType({ExportedStatus = exportedStatus; GenericsInstantiation = r; StructName = structName }))
        
    | PendingResolutionType(InterfaceReference (exportedStatus, interfaceName, _, _), genericsInstantiationData) ->
        tryResolveGenericsInstantiation astData surroundingGenerics genericsInstantiationData
        |> Result.map (fun r -> ResolvedAst.ResolvedType.InterfaceType({ExportedStatus = exportedStatus; GenericsInstantiation = r; InterfaceName = interfaceName }))        
    | PendingResolutionType(WrapperTypeReference (exportedStatus, wrapperTypeName, _, _), genericsInstantiationData) ->
        tryResolveGenericsInstantiation astData surroundingGenerics genericsInstantiationData
        |> Result.map (fun r -> ResolvedAst.ResolvedType.WrapperTypeType({ExportedStatus = exportedStatus; WrapperTypeName = wrapperTypeName; GenericsInstantiation = r }))
    | PendingResolutionType(EnumReference _, _) ->
        Ok ResolvedAst.ResolvedType.EnumType
    | PendingResolutionType(AliasReference (exportedStatus, aliasName, _, _), genericsInstantiationData) ->
        tryResolveGenericsInstantiation astData surroundingGenerics genericsInstantiationData
        |> Result.map (fun r -> ResolvedAst.ResolvedType.AliasType({|AliasName = aliasName; ExportedStatus = exportedStatus; GenericsInstantiation = r|}))
    | PendingResolutionType(GenericReference (unresolvedConstraint, typeName), s) -> result {
        let! resolvedConstraint = tryResolveType astData surroundingGenerics unresolvedConstraint
        return ResolvedAst.ResolvedType.GenericType({Name = typeName; ConstraintType = resolvedConstraint})
        }
    | PendingResolutionType(Unresolved typeName, _) ->
        Error [$"Type '{typeName}' is not defined"]

and resolveParameter (astData: Ast.ASTData) (generics:Generics) ((hasEllipsis, t, identifierName): Parameter) : Result<ResolvedAst.Parameter, string list> =
    tryResolveType astData generics t 
    |> Result.map (fun r ->
        ({ Name = identifierName
           HasEllipsis = hasEllipsis
           Type = r }):ResolvedAst.Parameter)
    

and tryResolveGenericsInstantiation
    (astData: Ast.ASTData)
    (generics:Generics)
    (genericsInstantiationData: GenericsInstantiationData)
    : Result<ResolvedAst.GenericsInstantiationData, string list> =
    genericsInstantiationData
    |> List.map (tryResolveType astData generics) 
    |> List.sequenceResultA
    |> Result.mapError Misc.flattenList

and tryResolveOptionalGenericsInstantiation
    (astData: Ast.ASTData)
    (generics:Generics)
    (genericsInstantiationData': GenericsInstantiationData option) = result {
        let! resolvedGenericsInstaniationData =
            genericsInstantiationData'
            |> Misc.applyResultToOption (tryResolveGenericsInstantiation astData generics)
        return resolvedGenericsInstaniationData
    }
    

let tryResolveGenericData
    (astData: Ast.ASTData)
    ((unresolvedType, genericTypeName): Base.GenericData as generics)
    : Result<ResolvedAst.GenericData, string list> =
        // Putting [] since you can't contrain a generic type to another generic type.
        // Therefore, the function does not need generic data
     tryResolveType astData [] unresolvedType
     |> Result.map (fun r ->
        {
        Type = r
        TypeName = genericTypeName })


let tryResolveGenerics (astData:Ast.ASTData) (input:Base.Generics) = 
    input
    |> List.map (tryResolveGenericData astData)
    |> List.sequenceResultA
    |> Result.mapError Misc.flattenList
    
let tryResolveParameter (astData:Ast.ASTData) (generics:Generics) ((hasEllipsis, unresolvedType, identifierName):Base.Parameter):Result<ResolvedAst.Parameter, string list> = 
    tryResolveType astData generics unresolvedType
    |> Result.map(fun r -> 
        {
            HasEllipsis = hasEllipsis
            Type = r
            Name = identifierName
        }
    ) 



let (|IndexableType|_|) (input:ResolvedAst.ResolvedType) = 
    match input with
    | ResolvedAst.ResolvedType.ArrayType(data) -> Some {|InnerType = data.Type|}
    | ResolvedAst.ResolvedType.PointerType(ResolvedAst.ResolvedType.ArrayType data) -> Some {|InnerType = data.Type|}
    | ResolvedAst.ResolvedType.SliceType(data) -> Some {|InnerType = data.Type|}
    | ResolvedAst.ResolvedType.StringType -> Some {|InnerType = ResolvedAst.ResolvedType.StringType|}
    | ResolvedAst.ResolvedType.MapType(data) -> Some {|InnerType = data.ValueType|}
    | _ -> None

let (|SliceableType|_|) (input:ResolvedAst.ResolvedType) = 
    match input with
    | ResolvedAst.ResolvedType.SliceType(sliceTypeData) -> Some {|InnerType = sliceTypeData.Type|}
    | ResolvedAst.ResolvedType.StringType -> Some {|InnerType = ResolvedAst.ResolvedType.StringType|}
    | ResolvedAst.ResolvedType.ArrayType(arrayTypeData) -> Some {|InnerType = arrayTypeData.Type|}
    | _ -> None

let rec tryGetTypeFromExpression (declarations:ResolvedAst.ResolvedDeclarations) (input:ResolvedAst.Expression): Result<ResolvedAst.ResolvedType, string list> =
    
    match input with
    | ResolvedAst.Expression.Accessors(data) ->
        let lastAccessor = data.Accessors |> Misc.getLastItem
        Ok <| getTypeFromExprArgAccessor declarations lastAccessor
    | ResolvedAst.Expression.ConstantIdentifier(data) ->
        Ok <| getTypeFromConst declarations.Consts data.Reference.Expression
    | ResolvedAst.ArrayLiteral(x) -> Ok x.ItemsType
    | ResolvedAst.Char(_) -> Ok <| ResolvedAst.ResolvedType.IntegerType(IntType.Int32Type)
    | ResolvedAst.EnumCaseLiteral(_) -> Ok ResolvedAst.ResolvedType.EnumType
    | ResolvedAst.Boolean(_) -> ResolvedAst.ResolvedType.BooleanType |> Ok
    | ResolvedAst.ChannelRecieve(data) ->
        result {
            let! resolvedType = tryGetTypeFromExpression declarations data.Expression
            return ResolvedAst.ResolvedType.ChanType(ReceiveOnly, resolvedType)
        }
    | ResolvedAst.DereferenceOf(data) ->
        tryGetTypeFromVariable declarations data.ReferenceIdentifier
    | ResolvedAst.Float(data) ->
        Ok <| ResolvedAst.ResolvedType.IntegerType(Ast.getTypeFromFloat data.Float data.IsImaginary)
    | ResolvedAst.FunctionCall(data) ->
        getPossiblyCallableExpressionReturnType declarations data.Reference
        |> Result.map (fun r -> r.ReturnType)
    | ResolvedAst.Index(data) ->
        match tryGetTypeFromExpression declarations data.ReferenceExpr with
        | Ok (IndexableType d) -> Ok d.InnerType
        | Ok otherType -> Error [$"Type '{Ast.transpileType otherType}' is not indexable"]
        | e -> e
    | ResolvedAst.Lambda(data) ->
        Ok <| ResolvedAst.ResolvedType.FunctionType({
            Parameters = parametersToFunctionTypeParameter data.Parameters
            ReturnType = data.ReturnType
        })
    | ResolvedAst.MakeFunction(data) ->
        Ok data.Type
    | ResolvedAst.MapLiteral(data) ->
        Ok <| ResolvedAst.ResolvedType.MapType({KeyType = data.KeyType; ValueType = data.ValueType})
    (* | ResolvedAst.MapLookup(data) ->
        match tryGetTypeFromExpression declarations data.Reference with
        | Ok (ResolvedAst.ResolvedType.MapType mapData) -> Ok mapData.ValueType
        | Ok otherType -> Error [$"Type '{Ast.transpileType otherType}' cannot be used in a map lookup"]
        | e -> e *)
    | ResolvedAst.ModuleAccessor(_) ->
        Ok ResolvedAst.ResolvedType.UnknownType
    | ResolvedAst.WrapperTypeLiteral({WrapperType = {WrapperDeclaration = {Name = DeclaredWrapperType declarations wrappedTypeData}; GenericsInstantiation = genericsInstantiationData}}) ->
        Ok <| ResolvedAst.ResolvedType.WrapperTypeType({WrapperTypeName = wrappedTypeData.Name; GenericsInstantiation = genericsInstantiationData; ExportedStatus = wrappedTypeData.ExportedStatus})
    | ResolvedAst.NegatedExpression(_) ->
        Ok ResolvedAst.ResolvedType.BooleanType
    | ResolvedAst.NewFunction(data) ->
        Ok data.Type
    | ResolvedAst.Null -> Ok ResolvedAst.ResolvedType.NullType
    | ResolvedAst.Number(numData) ->
        Ast.getTypeFromNumber numData.StringRepresentation numData.Property
        |> ResolvedAst.ResolvedType.IntegerType
        |> Ok
    | ResolvedAst.OperatorSequence(data) ->
        tryGetTypeFromExpression declarations data.BaseExpression
    | ResolvedAst.Parentheses(data) ->
        tryGetTypeFromExpression declarations data.InnerExpression
    | ResolvedAst.PipeOperator(data) ->
        data.Pipes
        |> List.last
        |> function
           | ResolvedAst.PipeCall.AccessorCall(accessorData) -> Ok <| getTypeFromPipeArgAccessor declarations (List.last accessorData.Accessors)
           | ResolvedAst.PipeCall.FunctionCall({Reference = r}) -> 
                getPossiblyCallableExpressionReturnType declarations r
                |> Result.map (fun r -> r.ReturnType)
           | ResolvedAst.PipeCall.ModuleAccessorCall(_) ->
            Ok ResolvedAst.ResolvedType.UnknownType  
    | ResolvedAst.PointerTo(data) ->
        result {
            let! resolvedExpressionType = tryGetTypeFromVariable declarations data.ReferenceIdentifier
            return ResolvedAst.ResolvedType.PointerType(resolvedExpressionType)
        }
    | ResolvedAst.Slice(data) ->
        match tryGetTypeFromExpression declarations data.Reference with
        | Ok (SliceableType d) -> Ok d.InnerType
        | Ok (otherType) -> Error [$"Type '{Ast.transpileType otherType}' is not sliceable"]
        | e -> e
    | ResolvedAst.SliceLiteral(data) ->
        Ok <| ResolvedAst.ResolvedType.SliceType({Type = data.SliceItemsType})
    | ResolvedAst.String(_) -> Ok ResolvedAst.ResolvedType.StringType
    | ResolvedAst.StructLiteral({Struct = {StructDeclaration = {Name = DeclaredStruct declarations structData}; GenericsInstantiation = genericsInstantiationData}}) ->
        Ok <| ResolvedAst.ResolvedType.StructType({StructName = structData.Name; GenericsInstantiation = genericsInstantiationData; ExportedStatus = structData.ExportedStatus})
    | ResolvedAst.When(data) ->
        Ok data.ReturnType
    | ResolvedAst.TernaryOperator(data) ->
        tryGetTypeFromExpression declarations data.SuccessExpression
    | ResolvedAst.Tuple({Expressions = exprs}) ->
        result {
            let! exprTypes = 
                exprs
                |> List.map(tryGetTypeFromExpression declarations)
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList

            return ResolvedAst.ResolvedType.TupleType(exprTypes)
        }
    | ResolvedAst.TypeAssertion(data) ->
        Ok data.Type
    | ResolvedAst.TypeConversion(data) ->
        Ok data.Type
    | ResolvedAst.UnaryOperator(data) ->
        tryGetTypeFromExpression declarations data.BaseExpression
    | ResolvedAst.Unit ->
        Ok ResolvedAst.ResolvedType.UnitType
    | ResolvedAst.Variable(data) ->
        tryGetTypeFromVariable declarations data
    | ResolvedAst.ModuleReference({ModuleIdentifier = moduleIdentifier}) ->
        Ok <| ResolvedAst.ResolvedType.ModuleType(moduleIdentifier)
      
and tryGetTypeFromVariable (declarations:ResolvedAst.ResolvedDeclarations) (input:ResolvedAst.ScopedVariable) = 
    match input with
    | ResolvedAst.ScopedVariable.ExprVariable({Expression = expr}) -> tryGetTypeFromExpression declarations expr
    | ResolvedAst.ScopedVariable.ParameterVariable({Type = t}) -> Ok t
    | ResolvedAst.ScopedVariable.ZeroVariable({Type = t}) -> Ok t

/// This function is a 2-in-1 because in order to get the return type,
/// it must first verify that it, indeed, refers to a callable type
and getPossiblyCallableExpressionReturnType (declarations:ResolvedAst.ResolvedDeclarations) (input:ResolvedAst.PossiblyCallableExpression): Result<{| Parameters: list<ResolvedAst.FunctionTypeParameter>; ReturnType: ResolvedAst.ResolvedType |},list<string>> = 
    match input with
    | ResolvedAst.PossiblyCallableExpression.FunctionDeclaration({ReturnType = rt; Parameters = p}) ->
        Ok {|ReturnType = rt; Parameters = parametersToFunctionTypeParameter p|}
    | ResolvedAst.PossiblyCallableExpression.Parentheses(innerExpr) -> result {
        let! t = tryGetTypeFromExpression declarations innerExpr 
        return!
            match t with 
            | CallableType d -> Ok d
            | _ -> Error [$"Type '{Ast.transpileType t}' is not callable"]
        }
    | ResolvedAst.PossiblyCallableExpression.Var(varData) -> result {
        let! t = tryGetTypeFromVariable declarations varData
        return!
            match t with 
            | CallableType d -> Ok d
            | _ -> Error [$"Type '{Ast.transpileType t}' is not callable"]
        }
    | ResolvedAst.PossiblyCallableExpression.BuiltInFunction(data) ->
        let mappedParameters = parametersToFunctionTypeParameter data.Parameters
        Ok {| Parameters = mappedParameters; ReturnType = data.ReturnType |}

    
