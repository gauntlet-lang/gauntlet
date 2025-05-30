(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)



/// A myriad of functions (most of which are smart constructors) that convert
/// "unresolved" elements into their "resolved"
/// counterparts. These have to do with elements that are usually inside a scope
/// like an expression or statement
module Resolvers.ScopeResolvers

open Types
open BaseResolvers
open Utils
open FsToolkit.ErrorHandling
open Utils.Ast
open Misc

/// Resolves an "unresolved" expression. Check out 
/// `Types.ResolvedAst.Expression` to learn more.  
let rec tryResolveExpression
    ({ResolvedDeclarations = resolvedDeclarations; AstData = astData; ScopeVariables = scopeVariables}:ResolvedAst.Context as context)
    (inputExpr: Ast.Expression)
    : Result<ResolvedAst.Expression, string list> =
    match inputExpr with
    | Ast.Expression.ConstantIdentifier(data) -> tryResolveConstantIdentifier context data
    | Ast.Expression.FunctionCall(data) -> tryResolveFunctionCall context data
    | Ast.Expression.Accessors(data) -> tryResolveAccessorsExpressionVersion context data
    | Ast.Expression.ArrayLiteral(data) -> tryResolveArrayLiteral context data
    | Ast.Expression.Boolean(data) -> tryResolveBoolean data
    | Ast.Expression.ChannelRecieve(data) -> tryResolveChannelReceive context data
    | Ast.Expression.DereferenceOf(data) -> tryResolveDereferenceOf scopeVariables data
    | Ast.Expression.Float(data) -> tryResolveFloat data
    | Ast.Expression.Identifier(data) -> tryResolveIdentifier context data
    | Ast.Expression.Index(data) -> tryResolveIndex context data
    | Ast.Expression.Lambda(data) -> tryResolveLambda context data
    | Ast.Expression.MakeFunction(data) -> tryResolveMakeFunction context data 
    | Ast.Expression.MapLiteral(data) -> tryResolveMapLiteral context data
//    | Ast.Expression.MapLookup(data) -> tryResolveMapLookup context data
    | Ast.Expression.WrapperTypeLiteral(data) -> tryResolveWrapperTypeLiteral context data
    | Ast.Expression.NegatedExpression(data) -> tryResolveNegatedExpression context data
    | Ast.Expression.NewFunction(data) -> tryResolveNewFunction context astData data
    | Ast.Expression.Null -> tryResolveNull ()
    | Ast.Expression.Number(data) -> tryResolveNumber data
    | Ast.Expression.OperatorSequence(data) ->
        tryResolveOperatorSequence context data
    | Ast.Expression.Parentheses(data) -> tryResolveParentheses context data
    | Ast.Expression.PipeOperator(data) -> tryResolvePipeOperators context data
    | Ast.Expression.PointerTo(data) -> tryResolvePointerTo scopeVariables data
    | Ast.Expression.Slice(data) -> tryResolveSlice context data
    | Ast.Expression.SliceLiteral(data) -> tryResolveSliceLiteral context data
    | Ast.Expression.String(data) -> tryResolveString data
    | Ast.Expression.StructLiteral(data) -> tryResolveStructLiteral context data
    //| Ast.Expression.When(data) -> tryResolveSwitchCase context data
    | Ast.Expression.When(whenData) ->
        tryResolveWhen context whenData
    | Ast.Expression.TernaryOperator(data) ->
        tryResolveTernaryOperator context data
    | Ast.Expression.Tuple(data) ->
        tryResolveTuple context data
    | Ast.Expression.TypeAssertion(data) ->
        tryResolveTypeAssertion context data
    | Ast.Expression.TypeConversion(data) ->
        tryResolveTypeConversion context data
    | Ast.Expression.UnaryOperator(data) ->
        tryResolveUnaryOperator context data
    | Ast.Expression.Unit -> Ok ResolvedAst.Expression.Unit
    | Ast.Expression.Char(data) -> tryResolveChar data


//and tryResolveIota () = Ok <| ResolvedAst.Iota
and tryResolveConstantIdentifier (context:ResolvedAst.Context) (identifier:Ast.ConstantIdentifierData) = result {
    
    let! reference = 
        context.ResolvedDeclarations.Consts
        |> Map.tryFind identifier
        |> Misc.fromOptionToResult [$"Constant '{identifier}' does not exist"]

    return ResolvedAst.Expression.ConstantIdentifier({
        Identifier = identifier
        Reference = reference
    }:ResolvedAst.ConstantIdentifierData)
}

and tryResolveUnaryOperator (context:ResolvedAst.Context) ((expr, op, secondExpr'):Ast.UnaryOperatorData) = result {
    let! resolvedExpr = tryResolveExpression context expr
    let! resolvedSecondExpr' = 
        match secondExpr' with
        | Some secondExpr ->
            tryResolveExpression context secondExpr
            |> Result.map Some
        | None -> result {return None}
        
    return ResolvedAst.Expression.UnaryOperator({
        BaseExpression = resolvedExpr
        Operator = op
        SecondExpression = resolvedSecondExpr'
    })

}

and tryResolveTypeConversion (context:ResolvedAst.Context) ((unresolvedType, expression):Ast.TypeConversionData) = result {
    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType
    let! resolvedExpr = tryResolveExpression context expression
    return ResolvedAst.Expression.TypeConversion({
        Type = resolvedType
        Expression = resolvedExpr
    })
}

and tryResolveTypeAssertion (context:ResolvedAst.Context) ((identifierName, unresolvedType):Ast.TypeAssertionData) = result {
    let! resolvedIdentifierData = context.ScopeVariables.TryFindVariable(identifierName)
    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType
    return ResolvedAst.Expression.TypeAssertion({
        Identifier = resolvedIdentifierData
        Type = resolvedType
    })
}

and tryResolveTuple (context:ResolvedAst.Context) (exprs:Ast.TupleData) = result {
    let! resolvedExpressions = 
        exprs
        |> List.map (tryResolveExpression context)
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
    
    return ResolvedAst.Expression.Tuple({
        Expressions = resolvedExpressions
    })
}

and tryResolveTernaryOperator (context:ResolvedAst.Context) ((t, conditionExpr, successExpr, failureExpr):Ast.TernaryOperatorData) = result {
    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics t
    let! resolvedConditionExpression = tryResolveExpression context conditionExpr
    let! resolvedSuccessExpr = tryResolveExpression context successExpr
    let! failureExpr = tryResolveExpression context failureExpr
    return ResolvedAst.TernaryOperator({
        ExpressionType = resolvedType
        ConditionExpression = resolvedConditionExpression
        SuccessExpression = resolvedSuccessExpr
        FailureExpression = failureExpr
    })
}

and tryResolveSwitchCase (context:ResolvedAst.Context) (switchType:Ast.SwitchCaseType) = result {
    
    
    let! resolvedDefaultCase = 
        match switchType with
        | Ast.SwitchCaseType.TypeSwitch(_, _, Some (Ast.DefaultCase(rawScopeData)))
        | Ast.SwitchCaseType.ExpressionSwitch(_, _, Some (Ast.DefaultCase(rawScopeData))) ->
            tryResolveScope context [] [] [] rawScopeData
            |> Result.map fst
            |> Result.map Some
        | Ast.SwitchCaseType.ExpressionSwitch(_, _, None)
        | Ast.SwitchCaseType.TypeSwitch(_, _, None) -> Ok None

    let! resolvedSwitch =
        match switchType with
        | Ast.SwitchCaseType.ExpressionSwitch(expr, cases, _) -> result {
            let! resolvedExpression = tryResolveExpression context expr 
            let! resolvedCases =
                cases
                |> List.map(fun (expr, scopeData) -> result {
                    let! resolvedExpr = tryResolveExpression context expr
                    let! resolvedScope, _ = tryResolveScope context [] [] [] scopeData
                    return resolvedExpr, resolvedScope
                    }
                )
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            return (({
                SwitchExpression = resolvedExpression
                Cases = resolvedCases
                DefaultCase = resolvedDefaultCase
            }):ResolvedAst.SwitchExpressionCaseData) |> ResolvedAst.SwitchCaseType.SwitchExpression
            }  
        | Ast.SwitchCaseType.TypeSwitch(expr, cases, _) -> result {
            let! resolvedExpression = tryResolveExpression context expr
            let! resolvedCases = 
                cases
                |> List.map(fun (t, scopeData) -> result {
                    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics t
                    let! resolvedScope, _ = tryResolveScope context [] [] [] scopeData
                    return resolvedType, resolvedScope
                    }
                )
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            return (({
                Expression = resolvedExpression
                Cases = resolvedCases
                DefaultCase = resolvedDefaultCase
            }):ResolvedAst.SwitchTypeCaseData) |> ResolvedAst.SwitchCaseType.SwitchType
        }
            
        

    return ResolvedAst.SwitchCase(resolvedSwitch)
}

and tryResolveStructLiteral (context:ResolvedAst.Context) ((Base.UnresolvedStructReference(typeName, genericsInstantiationData), fields):Ast.StructLiteralData) = result {
    
    let! resolvedStruct =
        context.ResolvedDeclarations.Structs
        |> tryFindOrError typeName $"Struct '{typeName}' not found"
    let! resolvedGenericsInstaniationData = tryResolveGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiationData

    let! mappedFields = 
        fields
        |> List.map(fun (fieldName, expr) ->
            result {
                let! resolvedField =
                    resolvedStruct.GetMember(context.ResolvedDeclarations.Methods, fieldName)
                    |> fromOptionToResult [$"'Field {fieldName} does not exist on struct type '{resolvedStruct.Name}'"]
                let fieldExportStatus = 
                    match resolvedField with
                    | ResolvedAst.Member.InterfaceDeclaration({InterfaceDeclaration = {ExportedStatus = exportedStatus}})
                    | ResolvedAst.Member.InterfaceMethod({ExportedStatus = exportedStatus}) 
                    | ResolvedAst.Member.Method({ExportedStatus = exportedStatus}) 
                    | ResolvedAst.Member.StructDeclaration({StructDeclaration = {ExportedStatus = exportedStatus}})
                    | ResolvedAst.Member.StructField({ExportedStatus = exportedStatus}) -> exportedStatus
//                    | ResolvedAst.Member.MemberDerivedFromModule(_) -> Ok Base.ExportedStatus.Exported
                let! resolvedExpr = tryResolveExpression context expr
                return (({
                Name = fieldName
                Value = resolvedExpr
                ExportedStatus = fieldExportStatus
                }):ResolvedAst.StructLiteralField)
            }
            
            
        )
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList

    return ResolvedAst.Expression.StructLiteral({
        Struct = {
            StructDeclaration = resolvedStruct
            GenericsInstantiation = resolvedGenericsInstaniationData
        }
        Fields = mappedFields
    })
}

and tryResolveString (input:Ast.StringData) = result {
    return ResolvedAst.Expression.String(input)
}

and tryResolveSliceLiteral (context:ResolvedAst.Context) ((unresolvedType, exprs):Ast.SliceLiteralData) = result {
    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType 
    let! resolvedExprs = 
        exprs
        |> List.map (tryResolveExpression context) 
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
    
    return ResolvedAst.Expression.SliceLiteral({
        SliceItemsType = resolvedType
        Items = resolvedExprs
    })
}

and tryResolveSlice (context:ResolvedAst.Context) ((reference, startExpr, endExpr):Ast.SliceData) = result {
    let! mappedExpr = 
        match reference with
        | Ast.SliceableReference.ArrayLiteral(data) ->
            tryResolveArrayLiteral context data
        | Ast.SliceableReference.Identifier(identifierName) -> result {
            let! resolvedIdentifier = context.ScopeVariables.TryFindVariable(identifierName)
            return ResolvedAst.Variable resolvedIdentifier
            }
        | Ast.SliceableReference.SliceLiteral(data) ->
            tryResolveSliceLiteral context data
        | Ast.SliceableReference.String(data) ->
            tryResolveString data

    let! resolvedStartExpr = tryResolveExpression context startExpr
    let! resolvedEndExpr = tryResolveExpression context endExpr

    return ResolvedAst.Expression.Slice({
        Reference = mappedExpr
        StartIndex = resolvedStartExpr
        EndIndex = resolvedEndExpr
    })
}

and tryResolvePointerTo (scopeVariables:ResolvedAst.ScopeVariables) (input:Ast.PointerToData) = result {
    let! resolvedReference = scopeVariables.TryFindVariable(input)
    return ResolvedAst.Expression.PointerTo({
        ReferenceIdentifier = resolvedReference
    })
}

and tryResolveExprArgs (context:ResolvedAst.Context) (input:Ast.ExpressionCallArguments) = result {
    let! resolvedArguments = 
        input
        |> List.map(fun (arg, hasE) -> result {
            let! resolvedExpr = tryResolveExpression context arg
            return {|Argument = resolvedExpr; HasEllipsis = hasE|}
        })
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
    return resolvedArguments
}

and tryResolveOptionalExprArgs (context:ResolvedAst.Context) (input:Ast.ExpressionCallArguments option) = result {
    return!
        input
        |> applyResultToOption (tryResolveExprArgs context)
}


and tryResolvePipeArgs (context:ResolvedAst.Context) (input:Ast.PipeCallArguments) = result {
    let! (resolvedArguments: ResolvedAst.PipeCallArguments) = 
        input
        |> List.map(fun (arg, hasE) -> 
            match arg with
            | Ast.PipeFunctionCallArgument.Normal(expr) -> result {
                let! resolvedExpr = tryResolveExpression context expr
                return {|Argument = ResolvedAst.PipeFunctionCallArgument.Normal(resolvedExpr); HasEllipsis = hasE|}
                }
            | Ast.PipeFunctionCallArgument.Placeholder -> Ok {|Argument = ResolvedAst.PipeFunctionCallArgument.Placeholder; HasEllipsis = hasE|}
            )
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList

    return resolvedArguments
}

and tryResolvePipedArguments (context:ResolvedAst.Context) (input:Ast.PipeFunctionCallArgument) = 
    match input with
    | Ast.PipeFunctionCallArgument.Normal expr -> result {
            let! mappedExpr = tryResolveExpression context expr
            return ResolvedAst.PipeFunctionCallArgument.Normal mappedExpr
        }
    | Ast.PipeFunctionCallArgument.Placeholder ->
        Ok ResolvedAst.PipeFunctionCallArgument.Placeholder


and expressionAccessorsToModuleAccessors (context:ResolvedAst.Context) (input:list<Ast.AccessorItemExprArgs>) = result {
    let! resolvedAccessors = 
        input
        |> List.map (fun accessorData ->
            match accessorData with
            | Ast.AccessorItemExprArgs.FieldAccessor(fieldName, genericsInstantiation', exprArgs') -> result {
                let! resolvedArgs = tryResolveOptionalExprArgs context exprArgs'
                let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
                return
                    ({
                        AttributeName = fieldName
                        Arguments = resolvedArgs 
                        GenericsInstantiation = resolvedGenericsInstantiation'
                    }:ResolvedAst.ModuleAccessorItemExprArgs)
                }
            | Ast.AccessorItemExprArgs.MethodAccessor(methodData, genericsInstantiation', exprArgs') -> result {
                let! resolvedArgs' = tryResolveOptionalExprArgs context exprArgs'
                let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
                return
                    ({
                        AttributeName = methodData
                        Arguments = resolvedArgs'
                        GenericsInstantiation = resolvedGenericsInstantiation'
                    }:ResolvedAst.ModuleAccessorItemExprArgs)
                }
                )
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
    return resolvedAccessors
}
 


and pipeFunctionCallArgumentAccessorsToModuleAccessors (context:ResolvedAst.Context) (input:list<Ast.AccessorItemPipeArgs>) = result {
    let! resolvedAccessors = 
        input
        |> List.map (fun accessorData ->
            match accessorData with
            | Ast.AccessorItemPipeArgs.FieldAccessor(fieldName, genericsInstantiation', pipeArgs) -> result {
                let! resolvedArgs = tryResolvePipeArgs context pipeArgs
                let! resolvedGenericsInstantiation = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'

                return
                    ({
                        AttributeName = fieldName
                        Arguments = resolvedArgs
                        GenericsInstantiation = resolvedGenericsInstantiation
                    }:ResolvedAst.ModuleAccessorItemPipeArgs)
                }
            | Ast.AccessorItemPipeArgs.MethodAccessor(methodData, genericsInstantiation', pipeArgs) -> result {
                
                let! resolvedArgs = tryResolvePipeArgs context pipeArgs
                let! resolvedGenericsInstantiation = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'

                return
                    ({
                        AttributeName = methodData
                        Arguments = resolvedArgs
                        GenericsInstantiation = resolvedGenericsInstantiation
                    }:ResolvedAst.ModuleAccessorItemPipeArgs)
                }
                )
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
    return resolvedAccessors
}


and tryResolveAccessorsExpressionVersion (context:ResolvedAst.Context) ((baseExpression, accessors):Ast.ExprArgsAccessorsData as data) = result {
    // Could be a module call. Check for that.
    let! resolvedBaseExpression = tryResolveExpression context baseExpression
    match resolvedBaseExpression with
    | ResolvedAst.ModuleReference(moduleReferenceData) ->
        // If it's a module call, which it is, resolve everything.
        let! resolvedAccessors = expressionAccessorsToModuleAccessors context accessors
        return ResolvedAst.Expression.ModuleAccessor {
            IdentifierName = moduleReferenceData.ModuleIdentifier
            Accessors = resolvedAccessors
        }
    | _ ->
        let! resolvedAccessors = tryResolveIndividualExprArgAccessors context data

        return
                ResolvedAst.Expression.Accessors {
                    BaseExpression = resolvedBaseExpression
                    Accessors = resolvedAccessors
                    }
    } 


and tryResolvePipeOperators (context:ResolvedAst.Context) ((expr, pipes):Ast.PipeOperatorData) = result {
    let! resolvedBaseExpression = tryResolveExpression context expr


    let! mappedPipes = 
        pipes
        |> List.map (fun pipe ->
            match pipe with
            | Ast.PipeCall.FunctionCall(possibleFunctionReference, genericsInstantiationData', arguments) -> result {
                let! resolvedOptionalGenericsInstantiation = Misc.applyResultToOption (tryResolveGenericsInstantiation context.AstData context.ScopeGenerics) genericsInstantiationData'

                (* let numberOfUnderscoreParameters = 
                    arguments
                    |> List.filter (fun (arg, _) ->
                        match arg with
                        | Ast.PipeFunctionCallArgument.Normal(_) -> false
                        | Ast.PipeFunctionCallArgument.Placeholder -> true)
                        |> _.Length

                do! Result.requireTrue ["When using pipe operator, functions cannot have more than one underscore as a parameter"] (numberOfUnderscoreParameters = 1) *)
                
                let! mappedArguments = 
                    arguments
                    |> List.map(fun (arg, hasEllipsis) ->
                        let mappedArg =
                            match arg with
                            | Ast.PipeFunctionCallArgument.Normal(expr) -> result {
                                let! r = tryResolveExpression context expr
                                return ResolvedAst.PipeFunctionCallArgument.Normal(r)
                                }
                                
                            | Ast.PipeFunctionCallArgument.Placeholder -> Ok ResolvedAst.PipeFunctionCallArgument.Placeholder
                        mappedArg
                        |> Result.map (fun r -> {|Argument = r; HasEllipsis = hasEllipsis|})
                        
                    )
                    |> List.sequenceResultA
                    |> Result.mapError Misc.flattenList
                let! mappedCallableDeclaration = 
                    match possibleFunctionReference with
                    | Ast.PossibleFunctionReference.Parentheses(expr) ->
                        tryResolveExpression context expr
                        |> Result.map(fun r -> ResolvedAst.PossiblyCallableExpression.Parentheses(r))
                    | Ast.PossibleFunctionReference.Identifier(possibleCallableIdentifier) ->
                        tryResolvePossibleCallableIdentifier context.ResolvedDeclarations context.ScopeVariables possibleCallableIdentifier

                return
                    (({
                        Arguments = mappedArguments
                        Reference = mappedCallableDeclaration
                        GenericsInstantiation = resolvedOptionalGenericsInstantiation
                    }):ResolvedAst.FunctionPipeCallData)
                    |> ResolvedAst.PipeCall.FunctionCall
                }
            | Ast.PipeCall.AccessorCall((baseExpr, accessors) as data) -> result {
                let! resolvedBaseExpression =
                    tryResolveExpression context baseExpr


                return!
                    match resolvedBaseExpression with
                    // If it's referencing a module, then it must be converted to it
                    | ResolvedAst.Expression.ModuleReference(moduleReferenceData) -> result {
                        let! resolvedAccessors = pipeFunctionCallArgumentAccessorsToModuleAccessors context accessors
                        return ResolvedAst.PipeCall.ModuleAccessorCall {
                            IdentifierName = moduleReferenceData.ModuleIdentifier
                            Accessors = resolvedAccessors}
                        }
                    // Otherwise, treat it as a regular accessor call
                    | _ -> result {
                        let! resolvedAccessors =
                            tryResolveIndividualPipeArgAccessors context data
                            
                        return
                            ResolvedAst.PipeCall.AccessorCall({
                                BaseExpression = resolvedBaseExpression
                                Accessors = resolvedAccessors
                            })
                    }
                }
            )
       |> List.sequenceResultA
       |> Result.mapError Misc.flattenList
    return
        ResolvedAst.Expression.PipeOperator({
            BaseExpression = resolvedBaseExpression
            Pipes = mappedPipes
        })
}

and tryResolveParentheses (context:ResolvedAst.Context) (input: Ast.ParenthesesData) = result {
    let! resolvedExpr = tryResolveExpression context input
    return ResolvedAst.Expression.Parentheses({
        InnerExpression = resolvedExpr
    })
}

and tryResolveOperatorSequence (context:ResolvedAst.Context) ((baseExpr, rest):Ast.OperatorSequenceData) = result {
    let! resolvedBaseExpression = tryResolveExpression context baseExpr
    
    let! mappedRest = 
        rest
        |> List.map (fun (operator, expression) -> result {
            let! resolvedExpression = tryResolveExpression context expression
            return {|Operator = operator; Expression = resolvedExpression|}
        }
        )
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
    
    return ResolvedAst.Expression.OperatorSequence({
        BaseExpression = resolvedBaseExpression
        OperatorsAndExpressions = mappedRest
    })
}

and tryResolveNumber ((isNegative, n, numberProperty'):Ast.NumberData) = result {
    return ResolvedAst.Expression.Number({
        IsNegative = isNegative
        StringRepresentation = n
        Property = numberProperty'
    })
}

and tryResolveNull () = Ok ResolvedAst.Expression.Null

and tryResolveNewFunction (context:ResolvedAst.Context) (astData:Ast.ASTData) (input:Ast.NewFunctionData) = result {
    let! resolvedType = tryResolveType astData context.ScopeGenerics input 
    return ResolvedAst.Expression.NewFunction({
        Type = resolvedType
    })
}

and tryResolveNegatedExpression (context:ResolvedAst.Context) (input:Ast.NegatedExpressionData) = result {
    let! resolvedExpression = tryResolveExpression context input
    return ResolvedAst.Expression.NegatedExpression({
        Expression = resolvedExpression
    })
}

and tryResolveWrapperTypeLiteral (context:ResolvedAst.Context) (((typeName, genericsInstantiation), expr):Ast.WrapperTypeLiteralData) = result {
    let! resolvedWrapperTypeDeclaration = Misc.tryFindOrError typeName $"Wrapper type literals must be constructed with wrapper types. '{typeName}' is not a wrapper type." context.ResolvedDeclarations.WrapperTypes
    let! resolvedGenericsInstantiationData = tryResolveGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation
    let! resolvedExpression = tryResolveExpression context expr
    return ResolvedAst.Expression.WrapperTypeLiteral({
        WrapperType = {
            WrapperDeclaration = resolvedWrapperTypeDeclaration
            GenericsInstantiation = resolvedGenericsInstantiationData
        }
        Argument = resolvedExpression
    })
}

and tryResolveIndex (context:ResolvedAst.Context) ((baseExpr, indexExpr):Ast.MapLookupData) = result {
    let! resolvedBaseExpr = tryResolveExpression context baseExpr
    let! resolvedLookupExpr = tryResolveExpression context indexExpr
    return ResolvedAst.Expression.Index({
        ReferenceExpr = resolvedBaseExpr
        LookupExpr = resolvedLookupExpr
    })
}


and tryResolveMapLiteral (context:ResolvedAst.Context) ((keyType, valueType, items):Ast.MapLiteralData) = result {

    let! resolvedKeyType = tryResolveType context.AstData context.ScopeGenerics keyType
    let! resolvedValueType = tryResolveType context.AstData context.ScopeGenerics valueType

    let! resolvedItems = 
        items
        |> List.map (fun (key, valueExpr) -> result {
            let! resolvedKeyExpr = tryResolveExpression context key
            let! resolvedValueExpr = tryResolveExpression context valueExpr
            return (({
                Key = resolvedKeyExpr
                Value = resolvedValueExpr
            }):ResolvedAst.MapKeyValueEntry)
        })
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList

    return ResolvedAst.Expression.MapLiteral({
        KeyType = resolvedKeyType
        ValueType = resolvedValueType
        Items = resolvedItems
    })
}


and tryResolveMakeFunction (context:ResolvedAst.Context) ((unresolvedType, exprs):Ast.MakeFunctionData) = result {
    let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType
    let! mappedExprs = 
        exprs
        |> List.map (tryResolveExpression context)
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList
        
    return 
        ResolvedAst.Expression.MakeFunction({
            Type = resolvedType
            Args = mappedExprs
        })
}
and tryResolveLambda (context:ResolvedAst.Context) ((parameters, returnType, rawScopeData):Ast.LambdaData) = result {
    let! resolvedReturnType = tryResolveType context.AstData context.ScopeGenerics returnType
    let! resolvedParameters = 
        parameters
        |> List.map (tryResolveParameter context.AstData context.ScopeGenerics)
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList

    let! createdParameters =
        parameters
        |> List.map(fun (hasEllipsis, unresolvedType, identifierName) -> result {
            let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType
            return createParameterVariable identifierName hasEllipsis resolvedType
        })
        |> List.sequenceResultA
        |> Result.mapError Misc.flattenList

    let! resolvedScope, _ = tryResolveScope context createdParameters [] [] rawScopeData

    return
        ResolvedAst.Expression.Lambda({
            ReturnType = resolvedReturnType
            Parameters = resolvedParameters
            ScopeData = resolvedScope
        })
}

(* and tryResolveIndex (context:ResolvedAst.Context) ((indexableRef, indexNumber): Ast.IndexData) = 
    result {
        let convertedExpression = 
            match indexableRef with
            | Ast.IndexableReference.Identifier(name) -> Ast.Expression.Identifier(name)
            | Ast.IndexableReference.Parentheses(expr) -> Ast.Expression.Parentheses(expr)
            | Ast.IndexableReference.ArrayLiteral(data) -> Ast.Expression.ArrayLiteral(data)
            | Ast.IndexableReference.SliceLiteral(data) -> Ast.Expression.SliceLiteral(data)
            | Ast.IndexableReference.String(data) -> Ast.Expression.String data
            | Ast.IndexableReference.MapLiteral(data) -> Ast.Expression.MapLiteral(data)
            | Ast.IndexableReference.PointerTo(data) -> Ast.Expression.PointerTo(data)
        let! resolvedExpr = tryResolveExpression context convertedExpression
        return ResolvedAst.Expression.Index({
            Reference = resolvedExpr
            Position = indexNumber
        })
    }
 *)
and tryResolveIdentifier (context:ResolvedAst.Context) (variableName:string) =
    let possibleVariable =
        context.ScopeVariables.TryFindVariable(variableName)
        |> Result.map ResolvedAst.Variable
        |> Result.toOption

    
    let possibleModuleIdentifier = 
        context.ResolvedDeclarations.ImportedModules
        |> Misc.tryFindOrError variableName $"No variable or module '{variableName}' found"
        |> Result.map (fun x -> ResolvedAst.Expression.ModuleReference({ModuleIdentifier = x.IdentifierName}))
        |> Result.toOption
    

    possibleVariable
    |> Option.orElse possibleModuleIdentifier
    |> Misc.fromOptionToResult [$"Variable '{variableName}' not found"]
    |> function
       | Ok x -> Ok x
       | Error e ->
            // failwith "error"
            Error e
    
    


and tryResolveFloat ((isNegative, f, isImaginary): Ast.FloatData) = result {
    return ResolvedAst.Float({
        IsNegative = isNegative
        Float = f
        IsImaginary = isImaginary
    })
}

and tryResolveDereferenceOf (scopeVariables:ResolvedAst.ScopeVariables) (input:Ast.DereferenceOfData) = result {
    let! resolvedVariable = scopeVariables.TryFindVariable(input)
    return 
        ResolvedAst.Expression.DereferenceOf({
            ReferenceIdentifier = resolvedVariable
        })
}

and tryResolveChannelReceive (context:ResolvedAst.Context) (input:Ast.ChannelRecieveData) = 
    result {
        let! resolvedExpr = tryResolveExpression context input
        return ResolvedAst.Expression.ChannelRecieve({
            Expression = resolvedExpr
        })
    }

and tryResolveBoolean (data:Ast.BooleanData) = 
    result {
        return ResolvedAst.Expression.Boolean({
            Value = data
        })
    }

and tryResolveArrayLiteral (context:ResolvedAst.Context) ((arrayLiteralLength, unresolvedType, expressions):Ast.ArrayLiteralData): Result<ResolvedAst.Expression, string list> = 
    result {
        let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType
        let! resolvedExpressions = 
            expressions
            |> List.map (tryResolveExpression context)
            |> List.sequenceResultA
            |> Result.mapError Misc.flattenList

        return ResolvedAst.Expression.ArrayLiteral({
            ItemsType = resolvedType
            Length = arrayLiteralLength
            Items = resolvedExpressions
        })
    }

and tryGetAccessorSupportedType (input:ResolvedAst.ResolvedType) = 
    match input with
    | ResolvedAst.ResolvedType.WrapperTypeType(typeData) ->
        Ok (ResolvedAst.AccessorSupportedType.WrapperType typeData)
    | ResolvedAst.ResolvedType.StructType(typeData) ->
        Ok (ResolvedAst.AccessorSupportedType.StructType typeData)
    | ResolvedAst.ResolvedType.InterfaceType(typeData) ->
        Ok (ResolvedAst.AccessorSupportedType.InterfaceType typeData)
    (* | ResolvedAst.ResolvedType.DerivedFromModuleType(data) ->
        Ok (ResolvedAst.AccessorSupportedType.DerivedFromModuleType data) *)
    (* | ResolvedAst.ResolvedType.ModuleType(moduleName) ->
        // This is technically not correct since the module 
        // type is not the same as "derived" from module type.
        // However, it's much easier to do this "fake it till you make it", esp since, in the future
        // I plan to actually support module types
        Ok (ResolvedAst.AccessorSupportedType.DerivedFromModuleType({AttributeName = moduleName})) *)
    | _ ->
        Error [$"Type '{Ast.transpileType input}' does not support attributes"]

and getAccessorSupportedTypeName (input:ResolvedAst.AccessorSupportedType) = 
    match input with
    | ResolvedAst.AccessorSupportedType.WrapperType(typeData) ->
        typeData.WrapperTypeName
    | ResolvedAst.AccessorSupportedType.StructType(typeData) ->
        typeData.StructName
    | ResolvedAst.AccessorSupportedType.InterfaceType(typeData) ->
        typeData.InterfaceName
(*     | ResolvedAst.AccessorSupportedType.DerivedFromModuleType({AttributeName = name}) ->
        $"Unknown (type comes from '{name}' method or attribute)" *)


and tryResolveIndividualExprArgAccessors (context:ResolvedAst.Context) ((baseExpression, accessors):Ast.ExprArgsAccessorsData):Result<ResolvedAst.AccessorItemExprArgs list, string list> = result {
        
    let memberToAccessorItem (genericsInstantiation:ResolvedAst.GenericsInstantiationData option) (args:ResolvedAst.ExpressionCallArguments option) (input:ResolvedAst.Member) = 
        match input, genericsInstantiation, args with
        | ResolvedAst.Member.InterfaceDeclaration(_), Some _, _ ->
            Error ["Generics are not supported on an interface"]
        | ResolvedAst.Member.InterfaceDeclaration(interfaceRef), _, None ->
            Ok <| ResolvedAst.AccessorItemExprArgs.InterfaceDeclaration(interfaceRef)
        | ResolvedAst.Member.InterfaceDeclaration(interfaceRef), _, Some _ ->
            Error [$"Interface '{interfaceRef.InterfaceDeclaration.Name}' is not callable"]
        | ResolvedAst.Member.InterfaceMethod(methodRef), genericsInstantiationData', None ->
            Ok <| ResolvedAst.AccessorItemExprArgs.InterfaceMethod(methodRef, Arguments = None, GenericsInstantiation = genericsInstantiationData')
        | ResolvedAst.Member.InterfaceMethod(methodRef), genericsInstantiation', Some args ->
            Ok <| ResolvedAst.AccessorItemExprArgs.InterfaceMethod(methodRef, Arguments = Some args, GenericsInstantiation = genericsInstantiation')
        | ResolvedAst.Member.Method(methodDecWithoutScope), genericsInstantiation', None ->
            Ok <| ResolvedAst.AccessorItemExprArgs.Method(methodDecWithoutScope, genericsInstantiation', None)
        | ResolvedAst.Member.Method(methodDecWithoutScope), genericsInstantiation', Some args ->
            Ok <| ResolvedAst.AccessorItemExprArgs.Method(methodDecWithoutScope, genericsInstantiation', Some args)
        | ResolvedAst.Member.StructField(structFieldDec), genericsInstantiation', None ->
            Ok <| ResolvedAst.AccessorItemExprArgs.StructField(structFieldDec, genericsInstantiation', None)
        | ResolvedAst.Member.StructField({Type = ResolvedAst.ResolvedType.FunctionType(_)} as structFieldDec), genericsInstantiationData', Some callData ->
            Ok <| ResolvedAst.AccessorItemExprArgs.StructField(structFieldDec, genericsInstantiationData', Some callData)
        | ResolvedAst.Member.StructField({Type = t}), _, Some _ ->
            Error [$"Type '{transpileType t}' is not callable"]
        | ResolvedAst.Member.StructDeclaration(_), Some _, _ -> 
            Error ["Generics are not supported on a struct"]
        | ResolvedAst.Member.StructDeclaration(structRef), _, None ->
            Ok <| ResolvedAst.AccessorItemExprArgs.StructDeclaration(structRef)
        | ResolvedAst.Member.StructDeclaration(structRef), _, Some _ ->
            Error [$"Struct '{structRef.StructDeclaration.Name}' is not callable"]


    let! resolvedBaseExpression = tryResolveExpression context baseExpression
    let! resolvedBaseType = tryGetTypeFromExpression context.ResolvedDeclarations resolvedBaseExpression
    let! baseAccessorSupportedType = tryGetAccessorSupportedType resolvedBaseType

    let firstAccessor = List.head accessors  
    let restAccessors = List.tail accessors  
        
    /// Gets the next member from the type of the previous expression/accessor
    let getMemberFromType (baseType:ResolvedAst.AccessorSupportedType) (accessorName:string) = 
        match baseType with
        | ResolvedAst.AccessorSupportedType.InterfaceType({InterfaceName = DeclaredInterface context.ResolvedDeclarations interfaceData}) ->
            interfaceData.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        | ResolvedAst.AccessorSupportedType.WrapperType({WrapperTypeName = DeclaredWrapperType context.ResolvedDeclarations wrapperTypeData}) ->
            wrapperTypeData.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        | ResolvedAst.AccessorSupportedType.StructType({StructName = DeclaredStruct context.ResolvedDeclarations structDeclaration}) ->
            structDeclaration.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        (* | ResolvedAst.AccessorSupportedType.DerivedFromModuleType({AttributeName = an}) ->
            Some <| ResolvedAst.Member.MemberDerivedFromModule(AttributeName = an) *)
        |> Misc.fromOptionToResult [$"Accessor '{accessorName}' does not exist on type '{getAccessorSupportedTypeName baseType}'"]


    // Obtain the first accessor and convert it to AccessorItem
    let! firstResolvedAccessor =
        match firstAccessor with
        | Ast.AccessorItemExprArgs.FieldAccessor(name, None, None) ->
            getMemberFromType baseAccessorSupportedType name
            |> Result.bind (memberToAccessorItem None None)

        | Ast.AccessorItemExprArgs.FieldAccessor(name, genericsInstantiation', args') -> result {
            let! resolvedAccessorItem = getMemberFromType baseAccessorSupportedType name
            let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
            let! resolvedArgs' =
                args'
                |> tryResolveOptionalExprArgs context
            return!
                memberToAccessorItem resolvedGenericsInstantiation' resolvedArgs' resolvedAccessorItem
            }
        | Ast.AccessorItemExprArgs.MethodAccessor(name, None, None) ->
            getMemberFromType baseAccessorSupportedType name
            |> Result.bind (memberToAccessorItem None None)

        | Ast.AccessorItemExprArgs.MethodAccessor(name, genericsInstantiation', args') -> result {
            let! resolvedAccessorItem = getMemberFromType baseAccessorSupportedType name
            let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
            let! resolvedArgs' =
                args'
                |> tryResolveOptionalExprArgs context
            return!
                memberToAccessorItem resolvedGenericsInstantiation' resolvedArgs' resolvedAccessorItem
        }
    

    let mutable (resolvedAccessors: ResolvedAst.AccessorItemExprArgs list) = [firstResolvedAccessor]
            
    
    for unresolvedAccessor in restAccessors do 
        let previousAccessor = List.last resolvedAccessors
        let typeFromPreviousAccessor = getTypeFromExprArgAccessor context.ResolvedDeclarations previousAccessor
        let! baseAccessorSupportedType = tryGetAccessorSupportedType typeFromPreviousAccessor
        do!
            match unresolvedAccessor with
            | Ast.AccessorItemExprArgs.MethodAccessor(name, genericsInstantiation', args')
            | Ast.AccessorItemExprArgs.FieldAccessor(name, genericsInstantiation', args') -> result {
                let! resolvedAccessorWithoutCallData = getMemberFromType baseAccessorSupportedType name
                let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
                let! resolvedArgs' =
                    args'
                    |> tryResolveOptionalExprArgs context
                let! resolvedAccessor = memberToAccessorItem  resolvedGenericsInstantiation' resolvedArgs' resolvedAccessorWithoutCallData
                resolvedAccessors <- Misc.listAdd resolvedAccessors resolvedAccessor
                }
            
                

    return resolvedAccessors
}



and tryResolveIndividualPipeArgAccessors (context:ResolvedAst.Context) ((baseExpression, accessors):Ast.PipeArgsAccessorsData):Result<ResolvedAst.AccessorItemPipeArgs list, string list> = result {
        
    let memberToAccessorItem (genericsInstantiation:ResolvedAst.GenericsInstantiationData option) (args:ResolvedAst.PipeCallArguments) (input:ResolvedAst.Member) = 
        match input, genericsInstantiation, args with 
        | ResolvedAst.Member.InterfaceDeclaration(_), Some _, _ ->
            Error ["Generics are not supported on an interface"]
        | ResolvedAst.Member.InterfaceDeclaration(interfaceRef), _, _ ->
            Error [$"Interface '{interfaceRef.InterfaceDeclaration.Name}' is not callable"]
        | ResolvedAst.Member.InterfaceMethod(methodRef), genericsInstantiationData', args ->
            Ok <| ResolvedAst.AccessorItemPipeArgs.InterfaceMethod(methodRef, genericsInstantiationData', args)
        | ResolvedAst.Member.Method(methodDecWithoutScope), genericsInstantiationData', args ->
            Ok <| ResolvedAst.AccessorItemPipeArgs.Method(methodDecWithoutScope, genericsInstantiationData', args)
        | ResolvedAst.Member.StructField({Type = ResolvedAst.ResolvedType.FunctionType(_)} as structFieldDec), genericsInstantiationData', args ->
            Ok <| ResolvedAst.AccessorItemPipeArgs.StructField(structFieldDec, genericsInstantiationData', args)
        | ResolvedAst.Member.StructField({Type = t}), _, _ ->
            Error [$"Type '{transpileType t}' is not callable"]
        | ResolvedAst.Member.StructDeclaration(_), Some _, _ ->
             Error ["Generics are not supported on a struct"]
        | ResolvedAst.Member.StructDeclaration(structRef), _, _ ->
            Error [$"Struct '{structRef.StructDeclaration.Name}' is not callable"]


    let! resolvedBaseExpression = tryResolveExpression context baseExpression
    let! resolvedBaseType = tryGetTypeFromExpression context.ResolvedDeclarations resolvedBaseExpression
    let! baseAccessorSupportedType = tryGetAccessorSupportedType resolvedBaseType

    let firstAccessor = List.head accessors  
    let restAccessors = List.tail accessors  
        
    /// Gets the next member from the type of the previous expression/accessor
    let getMemberFromType (baseType:ResolvedAst.AccessorSupportedType) (accessorName:string) = 
        match baseType with
        | ResolvedAst.AccessorSupportedType.InterfaceType({InterfaceName = DeclaredInterface context.ResolvedDeclarations interfaceData}) ->
            interfaceData.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        | ResolvedAst.AccessorSupportedType.WrapperType({WrapperTypeName = DeclaredWrapperType context.ResolvedDeclarations wrapperTypeData}) ->
            wrapperTypeData.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        | ResolvedAst.AccessorSupportedType.StructType({StructName = DeclaredStruct context.ResolvedDeclarations structDeclaration}) ->
            structDeclaration.GetMember(context.ResolvedDeclarations.Methods, accessorName)
        (* | ResolvedAst.AccessorSupportedType.DerivedFromModuleType({AttributeName = an}) ->
            Some <| ResolvedAst.Member.MemberDerivedFromModule(AttributeName = an) *)
        |> Misc.fromOptionToResult [$"Accessor '{accessorName}' does not exist on type '{getAccessorSupportedTypeName baseType}'"]


    // Obtain the first accessor and convert it to AccessorItem
    let! firstResolvedAccessor =
        match firstAccessor with
        | Ast.AccessorItemPipeArgs.FieldAccessor(name, genericsInstantiation', args) -> result {
            let! resolvedAccessorItem = getMemberFromType baseAccessorSupportedType name
            let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
            let! resolvedArgs =
                args
                |> tryResolvePipeArgs context
            return!
                memberToAccessorItem resolvedGenericsInstantiation' resolvedArgs resolvedAccessorItem
            }
        | Ast.AccessorItemPipeArgs.MethodAccessor(name, genericsInstantiation', args) -> result {
            let! resolvedAccessorItem = getMemberFromType baseAccessorSupportedType name
            let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
            let! resolvedArgs =
                args
                |> tryResolvePipeArgs context
            return!
                memberToAccessorItem resolvedGenericsInstantiation' resolvedArgs resolvedAccessorItem
        }
    

    let mutable (resolvedAccessors: ResolvedAst.AccessorItemPipeArgs list) = [firstResolvedAccessor]
            
    
    for unresolvedAccessor in restAccessors do 
        let previousAccessor = List.last resolvedAccessors
        let typeFromPreviousAccessor = getTypeFromPipeArgAccessor context.ResolvedDeclarations previousAccessor
        let! baseAccessorSupportedType = tryGetAccessorSupportedType typeFromPreviousAccessor
        do!
            match unresolvedAccessor with
            | Ast.AccessorItemPipeArgs.MethodAccessor(name, genericsInstantiation', args)
            | Ast.AccessorItemPipeArgs.FieldAccessor(name, genericsInstantiation', args) -> result {
                let! resolvedAccessorWithoutCallData = getMemberFromType baseAccessorSupportedType name
                let! resolvedGenericsInstantiation' = tryResolveOptionalGenericsInstantiation context.AstData context.ScopeGenerics genericsInstantiation'
                let! resolvedArgs =
                    args
                    |> tryResolvePipeArgs context
                let! resolvedAccessor = memberToAccessorItem resolvedGenericsInstantiation' resolvedArgs resolvedAccessorWithoutCallData
                resolvedAccessors <- Misc.listAdd resolvedAccessors resolvedAccessor
                }
            
                

    return resolvedAccessors
}



and tryResolvePossibleCallableIdentifier (resolvedDeclarations:ResolvedAst.ResolvedDeclarations) (scopeVariables:ResolvedAst.ScopeVariables) (Ast.PossibleCallableIdentifier(name)) =
    resolvedDeclarations.Functions
    |> Misc.tryFindAndMap name ResolvedAst.PossiblyCallableExpression.FunctionDeclaration $"Function '{name}' not found"
    |> Result.orElse(Misc.tryFindAndMap name ResolvedAst.PossiblyCallableExpression.BuiltInFunction $"Function '{name}' not found" resolvedDeclarations.BuiltInFunctions)
    |> Result.orElse (scopeVariables.TryFindVariable(name) |> Result.map ResolvedAst.PossiblyCallableExpression.Var)

and tryResolveChar (input:Ast.CharData) = result {
    return ResolvedAst.Expression.Char(input)
}    

and tryResolveFunctionCall (context:ResolvedAst.Context) ((possibleFunctionReference, genericsInstantiation', arguments):Ast.FunctionExprCallData) = 
    match possibleFunctionReference with
    | Ast.PossibleFunctionReference.Identifier(possibleFunctionNameOrVariable) -> result {
            
        let! resolvedCallableDeclaration =
            tryResolvePossibleCallableIdentifier context.ResolvedDeclarations context.ScopeVariables possibleFunctionNameOrVariable

        let! resolvedArguments = 
            arguments
            |> List.map (fun (expr, hasE) -> tryResolveExpression context expr |> Result.map (fun r -> r, hasE))
            |> List.sequenceResultA
            |> Result.mapError Misc.flattenList

        let mappedArguments = 
            resolvedArguments
            |> List.map (fun (expr, hasE) -> {|Argument = expr; HasEllipsis = hasE|})
            
        let! resolvedOptionalGenericsInstantiation = Misc.applyResultToOption (tryResolveGenericsInstantiation context.AstData context.ScopeGenerics) genericsInstantiation' 

        return ResolvedAst.Expression.FunctionCall({
            Reference = resolvedCallableDeclaration
            Arguments = mappedArguments
            GenericsInstantiation = resolvedOptionalGenericsInstantiation
        })
            
        }
    | Ast.PossibleFunctionReference.Parentheses(expr) -> result {
        let! resolvedExpr = tryResolveExpression context expr
        
        let! resolvedArguments = 
            arguments
            |> List.map (fun (expr, hasE) -> tryResolveExpression context expr |> Result.map (fun r -> r, hasE))
            |> List.sequenceResultA
            |> Result.mapError Misc.flattenList

        let mappedArguments = 
            resolvedArguments
            |> List.map (fun (expr, hasE) -> {|Argument = expr; HasEllipsis = hasE|})

        let! resolvedOptionalGenericsInstantiation = Misc.applyResultToOption (tryResolveGenericsInstantiation context.AstData context.ScopeGenerics) genericsInstantiation'

        return ResolvedAst.Expression.FunctionCall({
            Reference = ResolvedAst.PossiblyCallableExpression.Parentheses resolvedExpr
            Arguments = mappedArguments
            GenericsInstantiation = resolvedOptionalGenericsInstantiation
        })
        
    } 

and tryResolveNonToplevelDeclaration
    (context:ResolvedAst.Context)
    (input:Ast.NonToplevelDeclaration) = 
    match input with
    | Ast.NonToplevelDeclaration.NormalVar(t', vp, expr) -> result {
        let! resolvedExpression = tryResolveExpression context expr
        let! resolvedType' =
            t'
            |> Misc.applyResultToOption (tryResolveType context.AstData context.ScopeGenerics) 
        let transpileData = 
            ResolvedAst.NonToplevelDeclaration.NormalVar({
                VariablePattern = vp
                Expression = resolvedExpression
                Type = resolvedType'
            })
        let variableData = createExprVariables vp resolvedExpression
        context.ScopeVariables.StoreVariables(variableData)
        return transpileData
        }
    | Ast.NonToplevelDeclaration.ZeroVar(vp, unresolvedType) -> result {
        let! resolvedType = tryResolveType context.AstData context.ScopeGenerics unresolvedType 
        let transpileData =
            ResolvedAst.NonToplevelDeclaration.ZeroVar {
                VariablePattern = vp
                Type = resolvedType
            }
        let variableData = 
            createZeroVariables vp resolvedType
        context.ScopeVariables.StoreVariables(variableData)
        return transpileData
    }

and createExprVariables (vp:Base.VariablePattern) (expr:ResolvedAst.Expression) = 
    vp
    |> List.map (fun name -> ({Expression = expr; Name = name}:ResolvedAst.ExprVariableData))
    |> List.map ResolvedAst.ExprVariable

and createZeroVariables (vp:Base.VariablePattern) (t:ResolvedAst.ResolvedType) = 
    vp
    |> List.map (fun name -> ({Name = name; Type = t}:ResolvedAst.ZeroVariableData))
    |> List.map ResolvedAst.ZeroVariable

and createParameterVariable (name:string) (hasEllipsis:bool) (t:ResolvedAst.ResolvedType) = 
    ({Name = name; Type = t; HasEllipsis = hasEllipsis}:ResolvedAst.ParameterVariableData)
    |> ResolvedAst.ParameterVariable

and tryResolveStatement (context:ResolvedAst.Context) (input:Ast.Statement) =
    match input with
    | Ast.Statement.Continue -> tryResolveContinue ()
    | Ast.Statement.Break -> tryResolveBreak ()
    | Ast.Statement.Defer(deferData) -> tryResolveDefer context deferData
    | Ast.Statement.ForLoop(forLoopData) -> tryResolveForLoop context forLoopData
    | Ast.Statement.Force(forceData) -> tryResolveForce context forceData
    | Ast.Statement.Go(goData) -> tryResolveGo context goData
    | Ast.Statement.If(ifData) -> tryResolveIf context ifData
    | Ast.Statement.Return(returnData) -> tryResolveReturn context returnData
    | Ast.Statement.Select(selectData) -> tryResolveSelect context selectData
    | Ast.Statement.Try(tryData) -> tryResolveTry context tryData
    | Ast.Statement.SwitchCase(switchCaseData) -> tryResolveSwitchCase context switchCaseData
    | Ast.Statement.WhileLoop(whileLoopData) -> tryResolveWhileLoop context whileLoopData

and tryResolveWhileLoop (context:ResolvedAst.Context) ((expr, rawScopeData):Ast.WhileLoopData) = result {
    let! resolvedExpr = tryResolveExpression context expr
    let! resolvedScope, _ = tryResolveScope context [] [] [] rawScopeData
    return ResolvedAst.Statement.WhileLoop({
        Conditional = resolvedExpr
        ScopeData = resolvedScope
    })
}
    
and tryResolveWhen (context:ResolvedAst.Context) ((expr, returnType, whenCases, defaultExpr):Ast.WhenData) = result {
    let! resolvedExpr = tryResolveExpression context expr
    let! resolvedReturnType = tryResolveType context.AstData context.ScopeGenerics returnType
    
    let! resolvedRegularCases = result {
        let mutable cases = []
        for caseExpr, resultExpr in whenCases do
            let! resolvedCaseExpr = tryResolveExpression context caseExpr 
            let! resolvedResultExpr = tryResolveExpression context resultExpr 
            let (data:ResolvedAst.WhenCase) = {CaseExpr = resolvedCaseExpr; ResultExpr = resolvedResultExpr}
            cases <- Misc.listAdd cases data
        return cases
        }
    
    let! resolvedDefaultExpression = tryResolveExpression context defaultExpr


    return ResolvedAst.Expression.When({
        Cases = resolvedRegularCases
        Expression = resolvedExpr
        ReturnType = resolvedReturnType
        DefaultExpression = resolvedDefaultExpression
    })
}

and tryResolveTry (context:ResolvedAst.Context) ((identifiers', successExpr, errorExpr):Ast.TryData) = result {
    let! resolvedSuccessExpr = tryResolveExpression context successExpr
    
    match identifiers' with
    | Some (successVar, failureVar) ->
        let createdVariables = createExprVariables [successVar; failureVar] resolvedSuccessExpr
        context.ScopeVariables.StoreVariables(createdVariables)
    | None ->
        ()
    let! failureExpr = tryResolveExpression context errorExpr
    return ResolvedAst.Statement.Try({
        Identifiers = identifiers'
        ExpressionToTry = resolvedSuccessExpr
        FailureExpression = failureExpr
    })
}

and tryResolveSelect (context:ResolvedAst.Context) ((regularCases, defaultCase'):Ast.SelectData) = result {

    let! resolvedRegularCases = result {
        let mutable cases = []
        for variablePattern', expr, rawScopeData in regularCases do
            let! resolvedExpr = tryResolveExpression context expr 
            let createdVariables =
                variablePattern'
                |> Option.map (fun variablePattern -> createExprVariables variablePattern resolvedExpr)
                |> Option.defaultValue []
            let! resolvedScope, _ = tryResolveScope context createdVariables [] [] rawScopeData
            let (data:ResolvedAst.RegularSelectCase) = {VariablePattern = variablePattern'; Expression = resolvedExpr; Scope = resolvedScope}
            cases <- Misc.listAdd cases data
        return cases
        }
    
    let! resolvedDefaultCase = 
        match defaultCase' with
        | Some (Ast.DefaultCase(defaultCase)) -> result {
            let! resolvedScope, _ = tryResolveScope context [] [] [] defaultCase
            return Some resolvedScope
            }
        | None -> result {return None}


    return ResolvedAst.Statement.Select {Cases = resolvedRegularCases; DefaultCase = resolvedDefaultCase}
}

and tryResolveReturn (context:ResolvedAst.Context) (input:Ast.StatementData) = result {
    let! resolvedExpr = 
        match input with
        | Some expr -> result {
            let! r = tryResolveExpression context expr
            return Some r
            }
        | None -> result {return None} 
    return ResolvedAst.Statement.Return({
        ReturnedExpression = resolvedExpr
    })
}

and tryResolveIf (context:ResolvedAst.Context) (((expr, scopeData), elseIfs', else'):Ast.IfData) = result {
    let! resolvedExpr = tryResolveExpression context expr 
    let! resolvedScope, _ = tryResolveScope context [] [] [] scopeData

    let! resolvedElseIfs =
        elseIfs'
        |> Misc.applyResultToOption(fun elseIf -> result {
            return! 
                elseIf
                |> List.map(fun (expr, scopeData) -> result {
                    let! resolvedExpr = tryResolveExpression context expr 
                    let! resolvedScope, _ = tryResolveScope context [] [] [] scopeData
                    return 
                        {|
                            ConditionalExpr = resolvedExpr
                            ScopeData = resolvedScope 
                        |}
                })
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            })

    let! elseData = 
        else'
        |> Misc.applyResultToOption (fun elseData -> result {
            let! resolvedScope, _ = tryResolveScope context [] [] [] elseData
            return {|ScopeData = resolvedScope|}
            })


    return ResolvedAst.Statement.If({
        If = {|
            ConditionalExpr = resolvedExpr
            ScopeData = resolvedScope
        |}
        ElseIfs = resolvedElseIfs
        Else = elseData
    }) 
}

and tryResolveGo (context:ResolvedAst.Context) (input:Ast.RawScopeData) = result {
    let! resolvedScopeData, _ = tryResolveScope context [] [] [] input
    return ResolvedAst.Statement.Go(resolvedScopeData)
}

and tryResolveForce (context:ResolvedAst.Context) (((successVar, errorVar) as vars, attemptExpr, backupExpr):Ast.ForceData) = result {
    let! resolvedAttemptExpression = tryResolveExpression context attemptExpr
    let createdVariable = createExprVariables [successVar; errorVar] resolvedAttemptExpression
    context.ScopeVariables.StoreVariables(createdVariable)
    let! resolvedPanicExpression = tryResolveExpression context backupExpr
    return ResolvedAst.Statement.Force({
        Identifiers = vars
        ExpressionToTry = resolvedAttemptExpression
        PanicExpression = resolvedPanicExpression
    })
}

and tryResolveForLoop (context:ResolvedAst.Context) ((style, rawScopeData):Ast.ForLoopData) = result {
    
    // Made to remove the annoying warning received when you know for a fact
    // that there will only be two items in a list
    let (|TwoItemList|) (l:'T list) = 
        match l with
        [a; b] -> a, b
        | _ -> failwith "Should never get here"

    let! resolvedStyle, resolvedScope = 
        match style with
        | Ast.ForLoopStyle.ShortHand(vp, expr) -> result {
            let! resolvedExpr = tryResolveExpression context expr
            let newVariables = createExprVariables vp resolvedExpr
            let! resolvedScope, _ = tryResolveScope context newVariables [] [] rawScopeData
            let mappedVp =
                vp
                |> List.map (fun name -> ({Expression = resolvedExpr; Name = name}:ResolvedAst.ExprVariableData))
            return ResolvedAst.ForLoopStyle.ShortHand(mappedVp, resolvedExpr), resolvedScope
            }
        | Ast.ForLoopStyle.Traditional((vp, expr1), expr2, expr3) -> result {
            let! resolvedExpr1 = tryResolveExpression context expr1
            let newVariables = createExprVariables vp resolvedExpr1
            let! resolvedScope, TwoItemList(resolvedExpr2, resolvedExpr3) = tryResolveScope context newVariables [] [expr2; expr3] rawScopeData
            let mappedVp =
                vp
                |> List.map (fun name -> ({Expression = resolvedExpr1; Name = name}:ResolvedAst.ExprVariableData))
            return ResolvedAst.ForLoopStyle.Traditional((mappedVp, resolvedExpr1), resolvedExpr2, resolvedExpr3), resolvedScope
            }

    

    return ResolvedAst.Statement.ForLoop({
        Style = resolvedStyle
        ScopeData = resolvedScope
    })
}

and tryResolveDefer (context:ResolvedAst.Context) (input:Ast.RawScopeData) = result {
    let! resolvedScope, _ = tryResolveScope context [] [] [] input
    return ResolvedAst.Statement.Defer(resolvedScope)
}

and tryResolveBreak (_) = Ok ResolvedAst.Break

and tryResolveContinue () = Ok ResolvedAst.Continue

/// Resolves a scope.
/// context = the context this scope is being created in
/// additionalVariables = The variables that were created along with the creation of this scope
/// the unresolved data of the new scope
/// NOTE: The modified context that is returned is ONLY for evaluating expressions after the scope has finished
and tryResolveScope
    ({ScopeVariables = aboveScopeVariables; AstData = astData; ResolvedDeclarations = resolvedDeclarations; ScopeGenerics = scopeGenerics}:ResolvedAst.Context as context)
    (additionalVariables: ResolvedAst.ScopedVariable list)
    (newGenerics: Base.Generics)
    (additionalExprsToBeEvaluated: Ast.Expression list)
    ((scopeContent, scopeId): Ast.RawScopeData as incomingScope)

    : Result<ResolvedAst.ScopeData * ResolvedAst.Expression list, string list>
    =
    result {

        let thisScopeVariables = aboveScopeVariables.Copy()
        
        let thisContext = ({
            AstData = astData
            ResolvedDeclarations = resolvedDeclarations
            ScopeVariables = thisScopeVariables
            ScopeGenerics = newGenerics
        }:ResolvedAst.Context)

        let variablesBeforeResolving =
            thisContext.ScopeVariables.GetAllVariables()

        thisContext.ScopeVariables.StoreVariables(additionalVariables)

        let! evaluatedExprs = 
            additionalExprsToBeEvaluated
            |> List.map (tryResolveExpression thisContext)
            |> List.sequenceResultA
            |> Result.mapError Misc.flattenList


        let! resolvedScopeConstructs =

            scopeContent
            |> List.map (fun scopeConstruct ->
                match scopeConstruct with
                | Ast.ScopedExpression(expr) -> result {
                    let! resolvedExpression = 
                        tryResolveExpression thisContext expr
                    return ResolvedAst.ScopedExpression(resolvedExpression)
                    }
                    
                | Ast.ScopedDeclaration(dec) -> result {
                    let! resolvedDeclaration =
                        tryResolveNonToplevelDeclaration thisContext dec
                    return ResolvedAst.ScopedDeclaration(resolvedDeclaration)
                    }
                    
                | Ast.ScopedSatement(s) -> result {
                    let! resolvedStatement = tryResolveStatement thisContext s
                    return ResolvedAst.ScopedStatement(resolvedStatement)
                    } )
            |> List.sequenceResultA
            |> Result.mapError Misc.flattenList

        let variablesAfterResolving = 
            thisContext.ScopeVariables.GetAllVariables()

        let variblesDefinedInThisScope = 
            Misc.differenceOfTwoLists variablesAfterResolving variablesBeforeResolving

        let thisScopeVariablesCopy = thisScopeVariables.Copy()

        // Drop everything when we get to the end
        thisScopeVariables.Clear()

        return ({
            Id = scopeId
            Content = resolvedScopeConstructs
            Variables = thisScopeVariablesCopy
            VariablesDefinedInThisScope = variblesDefinedInThisScope
        }:ResolvedAst.ScopeData), evaluatedExprs

    
    }
    

(* and resolveAccessorItemArguments<'T, 'N> (context:ResolvedAst.Context) (argMapper: 'T -> Result<'N, string list>) (accessors:ResolvedAst.AccessorItem<'T> list): Result<ResolvedAst.AccessorItemExprArgs,string list> list = 
    
    accessors
    |> List.map (fun accessor ->
        match accessor with
        (* | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name; CallData = Some callData) -> result {
            let! mappedFunctionArguments = 
                callData.FunctionArguments
                |> List.map (fun arg ->
                    argMapper arg.Argument
                    |> Result.map (fun r -> {|Argument = r; HasEllipsis = arg.HasEllipsis|}))
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            return ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name, CallData = Some {GenericsInstantiation = callData.GenericsInstantiation; FunctionArguments = mappedFunctionArguments})
            }
        | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name; CallData = None) ->
            Ok <| ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name, CallData = None) *)
        | ResolvedAst.AccessorItem.Method(Declaration = declaration; CallData = Some callData) -> result {
            let! mappedFunctionArguments = 
                callData.FunctionArguments
                |> List.map (fun arg ->
                    argMapper arg.Argument
                    |> Result.map (fun r -> {|Argument = r; HasEllipsis = arg.HasEllipsis|}))
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList


            return ResolvedAst.AccessorItem.Method(Declaration = declaration, CallData = Some {GenericsInstantiation = callData.GenericsInstantiation; FunctionArguments = mappedFunctionArguments})
            }
            
        | ResolvedAst.AccessorItem.InterfaceMethod(Declaration = declaration; CallData = None) ->
            ResolvedAst.AccessorItem.InterfaceMethod(Declaration = declaration, CallData = None)
            |> Ok
        | ResolvedAst.AccessorItem.StructField(Declaration = declaration; CallData = None) ->
            ResolvedAst.AccessorItem.StructField(Declaration = declaration, CallData = None)
            |> Ok
        | ResolvedAst.AccessorItem.StructField(Declaration = declaration; CallData = Some callData) as d -> result {
            let! mappedFunctionArguments = 
                callData.FunctionArguments
                |> List.map (fun arg ->
                    argMapper arg.Argument
                    |> Result.map(fun r -> {|Argument = r; HasEllipsis = arg.HasEllipsis|}))
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            return ResolvedAst.AccessorItem.StructField(Declaration = declaration, CallData = Some {GenericsInstantiation = callData.GenericsInstantiation; FunctionArguments = mappedFunctionArguments})
            }
            
        | ResolvedAst.AccessorItem.InterfaceMethod(Declaration = declaration; CallData = Some callData) -> result {
            let! mappedFunctionArguments = 
                callData.FunctionArguments
                |> List.map (fun arg ->
                    argMapper arg.Argument
                    |> Result.map(fun r -> {|Argument = r; HasEllipsis = arg.HasEllipsis|}))
                |> List.sequenceResultA
                |> Result.mapError Misc.flattenList
            return ResolvedAst.AccessorItem.InterfaceMethod(Declaration = declaration, CallData = Some {GenericsInstantiation = callData.GenericsInstantiation; FunctionArguments = mappedFunctionArguments})
            }
            
        | ResolvedAst.AccessorItem.StructDeclaration({ GenericsInstantiation = genericsInstantiation; StructDeclaration = structDeclaration }) ->
            ResolvedAst.AccessorItem.StructDeclaration({StructDeclaration = structDeclaration; GenericsInstantiation = genericsInstantiation})
            |> Ok
        | ResolvedAst.AccessorItem.Method(Declaration = declaration; CallData = None) ->
            ResolvedAst.AccessorItem.Method(Declaration = declaration, CallData = None)
            |> Ok
        | ResolvedAst.AccessorItem.InterfaceDeclaration({InterfaceDeclaration = interfaceDeclaration; GenericsInstantiation = genericsInstantiation}) ->
            ResolvedAst.AccessorItem.InterfaceDeclaration({InterfaceDeclaration = interfaceDeclaration; GenericsInstantiation = genericsInstantiation})
            |> Ok
    )
 *)
