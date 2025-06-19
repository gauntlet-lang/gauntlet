(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Transpilers.ScopeTranspilers

open Types
open Utils
open FsToolkit.ErrorHandling
open Misc

let rec transpileScope (input:ResolvedAst.ScopeData) : string =
    
    let (|EndingInReturn|_|) (input:string) =
        let lines = Array.toList <| input.Split("\n")
        let lastLine = Misc.getLastItem lines
        let lastLineStartsWithReturn = lastLine.StartsWith("return")
        if lastLineStartsWithReturn then
            let strWithoutLastLine = String.concat "\n" (Misc.itemsExceptLast lines)
            Some (strWithoutLastLine, lastLine) 
        else
            None
        


    let transpiledContent = 
        input.Content
        |> List.map (fun construct ->
            match construct with
            | ResolvedAst.ScopeConstruct.ScopedExpression(expr) ->
                transpileExpression expr
            | ResolvedAst.ScopeConstruct.ScopedStatement(statement) ->
                transpileStatement statement
            | ResolvedAst.ScopeConstruct.ScopedDeclaration(declaration) ->
                transpileNonToplevelDeclaration declaration
        )
        |> String.concat "\n"

    let variablesToDump =
        input.VariablesDefinedInThisScope
        |> List.filter (fun v ->
            match v with
            | ResolvedAst.ScopedVariable.DestructuredVariable(_)
            | ResolvedAst.ScopedVariable.ParameterVariable (_) -> false // These don't count
            | ResolvedAst.ScopedVariable.ExprVariable(_)
            | ResolvedAst.ScopedVariable.ZeroVariable(_) -> true
        )
    
    let variableDiscarder = BaseTranspilers.createVariableDiscarder variablesToDump

    match transpiledContent with
    | EndingInReturn (strWithoutLastLine, lastLine) ->
        strWithoutLastLine 
        + "\n"
        + variableDiscarder
        + "\n"
        + lastLine
    | _ -> transpiledContent + variableDiscarder   
    

and transpileExpression (input:ResolvedAst.Expression): string = 
    match input with
    | ResolvedAst.Expression.Accessors(accessorsData) -> 
        transpileExprAccessors accessorsData    
    | ResolvedAst.Expression.ArrayLiteral(arrayLiteralData) ->
        transpileArrayLiteral arrayLiteralData
    | ResolvedAst.Expression.PatternMatch(data) ->
        transpilePatternMatch data
    | ResolvedAst.Expression.Boolean(b) ->
        transpileBool b
    | ResolvedAst.Expression.Char(c) -> transpileChar c
    | ResolvedAst.Expression.ChannelRecieve(channelRecieveData) ->
        $"<-{transpileExpression channelRecieveData.Expression}"
    | ResolvedAst.Expression.DereferenceOf(dereferenceOfData) ->
        $"&{dereferenceOfData.ReferenceIdentifier.GetName()}"
    | ResolvedAst.Expression.ConstantIdentifier(data) -> adjustName data.Reference.ExportedStatus data.Identifier
    | ResolvedAst.Expression.Float(floatData) ->
        transpileFloat floatData
    | ResolvedAst.Expression.FunctionCall(functionCallData) ->
        transpileExprFunctionCall functionCallData
    | ResolvedAst.Expression.EnumCaseLiteral(data) ->
        transpileExprEnumCaseLiteral data
    | ResolvedAst.Expression.Index(indexData) -> transpileIndex indexData
    | ResolvedAst.Expression.Lambda(lambdaData) -> transpileLambda lambdaData
    | ResolvedAst.Expression.MakeFunction(makeFunctionData) ->
        transpileMakeFunction makeFunctionData
    | ResolvedAst.Expression.MapLiteral(mapLiteralData) -> transpileMapLiteralData mapLiteralData
    | ResolvedAst.Expression.ModuleAccessor(moduleAccessorData) -> transpileModuleExprAccessor moduleAccessorData
    | ResolvedAst.Expression.WrapperTypeLiteral(wrapperTypeLiteralData) -> transpileWrapperTypeLiteral wrapperTypeLiteralData
    | ResolvedAst.Expression.NegatedExpression(negatedExpressionData) -> transpileNegatedExpression negatedExpressionData
    | ResolvedAst.Expression.NewFunction(newFunctionData) -> transpileNewFunction newFunctionData
    | ResolvedAst.Expression.Null -> "nil"
    | ResolvedAst.Expression.Number(numberData) -> transpileNumber numberData
    | ResolvedAst.Expression.OperatorSequence(operatorSequenceData) -> transpileOperatorSequence operatorSequenceData
    | ResolvedAst.Expression.Parentheses(parenthesesData) ->
        transpileParentheses parenthesesData
    | ResolvedAst.Expression.PipeOperator(pipeOperatorData) -> transpilePipeOperator pipeOperatorData
    | ResolvedAst.Expression.PointerTo(pointerToData) -> transpilePointerToData pointerToData
    | ResolvedAst.Expression.Variable(scopedVariableData) -> transpileScopedVariable scopedVariableData
    | ResolvedAst.Expression.Slice(sliceData) -> transpileSlice sliceData
    | ResolvedAst.Expression.SliceLiteral(sliceLiteralData) -> transpileSliceLiteral sliceLiteralData
    | ResolvedAst.Expression.String(str) -> transpileString str
    | ResolvedAst.Expression.StructLiteral(structLiteralData) -> transpileStructLiteral structLiteralData
    | ResolvedAst.Expression.When(whenData) -> transpileWhen whenData
    | ResolvedAst.Expression.TernaryOperator(ternaryOperatorData) -> transpileTernaryOperator ternaryOperatorData
    | ResolvedAst.Expression.Tuple(tupleData) -> transpileTuple tupleData
    | ResolvedAst.Expression.TypeAssertion(typeAssertionData) -> transpileTypeAssertion typeAssertionData
    | ResolvedAst.Expression.TypeConversion(typeConversionData) -> transpileTypeConversion typeConversionData
    | ResolvedAst.Expression.UnaryOperator(unaryOperatorData) -> transpileUnaryOperator unaryOperatorData
    | ResolvedAst.Expression.Unit -> "()"
    | ResolvedAst.Expression.ModuleReference(d) -> d.ModuleIdentifier

and transpileBool (b:ResolvedAst.BooleanData) = 
    (string b.Value).ToLower()

and transpileChar (c:char) = 
    "'" + string c + "'"

and transpileString (input:ResolvedAst.StringData) = 
    if not (input.Contains("\n")) then "\"" + input + "\"" else "`" + input + "`"

and transpileFloat (floatData:ResolvedAst.FloatData) = 
    let suffix = if floatData.IsImaginary then "i" else ""
    let prefix = if floatData.IsNegative then "-" else "" 
    prefix + string floatData.Float + suffix

and transpilePatternEnumCaseLiteral (transpiledExprBeingMatched:string) (input:ResolvedAst.PatternEnumCaseLiteralData) = 
    let mappedConditions = 
        input.Fields
        |> List.map (fun data ->
            let castedCase = $"[{input.CaseId}].(Case{input.CaseId})"
            let transpiledValue, _ = transpileMatchingPattern true transpiledExprBeingMatched data.Value
            match data.Value with
            | ResolvedAst.MatchingPattern.EnumCaseLiteral(_) ->
                transpiledValue
            | _ ->
                transpiledExprBeingMatched + $"{castedCase}." + data.Name  + "==" + transpiledValue)
        |> List.map (fun x -> "(" + x + ")")
        |> String.concat "&&"
    if mappedConditions.Length = 0 then
        ""
    else
        "(func() bool { return " + mappedConditions + "})()"

and getDeconstructedVarsFromPatternEnumCaseLiteral (input:ResolvedAst.PatternEnumCaseLiteralData) = 
    let mutable fields = []
    for field in input.Fields do
        match field.Value with
        | ResolvedAst.MatchingPattern.Destructure(ident) ->
            fields <- Misc.listAdd fields (ident, field.Name, input.CaseId)
        (* | ResolvedAst.MatchingPattern.ArrayLiteral(data) ->
            () *)
        | ResolvedAst.MatchingPattern.Boolean(b) ->
            ()
        | ResolvedAst.MatchingPattern.Char(c) ->
            ()
        | ResolvedAst.MatchingPattern.Float(data) ->
            ()
        | ResolvedAst.MatchingPattern.Number(num) ->
            ()
        (* | ResolvedAst.MatchingPattern.SliceLiteral(data) ->
            () *)
        | ResolvedAst.MatchingPattern.String(data) -> ()
        | ResolvedAst.MatchingPattern.EnumCaseLiteral(data) ->
            fields <- List.append fields (getDeconstructedVarsFromPatternEnumCaseLiteral data)
    fields
        
    

and transpileMatchingPattern (ignorePrefix:bool) (transpiledExprBeingMatched:string) (input:ResolvedAst.MatchingPattern) = 
    let prefix = if ignorePrefix then "" else transpiledExprBeingMatched + "=="
    match input with
    (* | ResolvedAst.MatchingPattern.ArrayLiteral(data) ->
        prefix + transpileArrayLiteral data, [] *)
    | ResolvedAst.MatchingPattern.Boolean(b) ->
        prefix + transpileBool b, []
    | ResolvedAst.MatchingPattern.Char(c) ->
        prefix + transpileChar c, []
    | ResolvedAst.MatchingPattern.Float(data) ->
        prefix + transpileFloat data, []
    | ResolvedAst.MatchingPattern.Destructure(ident) ->
        ident, []
    | ResolvedAst.MatchingPattern.Number(num) ->
        prefix + transpileNumber num, []
    (* | ResolvedAst.MatchingPattern.SliceLiteral(data) ->
        prefix + transpileSliceLiteral data, [] *)
    | ResolvedAst.MatchingPattern.String(data) -> prefix + transpileString data, []
    | ResolvedAst.MatchingPattern.EnumCaseLiteral(data) ->
        let x = getDeconstructedVarsFromPatternEnumCaseLiteral data
        transpilePatternEnumCaseLiteral transpiledExprBeingMatched data, x
    |> fun (result, destructuredVars) -> "(" + result + ")", destructuredVars


and transpilePatternMatch (input:ResolvedAst.PatternMatchData) = 
    let transpiledMatchExpr = transpileExpression input.MatchExpr
    input.Cases
    |> List.map (fun case ->
        let transpiledCase = transpileScope case.Scope
        let conditions, destructuredVars = transpileMatchingPattern false transpiledMatchExpr case.Pattern
        let varNames = destructuredVars |> List.map (fun (n, _, _) -> n)
        let varValues = destructuredVars |> List.map (fun (_, fieldName, caseId) -> $"{transpiledMatchExpr}[{caseId}].(Case{caseId}).{fieldName}")
        let ifStatementBeginning = (String.concat ", "varNames) + ":=" + (String.concat ", " varValues)
        let ifStatement =
            if varNames.Length <> 0 then
                "\nif " + ifStatementBeginning + "; " + conditions + "{\n" + transpiledCase + "\n}"
            else
                let modifiedConditions = if conditions.Trim() = "()" then "true" else conditions
                $"if {modifiedConditions}" + "{\n" + transpiledCase + "\n}"
        let getStatementSurrounding (innerIfStatement:string) = 
            match case.Pattern with
            | ResolvedAst.MatchingPattern.EnumCaseLiteral(data) ->
                $"if _, exists := {transpiledMatchExpr}[{data.CaseId}]; exists " + "{\n " + innerIfStatement + "}"
            | _ -> innerIfStatement
        ifStatement
        |> getStatementSurrounding
    )
    |> String.concat "\n"


and transpileExprEnumCaseLiteral (input: ResolvedAst.ExprEnumCaseLiteralData) = 
    
    let mappedFields = 
        input.Fields
        |> List.map (fun x -> x.Name + ":" + transpileExpression x.Value)
        |> String.concat ", "

    "map[int]any{" + (string input.CaseId) + ":" + $"Case{input.CaseId}" + "{" + mappedFields + "}" + "}"


and transpileUnaryOperator (input:ResolvedAst.UnaryOperatorData) =
    let transpiledBaseExpr = transpileExpression input.BaseExpression
    let transpiledSecondExpr =
         input.SecondExpression
        |> Option.map transpileExpression
        |> Option.defaultValue ""

    $"{transpiledBaseExpr} {input.Operator} {transpiledSecondExpr}"

and transpileTypeConversion (input:ResolvedAst.TypeConversionData) =
    let transpiledType = Ast.transpileType input.Type
    let transpiledExpression = transpileExpression input.Expression
    $"{transpiledType}({transpiledExpression})"
    

and transpileTypeAssertion (input:ResolvedAst.TypeAssertionData) =
    let transpiledScopeVariable = transpileScopedVariable input.Identifier
    let transpiledType = Ast.transpileType input.Type
    transpiledScopeVariable + $".({transpiledType})" 

and transpileTuple (input:ResolvedAst.TupleData) =
    let transpiledExprs = 
        input.Expressions
        |> List.map transpileExpression 
        |> String.concat ", "
        
    $"{transpiledExprs}"
    

and transpileTernaryOperator (input:ResolvedAst.TernaryOperatorData) =
    let transpiledReturnType = Ast.transpileType input.ExpressionType
    let transpiledConditionExpr = transpileExpression input.ConditionExpression
    let transpiledSuccessExpr = transpileExpression input.SuccessExpression
    let transpiledFailureExpr = transpileExpression input.FailureExpression
    $"(func () {transpiledReturnType}"
    + "{\n"
    + $"if {transpiledConditionExpr} "
    + "{\n"
    + $"return {transpiledSuccessExpr}\n"
    + "}\n"
    + $"return {transpiledFailureExpr}"
    + "})()"

and transpileSwitchCase (input:ResolvedAst.SwitchCaseType) =

    match input with
    | ResolvedAst.SwitchCaseType.SwitchExpression(data) ->
        let transpiledDefaultCase = 
            data.DefaultCase
            |> Option.map (fun dc -> "default: {\n" + transpileScope dc + "\n}")
            |> Option.defaultValue ""
        let transpiledSwitchExpression = transpileExpression data.SwitchExpression
        let transpiledCases = 
            data.Cases
            |> List.map(fun (expr, scopeData) ->
                let transpiledExpression = transpileExpression expr 
                let transpiledScope = transpileScope scopeData
                $"case {transpiledExpression} : " + "{" + transpiledScope + "}"
            )
            |> String.concat "\n"
        
        "switch " + transpiledSwitchExpression + " {\n" + transpiledCases + $"\n{transpiledDefaultCase}" + "\n}"
    | ResolvedAst.SwitchCaseType.SwitchType(data) ->
        let transpiledExpr = transpileExpression data.Expression
        let transpiledDefaultCase = 
            data.DefaultCase
            |> Option.map (fun dc -> "default: {\n" + transpileScope dc + "\n}")
            |> Option.defaultValue ""
        let transpiledCases = 
            data.Cases
            |> List.map(fun (t, scopeData) ->
                let transpiledType = Ast.transpileType t 
                let transpiledScope = transpileScope scopeData
                $"case {transpiledType} : " + "{" + transpiledScope + "}"
            )
            |> String.concat "\n"
        
        "switch " + $"{transpiledExpr}.(type)" + " {\n" + transpiledCases + $"\n{transpiledDefaultCase}" + "\n}"

and transpileStructLiteral (input:ResolvedAst.StructLiteralData) = 
    let structData = input.Struct.StructDeclaration
    let transpiledTypeName = Ast.transpileTypeReference structData.ExportedStatus structData.Name input.Struct.GenericsInstantiation
    let transpiledStructFields =
        input.Fields
        |> List.map(fun {Name = name; Value = expr; ExportedStatus = exportedStatus} ->
            $"{Misc.adjustName exportedStatus name} : {transpileExpression expr}" 
        )
        |> String.concat ", "
    transpiledTypeName + "{" + transpiledStructFields + "}"


and transpileSliceLiteral (input:ResolvedAst.SliceLiteralData) = 
    let transpiledType = Ast.transpileType input.SliceItemsType
    let transpiledExprs = 
        input.Items
        |> List.map (fun item -> transpileExpression item)
        |> String.concat ", "
    "[]"
    + transpiledType
    + "{"
    + transpiledExprs
    + "}"

and transpileSlice (input:ResolvedAst.SliceData) =
    let transpiledReference = transpileExpression input.Reference
    let transpiledStartExpr = transpileExpression input.StartIndex
    let transpiledEndExpr = transpileExpression input.EndIndex
    transpiledReference + $"[{transpiledStartExpr}:{transpiledEndExpr}]"

and transpileScopedVariable (input:ResolvedAst.ScopedVariable) = 
    input.GetName()

and transpilePointerToData (input:ResolvedAst.PointerToData) = 
    "*" + transpileScopedVariable input.ReferenceIdentifier

and transpilePipeOperator (input:ResolvedAst.PipeOperatorData) =
    let transpiledBaseExpr = transpileExpression input.BaseExpression
    let transpiledPipes = 
        input.Pipes
        |> List.map (fun pipe ->
            match pipe with
            | ResolvedAst.PipeCall.AccessorCall(accessorCallData) ->
                let transpiledBaseExpr = transpileExpression accessorCallData.BaseExpression
                transpiledBaseExpr + "." + transpilePipeFunctionCallArgumentAccessors accessorCallData.Accessors
            | ResolvedAst.PipeCall.FunctionCall(functionPipeCallData) ->
                let transpiledReference = transpilePossiblyCallableExpression functionPipeCallData.Reference
                let transpiledArguments = transpilePipeArguments functionPipeCallData.Arguments
                transpiledReference + transpiledArguments
            | ResolvedAst.PipeCall.ModuleAccessorCall(moduleAccessorCallData) ->
                let transpiledAccessors = 
                    moduleAccessorCallData.Accessors
                    |> List.map (fun accessorData ->
                        let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation accessorData.GenericsInstantiation
                        let transpiledCallData = 
                            transpilePipeArguments accessorData.Arguments
                            

                        (* let transpiledCallData = 
                            accessorData.CallData
                            |> Option.map(fun callData ->
                                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation callData.GenericsInstantiation
                                let transpiledCallData = transpileFunctionArguments callData.FunctionArguments
                                transpiledGenericsInstantiation + $"({transpiledCallData})"
                            )
                            |> Option.defaultValue "" *)

                        Misc.adjustName Base.ExportedStatus.Exported accessorData.AttributeName + transpiledGenericsInstantiation + $"{transpiledCallData}")
                    |> String.concat "."
                moduleAccessorCallData.IdentifierName + "." + transpiledAccessors
        )
    transpiledPipes
    |> List.append [ transpiledBaseExpr ]
    |> List.reduce (fun previous transpiledFunctionCall -> transpiledFunctionCall.Replace("_", previous))

and transpileParentheses (input:ResolvedAst.ParenthesesData) = 
    let innerExpr = transpileExpression input.InnerExpression
    $"({innerExpr})"

and transpileOperatorSequence (input:ResolvedAst.OperatorSequenceData) = 
    let transpiledBaseExpr = transpileExpression input.BaseExpression
    let transpiledRest = 
        input.OperatorsAndExpressions
        |> List.map (fun data ->
            let expr = data.Expression
            let operator = data.Operator
            let transpiledExpression = transpileExpression expr
            operator + " " + transpiledExpression
        )
        |> String.concat ", "


    transpiledBaseExpr + transpiledRest

and transpileNumber (input:ResolvedAst.NumberData) = 
    let prefix = if input.IsNegative then "-" else ""
    let transpiledProperty = 
        match input.Property with
        | Some (Base.NumberProperty.Imaginary) -> "i"
        | Some (Base.NumberProperty.Unsigned) -> "u"
        | None -> ""
    prefix + input.StringRepresentation + transpiledProperty
    

and transpileNewFunction (input:ResolvedAst.NewFunctionData) = 
    let transpiledType = Ast.transpileType input.Type
    $"new({transpiledType})"

and transpileNegatedExpression (input:ResolvedAst.NegatedExpressionData) =
    "!" + transpileExpression input.Expression


and transpileWrapperTypeLiteral (input:ResolvedAst.WrapperTypeLiteralData) =
    let wrapperTypeData = input.WrapperType.WrapperDeclaration
    let transpiledWrapperTypeIdentifier = Ast.transpileTypeReference wrapperTypeData.ExportedStatus wrapperTypeData.Name input.WrapperType.GenericsInstantiation

    let transpiledExpression = transpileExpression input.Argument
    
    transpiledWrapperTypeIdentifier + $"({transpiledExpression})" 

and transpileModuleExprAccessor(input:ResolvedAst.ModuleAccessorDataExprArgs) = 
    let transpiledAccessors =
        input.Accessors
        |> List.map(fun accessorData -> 
            let transpiledCallData =
                match accessorData.Arguments with
                | Some data ->
                    let transpiledGenericsInstan = Ast.transpileOptionalGenericsInstantiation accessorData.GenericsInstantiation
                    let transpiledFunctionArguments = transpileExprArguments data
                    transpiledGenericsInstan + transpiledFunctionArguments
                | None -> ""
            Misc.adjustName Base.Exported accessorData.AttributeName + transpiledCallData        
            )
        |> List.rev
        |> String.concat "."
    
    input.IdentifierName + "." + transpiledAccessors 


and transpileExprAccessors (input:ResolvedAst.ExprArgsAccessorsData) = 
    let transpiledFirstExpr = transpileExpression input.BaseExpression
    let transpiledAccessors = transpileExpressionAccessors input.Accessors
    transpiledFirstExpr + "." + transpiledAccessors

and transpileMapLookupData (input:ResolvedAst.MapLookupData) = 
    let transpiledReference = transpileExpression input.Reference
    let transpiledLookupExpr = transpileExpression input.Lookup
    $"{transpiledReference}[{transpiledLookupExpr}]"


and transpileMapLiteralData(input:ResolvedAst.MapLiteralData) =
    let keyTypeName = Ast.transpileType input.KeyType
    let valueTypeName = Ast.transpileType input.ValueType
    let transpiledItems = 
        input.Items
        |> List.map (fun e ->
            let transpiledKey = transpileExpression e.Key
            let transpiledValue = transpileExpression e.Value
            $"{transpiledKey} : {transpiledValue}"
        )
        |> String.concat ", "

    $"map[{keyTypeName}]{valueTypeName}" + "{" + transpiledItems + "}"


and transpileMakeFunction(input:ResolvedAst.MakeFunctionData) =
    let typeName = Ast.transpileType input.Type
    let transpiledArguments = transpileExpressions input.Args ", "
    $"make({typeName}, {transpiledArguments})"

and transpileLambda (input:ResolvedAst.LambdaData) = 
    let transpiledParameters = BaseTranspilers.transpileParameters input.Parameters 
    let transpiledReturnType = Ast.transpileType input.ReturnType
    let transpiledContent = transpileScope input.ScopeData
    "func("
    + transpiledParameters
    + ")"
    + $" {transpiledReturnType}"
    + "{"
    + transpiledContent
    + "\n}"
          
and transpileIndex (input:ResolvedAst.IndexData) =
    let transpiledReferenceExpr = transpileExpression input.ReferenceExpr
    let transpiledLookupExpr = transpileExpression input.LookupExpr
    transpiledReferenceExpr + $"[{transpiledLookupExpr}]"

and transpilePossiblyCallableExpression (input:ResolvedAst.PossiblyCallableExpression) = 
    match input with
    | ResolvedAst.PossiblyCallableExpression.FunctionDeclaration(functionDec) ->
        Misc.adjustName functionDec.ExportedStatus functionDec.Name
    | ResolvedAst.PossiblyCallableExpression.Parentheses(expr) ->
        "(" + transpileExpression expr + ")"
    | ResolvedAst.PossiblyCallableExpression.Var(varData) ->
        varData.GetName()
    | ResolvedAst.PossiblyCallableExpression.BuiltInFunction(builtInFunctionData) ->
        builtInFunctionData.Name

and transpileExprFunctionCall (functionCallData:ResolvedAst.FunctionExpressionCallData) = 
    let transpiledReference = transpilePossiblyCallableExpression functionCallData.Reference
    let transpiledArguments = transpileExprArguments functionCallData.Arguments
    let transpiledGenericsInstantiation =
        functionCallData.GenericsInstantiation
        |> Ast.transpileOptionalGenericsInstantiation
    transpiledReference + transpiledGenericsInstantiation + transpiledArguments

and transpilePipeFunctionCall (functionCallData:ResolvedAst.FunctionPipeCallData) = 
    let transpiledReference = transpilePossiblyCallableExpression functionCallData.Reference
    let transpiledArguments = transpilePipeArguments functionCallData.Arguments
    let transpiledGenericsInstantiation =
        functionCallData.GenericsInstantiation
        |> Ast.transpileOptionalGenericsInstantiation
    transpiledReference + transpiledGenericsInstantiation + transpiledArguments




and transpileExprArguments (input:ResolvedAst.ExpressionCallArguments): string = 
   // let transpiledArguments =
    if input.Length = 0 then
        "()"
    else
        input
        |> List.map (fun argumentData ->
            let argument = argumentData.Argument
            let hasEllipsis = argumentData.HasEllipsis
            let transpiledArg = transpileExpression argument
            if hasEllipsis then "..." + transpiledArg else transpiledArg
        )
        |> String.concat ", "
        |> fun x -> $"({x})"

and transpileOptionalExprArguments (input':ResolvedAst.ExpressionCallArguments option): string = 
   match input' with
   | Some input ->
        if input.Length = 0
            then "()"
        else
            input
            |> transpileExprArguments
    | None ->
         ""


and transpilePipeArguments (input:ResolvedAst.PipeCallArguments): string = 
    input
    |> List.map (fun argumentData ->
        let argument = argumentData.Argument
        let hasEllipsis = argumentData.HasEllipsis
        match argument with
        | ResolvedAst.PipeFunctionCallArgument.Normal e ->
                transpileExpression e
        | ResolvedAst.PipeFunctionCallArgument.Placeholder -> "_"
        |> fun r -> if hasEllipsis then "..." + r else r
    )
    |> String.concat ", "
    |> fun x -> $"({x})"


and transpileExpressions (exprs:ResolvedAst.Expression list) (sep:string) = 
    exprs
    |> List.map transpileExpression
    |> String.concat sep

and transpileExpressionAccessors (accessors:list<ResolvedAst.AccessorItemExprArgs>) = 
    let transpiledAccessors =   
        accessors
        |> List.map (fun accessorData ->
            match accessorData with
            | ResolvedAst.AccessorItemExprArgs.InterfaceDeclaration(data) ->
                Misc.adjustName data.InterfaceDeclaration.ExportedStatus data.InterfaceDeclaration.Name
            | ResolvedAst.AccessorItemExprArgs.InterfaceMethod(data, genericsInstantiation', args') ->
                let transpiledCallData = transpileOptionalExprArguments args'
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + transpiledCallData
            | ResolvedAst.AccessorItemExprArgs.Method(data, genericsInstantiation', args') ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                let transpiledCallData = transpileOptionalExprArguments args'
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + transpiledCallData// + "(2)"
            | ResolvedAst.AccessorItemExprArgs.StructDeclaration(data) ->
                Misc.adjustName data.StructDeclaration.ExportedStatus data.StructDeclaration.Name
            | ResolvedAst.AccessorItemExprArgs.StructField(data, genericsInstantiation', args') ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                let transpiledCallData = transpileOptionalExprArguments args'
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + transpiledCallData
            (* | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name; CallData = Some callData) ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation callData.GenericsInstantiation
                let transpiledCallData = transpileFunctionArguments callData.FunctionArguments
                Misc.adjustName Base.ExportedStatus.Exported name + transpiledGenericsInstantiation + $"({transpiledCallData})"
            | ResolvedAst.AccessorItem.ItemDerivedFromModule(Name = name; CallData = None) ->
                Misc.adjustName Base.ExportedStatus.Exported name *)
        )
        |> List.rev
        |> String.concat "."
    transpiledAccessors


and transpilePipeFunctionCallArgumentAccessors (accessors:list<ResolvedAst.AccessorItemPipeArgs>) = 
    let transpiledAccessors =   
        accessors
        |> List.map (fun accessorData ->
            match accessorData with
            | ResolvedAst.AccessorItemPipeArgs.InterfaceDeclaration(data) ->
                Misc.adjustName data.InterfaceDeclaration.ExportedStatus data.InterfaceDeclaration.Name
            | ResolvedAst.AccessorItemPipeArgs.InterfaceMethod(data, genericsInstantiation', args) ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                let transpiledCallData = transpilePipeArguments args
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + transpiledCallData
            | ResolvedAst.AccessorItemPipeArgs.Method(data, genericsInstantiation', args) ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                let transpiledCallData = transpilePipeArguments args
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + $"{transpiledCallData}"
            | ResolvedAst.AccessorItemPipeArgs.StructDeclaration(data) ->
                Misc.adjustName data.StructDeclaration.ExportedStatus data.StructDeclaration.Name
            | ResolvedAst.AccessorItemPipeArgs.StructField(data, genericsInstantiation', args) ->
                let transpiledGenericsInstantiation = Ast.transpileOptionalGenericsInstantiation genericsInstantiation'
                let transpiledCallData = transpilePipeArguments args
                Misc.adjustName data.ExportedStatus data.Name + transpiledGenericsInstantiation + transpiledCallData
        )
        |> List.rev
        |> String.concat "."
    transpiledAccessors



and transpileArrayLiteral (arrayLiteralData:ResolvedAst.ArrayLiteralData) =
    let transpiledItems =
        transpileExpressions arrayLiteralData.Items ", "

    let transpiledTypeName = Ast.transpileType arrayLiteralData.ItemsType
    
    let transpiledLength = 
        match arrayLiteralData.Length with
        | Base.ArrayLiteralLength.Number(n) ->
            string n
        | Base.ArrayLiteralLength.Ellipsis -> "..."

    $"
    [{transpiledLength}]"
    + transpiledTypeName
    + "{"
    + transpiledItems
    + "}"


and transpileStatement (input:ResolvedAst.Statement) = 
    match input with
    | ResolvedAst.Statement.Break -> "break"
    | ResolvedAst.Statement.Continue -> "continue"
    | ResolvedAst.Statement.Defer(deferData) -> transpileDefer deferData
    | ResolvedAst.Statement.Force(forceData) -> transpileForce forceData
    | ResolvedAst.Statement.ForLoop(forLoopData) -> transpileForLoop forLoopData
    | ResolvedAst.Statement.Go(goData) -> transpileGo goData
    | ResolvedAst.Statement.If(ifData) -> transpileIf ifData
    | ResolvedAst.Statement.Return(returnData) -> transpileReturn returnData
    | ResolvedAst.Statement.Select(selectData) -> transpileSelect selectData
    | ResolvedAst.Statement.Try(tryData) -> transpileTry tryData
    | ResolvedAst.Statement.SwitchCase(switchCaseData) -> transpileSwitchCase switchCaseData
    | ResolvedAst.Statement.WhileLoop(whileLoopData) -> transpileWhile whileLoopData

and transpileWhile (input:ResolvedAst.WhileLoopData) =
    let transpiledConditional = transpileExpression input.Conditional
    let transpiledScope = transpileScope input.ScopeData
    "for {\n if !("
    + transpiledConditional
    + ")"
    + " { break }\n"
    + transpiledScope
    + "\n}"

and transpileWhen (input:ResolvedAst.WhenData) =
    let transpiledReturnType = Ast.transpileType input.ReturnType
    let transpiledWhenCases = 
        input.Cases
        |> List.map(fun caseData ->

            let transpiledCaseExpr = transpileExpression caseData.CaseExpr 
            let transpiledResultExpr = transpileExpression caseData.ResultExpr
            $"case {transpiledCaseExpr} : " + "{ return " + transpiledResultExpr + "}"
        )
        |> String.concat "\n"
    let transpiledExpr = transpileExpression input.Expression
    let transpiledDefaultCase = 
        input.DefaultExpression
        |> transpileExpression
        |> (fun resolvedExpr ->
            "\ndefault: { return " + resolvedExpr + " }"
        )

    let transpiledCases = transpiledWhenCases + transpiledDefaultCase

    $"(func() {transpiledReturnType}"
    + "{ switch "
    + transpiledExpr
    + " {\n"
    + transpiledCases
    + "\n}})()"

and transpileTry (input:ResolvedAst.TryData) =
    let r = new System.Random()
    let varName, errVarName =
        input.Identifiers
        |> Option.defaultValue (string <| r.Next(1_000, 1_000_000), string <| r.Next(1_000, 1_000_000))
    let transpiledExprToTry = transpileExpression input.ExpressionToTry
    let transpiledFailureExpr = transpileExpression input.FailureExpression
    $"{varName}, {errVarName} := {transpiledExprToTry}
      if {errVarName} != nil"
        + " {\n"
        + $"return {transpiledFailureExpr}"
        + "}"

and transpileSelect (input:ResolvedAst.SelectData) =
    let transpiledCases = 
        input.Cases
        |> List.map (fun caseData ->
            let transpiledExpression = transpileExpression caseData.Expression 
            let transpiledScope = transpileScope caseData.Scope
            let transpiledVariablePattern =
                caseData.VariablePattern
                |> Option.map (fun vPattern ->
                    let transpiledVp = String.concat ", " vPattern
                    $"{transpiledVp} := ")
                |> Option.defaultValue ""

            $"case {transpiledVariablePattern} {transpiledExpression} : " + "{" + transpiledScope + "}"
        )
        |> List.rev
        |> String.concat "\n"
    let transpiledDefaultCase = 
        input.DefaultCase
        |> Option.map (fun dc -> "default: {\n" + transpileScope dc + "\n}")
        |> Option.defaultValue ""

    let transpiledCases = transpiledCases + "\n" + transpiledDefaultCase 

    "select " + " {\n" + transpiledCases + "\n}"

and transpileReturn (input:ResolvedAst.ReturnData) =
    let transpiledExpr = 
        input.ReturnedExpression
        |> Option.map transpileExpression
        |> Option.defaultValue ""
    $"return {transpiledExpr}"

and transpileIf (input:ResolvedAst.IfData) =
    let transpiledIf = 
        let transpiledIfExpr = transpileExpression input.If.ConditionalExpr
        let transpiledIfScope = transpileScope input.If.ScopeData
        $"if {transpiledIfExpr}"
            + "{\n"
            + transpiledIfScope
            + "\n}"

    let transpiledElseIfs =
        input.ElseIfs
        |> Option.map (List.map (fun elseIfData ->
            let transpiledExpr = transpileExpression elseIfData.ConditionalExpr
            let transpiledScope = transpileScope elseIfData.ScopeData
            $"else if {transpiledExpr}"
                + "{\n"
                + transpiledScope
                + "\n}"
        ))
        |> Option.defaultValue []
        |> String.concat ""

    let transpiledElse =
        input.Else
        |> Option.map (fun elseData -> "else {\n" + transpileScope elseData.ScopeData + "\n}")
        |> Option.defaultValue ""
        

    transpiledIf + transpiledElseIfs + transpiledElse
   

and transpileGo (input:ResolvedAst.GoData) = 
    let transpiledScopeContent = transpileScope input
    ("go (func() () {\n" + transpiledScopeContent + "\n" + "\n})()")

and transpileForLoop (input:ResolvedAst.ForLoopData) = 
    let transpiledScope = transpileScope input.ScopeData
    match input.Style with
    | ResolvedAst.ForLoopStyle.ShortHand(vPattern, expr) ->
        let transpiledPattern = 
            vPattern
            |> List.map(_.Name)
            |> String.concat ", "

        $"for {transpiledPattern} := range {transpileExpression expr}"
            + " {\n"
            + transpiledScope
            + "\n}"
    | ResolvedAst.ForLoopStyle.Traditional((vPattern, expr1), expr2, expr3) ->
        let transpiledPattern = 
            vPattern
            |> List.map(_.Name)
            |> String.concat ", "
        let transpiledExpr1 = transpileExpression expr1
        let transpiledExpr2 = transpileExpression expr2
        let transpiledExpr3 = transpileExpression expr3
        $"for {transpiledPattern} := {transpiledExpr1}; {transpiledExpr2}; {transpiledExpr3}"
            + "{\n"
            + transpiledScope
            + "\n}"

and transpileForce ({Identifiers = (successVar, failureVar)} as input:ResolvedAst.ForceData) = 
    let r = new System.Random()
    let transpiledExprToTry = transpileExpression input.ExpressionToTry
    let transpiledPanicExpr = transpileExpression input.PanicExpression
    $"{successVar}, {failureVar} := {transpiledExprToTry}
      if {failureVar} != nil"
        + "{\npanic("
        + transpiledPanicExpr
        + ")\n}"

and transpileDefer (input:ResolvedAst.DeferData) = 
    let transpiledScopeContent = transpileScope input
    ("defer (func() () {\n" + transpiledScopeContent + "\n})()")

and transpileNonToplevelDeclaration (input:ResolvedAst.NonToplevelDeclaration) =
    match input with
    | ResolvedAst.NonToplevelDeclaration.NormalVar({Type = None; Expression = expr; VariablePattern = vp}) ->
        let transpiledVariablePattern = String.concat ", " vp
        let transpiledExpr = transpileExpression expr
        $"{transpiledVariablePattern} := {transpiledExpr}"
    | ResolvedAst.NonToplevelDeclaration.NormalVar({Type = Some t; Expression = expr; VariablePattern = vp}) ->
        let transpiledVariablePattern = String.concat ", " vp
        let transpiledType = Ast.transpileType t
        let transpiledExpr = transpileExpression expr
        $"var {transpiledVariablePattern} {transpiledType} = {transpiledExpr}"
    | ResolvedAst.NonToplevelDeclaration.ZeroVar(zeroVarData) ->
        let transpiledType = Ast.transpileType zeroVarData.Type
        let transpiledVarPattern = String.concat ", " zeroVarData.VariablePattern
        $"var {transpiledVarPattern} {transpiledType}"