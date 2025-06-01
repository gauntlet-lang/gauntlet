(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Transpilers.ToplevelDeclarationTranspilers

open Types
open Utils
open Transpilers.BaseTranspilers
open Transpilers.ScopeTranspilers
open Transpilers.BaseTranspilers
open FsToolkit.ErrorHandling
open Utils
open Misc

let transpileConstExpression (input:ResolvedAst.ConstExpression) = 
    match input with
    | ResolvedAst.ConstExpression.Boolean(b) ->
        if b.Value then "true" else "false"
    | ResolvedAst.ConstExpression.Const(constDeclarationData) ->
        constDeclarationData.IdentifierName 
    | ResolvedAst.ConstExpression.Float(f) ->
        transpileFloat f
    | ResolvedAst.ConstExpression.Number(n) ->
        transpileNumber n
    | ResolvedAst.ConstExpression.String(s) ->
        "\"" + s + "\""
    //| ResolvedAst.ConstExpression.Iota -> "iota"
    

let transpileToplevelDeclaration (input:ResolvedAst.ToplevelDeclaration) = 
    match input with
    | ResolvedAst.ToplevelDeclaration.Alias(aliasData) ->
        $"type {Misc.adjustName aliasData.ExportedStatus aliasData.Name} = {Ast.transpileType aliasData.Type}"
    | ResolvedAst.ToplevelDeclaration.Const(constData) ->
        $"const {Misc.adjustName constData.ExportedStatus constData.IdentifierName} = {transpileConstExpression constData.Expression}"
    | ResolvedAst.ToplevelDeclaration.Function(functionData) ->
        //let scopeVariables = functionData.ScopeData.Variables.GetAllVariables()
        //let variableDiscarder = createVariableDiscarder scopeVariables
        let resolvedScope = transpileScope functionData.ScopeData
        
        $"
        func {Misc.adjustName functionData.ExportedStatus functionData.Name}{transpileGenerics functionData.Generics}({transpileParameters functionData.Parameters}) {Ast.transpileType functionData.ReturnType}"
        + " {"
        + $"
        {resolvedScope}
        "
        + "\n}"
    
    | ResolvedAst.ToplevelDeclaration.Import(importData) ->
        $"import {importData.IdentifierName} \"{importData.ModuleName}\""
    | ResolvedAst.ToplevelDeclaration.Interface(interfaceData) ->
        let transpiledActualMethods = 
            interfaceData.ActualMethods
            |> List.map (fun d ->
                let transpiledName = Misc.adjustName d.ExportedStatus d.Name
                let transpiledParameters = transpileParameters d.Parameters
                let transpiledRT = Ast.transpileType d.ReturnType
                $"{transpiledName}({transpiledParameters}) {transpiledRT}"
            )
            |> String.concat "\n"
            
        let transpiledInterfaces = 
            interfaceData.EmbeddedInterfaces
            |> List.map (fun d ->
                let transpiledGenericsInstan = Ast.transpileGenericsInstantiation d.GenericsInstantiation
                let transpiledName = Misc.adjustName d.InterfaceDeclaration.ExportedStatus d.InterfaceDeclaration.Name
                transpiledName + transpiledGenericsInstan
            )
            |> String.concat "\n"
        
        let transpiledStructs = 
            interfaceData.EmbeddedStructs
            |> List.map (fun d ->
                let transpiledGenericsInstan = Ast.transpileGenericsInstantiation d.GenericsInstantiation
                let transpiledStructName = Misc.adjustName d.StructDeclaration.ExportedStatus d.StructDeclaration.Name
                transpiledStructName + transpiledGenericsInstan
            )
            |> String.concat "\n"

        let transpiledTypeSets = 
            interfaceData.TypeSets
            |> List.map (fun d ->
                let transpiledTypes =
                    d.Types
                    |> List.map (fun d ->
                        let prefix = if d.IncludeUnderlyingType then "~" else ""
                        let transpiledType = Ast.transpileType d.Type
                        prefix + transpiledType
                        )
                    |> String.concat "|"
                transpiledTypes
                )
                |> String.concat "\n"
        
        let transpiledConstructs = 
            $"{transpiledActualMethods}\n {transpiledInterfaces}\n {transpiledStructs}\n {transpiledTypeSets}"


        $"type {Misc.adjustName interfaceData.ExportedStatus interfaceData.Name}{transpileGenerics interfaceData.Generics} interface "
        + "{\n"
        + transpiledConstructs
        + "}"

    | ResolvedAst.ToplevelDeclaration.Struct(structData) ->
        
        let transpiledActualFields = 
            structData.ActualFields
            |> List.map (fun fd ->
                let transpiledName = Misc.adjustName fd.ExportedStatus fd.Name
                let transpiledTag =
                    fd.Tag
                    |> Option.map (fun x -> $"`{x}`")
                    |> Option.defaultValue "" 
                let transpiledType = Ast.transpileType fd.Type
                $"{transpiledName} " + $"{transpiledType} " + transpiledTag
            )
            |> String.concat "\n"
        
        let transpiledStructs = 
            structData.EmbeddedStructs
            |> List.map (fun d ->
                let transpiledGenericsInstan = Ast.transpileGenericsInstantiation d.GenericsInstantiation
                let transpiledStructName = Misc.adjustName d.StructDeclaration.ExportedStatus d.StructDeclaration.Name
                transpiledStructName + $"{transpiledGenericsInstan}"
            )
            |> String.concat "\n"

        let transpiledInterfaces = 
            structData.EmbeddedInterfaces
            |> List.map (fun d ->
                let transpiledGenericsInstan = Ast.transpileGenericsInstantiation d.GenericsInstantiation
                let transpiledName = Misc.adjustName d.InterfaceDeclaration.ExportedStatus d.InterfaceDeclaration.Name
                transpiledName + $"{transpiledGenericsInstan}"
            )
            |> String.concat "\n"
        
        let transpiledConstructs = 
            $"{transpiledActualFields}\n {transpiledInterfaces}\n {transpiledStructs}"

        $"
        type {Misc.adjustName structData.ExportedStatus structData.Name}{transpileGenerics structData.Generics} struct"
                + "{"
                + $"
        {transpiledConstructs} 
        " + "}"
    
    | ResolvedAst.ToplevelDeclaration.Method(methodData) -> 
        let transpiledReceiverType =
            match methodData.ReceiverType with
            | ResolvedAst.MethodReceiverType.WrapperTypeReceiver(d, genericsInstantiationData) ->
                Ast.createWrapperTypeFromDeclaration d genericsInstantiationData 
            | ResolvedAst.MethodReceiverType.StructReceiver(d, genericsInstantiationData) ->
                Ast.createStructTypeFromDeclaration d genericsInstantiationData
            |> Ast.transpileType
            
        let transpiledScope = 
            transpileScope methodData.ScopeData
        
        let scopeVariables = methodData.ScopeData.Variables.GetAllVariables()
        //let variableDiscarder = createVariableDiscarder scopeVariables

    
            
        $"func ("
        + methodData.ReceiverParameterName + " " + transpiledReceiverType
        + $") {Misc.adjustName methodData.ExportedStatus methodData.Name}({transpileParameters methodData.Parameters}) {Ast.transpileType methodData.ReturnType}"
        + "{\n"
        + transpiledScope
//        + $"\n{variableDiscarder}"
        + "\n}"
    

    | ResolvedAst.ToplevelDeclaration.WrapperType(wrapperTypeData) -> 
        $"type {Misc.adjustName wrapperTypeData.ExportedStatus wrapperTypeData.Name}{transpileGenerics wrapperTypeData.Generics} {Ast.transpileType wrapperTypeData.Type}"

       
        
