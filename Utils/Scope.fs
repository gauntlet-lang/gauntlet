(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Utils.Scope

open Types.Base
open Utils
open Ast
open Types


(* /// Returns the given scope followed by it's outer scopes
let rec getScopePath
    (targetScopeId: ScopeId)
    (givenScopes: ResolvedAst.ScopeData list)
    (parentScopes: ResolvedAst.ScopeData list)
    : ResolvedAst.ScopeData list =
    let mutable listToReturn = []
    let mutable shouldContinue = true

    for scope in givenScopes do
        if shouldContinue then
            if scope.Id = targetScopeId then
                listToReturn <- Misc.listAdd parentScopes scope
                shouldContinue <- false
            else
                listToReturn <- getScopePath targetScopeId (* scope.Id *) scope.Scopes (Misc.listAdd parentScopes scope)
                shouldContinue <- listToReturn.Length = 0

    listToReturn

let getScope (targetScopeId: ScopeId) (scopes: ResolvedAst.ScopeData list) =
    getScopePath targetScopeId scopes [] |> List.head

let getNestedScope (targetScopeId: ScopeId) (scope: ResolvedAst.ScopeData) =
    List.tryFind (fun ({ Id = scopeId }: ResolvedAst.ScopeData) -> scopeId = targetScopeId) scope.Scopes
    |> Misc.expect $"Unable to find scope with ID '{targetScopeId}' from previous scope with ID '{scope.Id}'"

/// Gets all variables in a given scope- even the ones in scopes above it
let getAllVariables (targetScopeId: ScopeId) (scopes: ResolvedAst.ScopeData list) (toplevelVars: IdentifierData list) =
    getScopePath targetScopeId scopes []
    |> List.map (fun scope -> scope.Variables)
    |> Misc.flattenList
    |> List.append toplevelVars


/// Put all scopes and nested scopes on top-level
let rec flatten (inputScopes: ResolvedAst.ScopeData list) : ResolvedAst.ScopeData list =

    let mutable newScopeList = []

    newScopeList <- List.append newScopeList inputScopes

    for scope in inputScopes do
        if scope.Scopes.Length > 0 then
            newScopeList <- List.append (flatten scope.Scopes) newScopeList
        else
            newScopeList <- List.append scope.Scopes newScopeList

    newScopeList

/// Get all variable names in scope
let extractVariableNames (input: Ast.ScopeContent) : IdentifierName list =
    if input.Length = 0 then
        []
    else
        input
        |> List.choose (function
            | VariableDeclaration(variablePattern, _) -> Some variablePattern
            | ScopeDeclaration(_)
            | CasesDeclaration(_)
            | NonDeclaratory(_) -> None)
        |> Misc.flattenList

let modifyContext ((a, b, _): Ast.Context) (newScopeData: ResolvedAst.ScopeData) : Ast.Context = a, b, newScopeData

/// Extracts variables from scope in the form of IdentifierData
let extractVariables
    (input: Ast.ScopeContent)
    ((astData, _, aboveScopeData) as context: Ast.Context)
    : IdentifierData list =

    (* if input.Length = 0 then
        []
    else
        input
        |> List.choose(function
            | ScopedDeclaration(NormalVar(names, _)) ->
                Some names
        ) *)
    let identifiersInScope = input |> extractVariableNames

    let mutable mappedIdentifiers = []

    for identifier in identifiersInScope do
        mappedIdentifiers <-
            Misc.listAdd
                mappedIdentifiers
                { VarName = identifier
                  VarType =
                    getTypeFromExpression
                        (Ast.Expression.Identifier(identifier))
                        astData
                        (List.append mappedIdentifiers aboveScopeData.Variables)
                  ExportStatus = NonExportable }

    mappedIdentifiers


/// Gets a variable. Errors if not found
let getIdentifierData<'a>
    (varName: IdentifierName)
    (scopes: ResolvedAst.ScopeData list)
    (toplevelVars: IdentifierData list)
    (scopeIdOpt: ScopeId option)
    : Orsak.Effect<'a, IdentifierData, unit> =

    let scopeVariables =
        scopeIdOpt
        |> Option.map (fun id -> getAllVariables id scopes toplevelVars)
        |> Option.defaultValue ([])


    toplevelVars
    |> List.append scopeVariables
    |> List.tryFind (fun var -> var.VarName = varName)
    |> fun r ->
        match r with
        | Some v -> Misc.lift v
        | None ->
            match scopeIdOpt with
            | Some id -> failwith $"Unable to find variable '{varName}' in scope {id} or global scope."
            | None -> failwith $"Unable to find variable '{varName}' in global scope"

// This may not work yet. Try it to test and see
let getVariableFromScope (variableName: IdentifierName) (targetScopeId: ScopeId) (astData: Ast.ASTData) =

    let rec getVariableFromScope
        (variableName: IdentifierName)
        (targetScopeId: ScopeId)
        (scopeData: ResolvedAst.ScopeData)
        =
        let mutable targetVariable = None

        for scope in scopeData.Scopes do
            if scope.Id = targetScopeId then
                let variableOpt =
                    List.tryFind (fun identifierData -> identifierData.VarName = variableName) scope.Variables

                match variableOpt with
                | Some variable -> targetVariable <- Some variable
                | None ->
                    for scope in scope.Scopes do
                        targetVariable <- getVariableFromScope variableName targetScopeId scope

        targetVariable


    List.tryPick (getVariableFromScope variableName targetScopeId) astData.Scopes

let getVariableExportedStatus (vars: ScopeVariables) (targetVarName: string) : ExportedStatus =
    vars
    |> List.tryPick (fun { VarName = vn; ExportStatus = es } ->
        let varNameMatches = vn = targetVarName

        match es with
        | Exportable isExported when varNameMatches -> Some isExported
        | _ -> None)
    |> Option.defaultValue Unexported

(* let addVariableToContext ((astData, transpilers, scopeData) as context: Context) (newVar:IdentifierData): Context = 
    let newScopeData = {scopeData with Variables = Misc.listAdd scopeData.Variables newVar}
    astData, transpilers, newScopeData

let addVariablesToContext
    ((astData, transpilers, scopeData) as context: Context)
    (scopeContent: ScopeContent)
    : Context =
    let newVariables = extractVariables scopeContent context
    let newScopeData = {scopeData with Variables = List.append scopeData.Variables newVariables}
    let newContext = astData, transpilers, newScopeData

    (* for construct in scopeContent do
        match construct with
        | ScopedDeclaration(NormalVar(names, expr)) ->
            for name in names do
                variables <-
                    Misc.listAdd
                        variables
                        { VarType = Ast.getTypeFromExpression expr variables astData.FieldsData
                          VarName = name
                          ExportStatus = NonExportable }
        | ScopedDeclaration(ZeroVar(names, t)) ->
            for name in names do
                variables <-
                    Misc.listAdd
                        variables
                        { VarType = t
                          VarName = name
                          ExportStatus = NonExportable }
        | ScopedSatement(Try())
        | _ -> () *)
    newContext
     *)

let getToplevelScope (input: ResolvedAst.ScopeData list) (targetScopeId: ScopeId) =
    List.find (fun (s: ResolvedAst.ScopeData) -> s.Id = targetScopeId) input
 *)