(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.StatementParsers

open FParsec
open BaseParsers
open Types
open Utils.Misc
open Types.Base


let switchCase =
    let defaultCase =
            pipe2 (pstr "default" >.< spaces >.< pstr ":" >.< sep) constructsBetweenBraces (fun _ content ->
                Ast.DefaultCase(content, newRandomNumber ()))
    
    let switchExpressionCasesParser =
        let regularCase =
            pipe3 (pstr "case" >.< spaces ) (expressionParser .>> sep .>> pchar ':' .>> spaces) constructsBetweenBraces (fun _ expr content ->
                Ast.RegularCase(expr, (content, newRandomNumber ())))
        sepByNewLines regularCase .>>. maybeFn defaultCase Some None

    let switchTypeCasesParser = 
        let typeCase =
            pipe3 (pstr "case" >.< spaces ) (typeParser .>> sep .>> pchar ':' .>> spaces) constructsBetweenBraces (fun _ expr content ->
                Ast.TypeSwitchCase(expr, (content, newRandomNumber ())))
        sepByNewLines typeCase .>>. maybeFn defaultCase Some None
            

    (* pipe3
            (spaces >.< ((pstr "case" .>> spaces1)))
            (maybeFn expressionParser Some None .>> spaces .>> pchar ':' .>> spaces)
            constructsBetweenBraces
            (fun _ conditionOpt content ->
                match conditionOpt with
                | Some condition -> Ast.RegularCase(condition, (content, newRandomNumber ()))
                | None -> Ast.DefaultCase(content, newRandomNumber ())) *)

    let expressionSwitch = 
        pipe5
            //(pstr "switch" >>. ((pchar '(' >>. typeNameParser .>> pchar ')')) .>> spaces1)
            (pstr "switch" .>> spaces1)
            (expressionParser .>> spaces)
            (pchar '{' >.< spaces)
            (switchExpressionCasesParser)
            (sep >.< pchar '}')
            (fun _ expr _ (regularCases, defaultCase') _ ->
                Ast.SwitchCaseType.ExpressionSwitch(expr, regularCases, defaultCase'))
    let typeSwitch = 
        pipe5
            //(pstr "switch" >>. ((pchar '(' >>. typeNameParser .>> pchar ')')) .>> spaces1)
            (pstr "switch" >.< spaces1 >.< pstr "typeof" >.< spaces1)
            (expressionParser .>> spaces)
            (pchar '{' >.< spaces)
            (switchTypeCasesParser)
            (sep >.< pchar '}')
            (fun _ expr _ (regularCases, defaultCase') _ ->
                Ast.SwitchCaseType.TypeSwitch(expr, regularCases, defaultCase'))

    attempt expressionSwitch <|> typeSwitch |>> Ast.Statement.SwitchCase
        

let tryStatement =
    pipe3
        (maybeFn
            (pstr "let"
             >>. spaces
             >>. (identifierNameParser .>>. (pchar ',' >>. spaces >>. identifierNameParser))
             .>> spaces
             .>> pstr "="
             .>> spaces)
            Some
            None)
        (pstr "try" >>. spaces >>. expressionParser)
        (spaces >>. pstr "with" >>. spaces >>. expressionParser)
        (fun variablePattern expr returnExpr -> Ast.Statement.Try(variablePattern, expr, returnExpr))
    <??> "Try-statement"

let forceStatement =
    pipe3
        (pstr "let"
            >>. spaces1
            >>. (identifierNameParser .>>. (pchar ',' >>. spaces >>. identifierNameParser) )
            .>> spaces
            .>> pstr "="
            .>> spaces)
        (pstr "force" >>. spaces >>. expressionParser)
        (spaces >>. pstr "with" >>. spaces >>. expressionParser)
        (fun variablePattern expr returnExpr -> Ast.Statement.Force(variablePattern, expr, returnExpr))
    <??> "Force-statement"

let returnStatement =
    pstr "return" >>. spaces1 >>. maybeFn expressionParser Some None
    |>> Ast.Statement.Return
    <??> "Return statement"

let forLoop =
    let traditionalInput =
        pipe4
            (variableDeclarationPattern .>> spaces .>> pchar '=' .>> spaces)
            (expressionParser .>> spaces)
            (pchar ';' >>. spaces >>. expressionParser)
            (pchar ';' >>. spaces >>. expressionParser)
            (fun variables expr expr1 expr2 -> Ast.ForLoopStyle.Traditional((variables, expr), expr1, expr2))

    let shorthandInput =
        pipe3 (variableDeclarationPattern .>> spaces) (pstr "in" >.< spaces) (expressionParser) (fun variables _ expr ->
            Ast.ForLoopStyle.ShortHand(variables, expr))

    let input = attempt traditionalInput <|> shorthandInput

    pipe2 (pstr "for let" >>. spaces >>. input) (spaces >>. constructsBetweenBraces) (fun style content ->
        Ast.Statement.ForLoop(style, (content, newRandomNumber ())))

let ifStatement =
    pipe3
        ((pstr "if" >>. spaces1 >>. expressionParser) .>>. (spaces >>. constructsBetweenBraces) .>> sep)
        (maybeFn (many ((pstr "elif" >>. spaces1 >>. expressionParser .>> spaces) .>>. constructsBetweenBraces .>> sep)) Some None)
        (maybeFn ((pstr "else" >.< spaces) .>>. constructsBetweenBraces) Some None)
        (fun (ifExpr, ifContent) ifElse' else' -> Ast.Statement.If(
            (ifExpr, (ifContent, newRandomNumber ())),
            ifElse' |> Option.map (fun ifElse ->
                ifElse |> List.map (fun (ifElseExpr, ifElseContent) -> ifElseExpr, (ifElseContent, newRandomNumber ()))
            ),
            else' |> Option.map (fun (elseExpr, elseContent) -> elseContent, newRandomNumber ())
            ))
        <??> "If statement"


let selectCaseStatement =
    let casesParser =
        let regularCase =
            pipe4
                (pstr "case" >>. spaces1)
                (maybeFn (pstr "let" >>. spaces1 >>. variableDeclarationPattern .>> spaces .>> pstr "=" .>> spaces) Some None)
                (expressionParser .>> spaces .>> pchar ':')
                (spaces >>. constructsBetweenBraces)
                (fun _ identifier' expr content ->
                    Ast.RegularSelectCase(identifier', expr, (content, newRandomNumber ())))

        let defaultCase =
            pstr "default" >>. spaces >>. pchar ':' >>. spaces >>. constructsBetweenBraces
            |>> fun content -> Ast.DefaultCase(content, newRandomNumber ())


        sepByNewLines regularCase .>>. maybeFn defaultCase Some None


    (* pipe3
            (spaces >.< ((pstr "case" .>> spaces1) <|> pstr "default"))
            (maybeFn expressionParser Some None .>> spaces .>> pchar ':' .>> spaces)
            constructsBetweenBraces
            (fun _ conditionOpt content ->
                match conditionOpt with
                | Some condition -> Ast.CaseType.NormalCase(condition, (content, newRandomNumber ()))
                | None -> Ast.CaseType.DefaultCase(content, newRandomNumber ())) *)

    pipe4 (pstr "select" >.< spaces1) (pchar '{' >.< sep) (casesParser) (sep >.< pchar '}') (fun _ _ cases _ ->
        Ast.Statement.Select(cases))


let whileLoop =
    pipe2
        (pstr "while" >>. spaces1 >>. expressionParser .>> spaces)
        constructsBetweenBraces
        (fun condition constructs -> Ast.Statement.WhileLoop(condition, (constructs, newRandomNumber ())))


let (breakKeyword: Parser<Ast.Statement>) =
    pstr "break" >>. preturn Ast.Statement.Break

let (continueKeyword: Parser<Ast.Statement>) = 
    pstr "continue" >>. preturn Ast.Statement.Continue

let go =
    pstr "go" >>. spaces >>. constructsBetweenBraces
    |>> fun s -> Ast.Statement.Go(s, newRandomNumber ())

let defer =
    pstr "defer" >>. spaces >>. constructsBetweenBraces
    |>> fun s -> Ast.Statement.Defer(s, newRandomNumber ())

let initializeStatementParser () =

    statementParserRef.Value <-
        choice
            [ attempt tryStatement
              returnStatement
              forLoop
              attempt forceStatement
              whileLoop
              ifStatement
              selectCaseStatement
              switchCase
              breakKeyword
              continueKeyword
              go
              defer ]
