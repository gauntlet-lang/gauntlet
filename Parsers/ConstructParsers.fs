(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.ConstructParsers

open BaseParsers
open Types
open FParsec
open DeclarationParsers
open ExpressionParsers
open StatementParsers
open TypeParsers

initializeExpressionParser ()
initializeDeclarationParser ()
initializeStatementParser ()
initializeTypeParser ()

(* toplevelDeclarationParserRef.Value <-
    (statementParser |>> StatementType)
    <|> (toplevelDeclarationParser |>> ToplevelDeclarationType)
    <|> (expressionParser |>> ExpressionType) *)

let initializeScopeConstructParser () =
    let nestedDeclarationParser = zeroVarDeclaration <|> normalVarDeclaration

    scopeConstructParserRef.Value <-
        choice
            [ (statementParser |>> Ast.ScopedSatement)
              (nestedDeclarationParser |>> Ast.ScopedDeclaration)
              (expressionParser |>> Ast.ScopedExpression) ]


initializeScopeConstructParser ()


let gauntletParser =
    (packageDeclaration) .>>. (sepByNewLines toplevelDeclarationParser) .>> eof
    |>> fun (pn, ast) -> pn, ast
