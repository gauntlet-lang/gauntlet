(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.DeclarationParsers

open FParsec
open BaseParsers
open Types.Base
open Types
open Utils.Misc

let constParser =
    let constExprParser =
        choice
            [
              //iota >>. preturn Ast.ConstExpression.Iota
              str |>> Ast.ConstExpression.String
              booleanParser |>> Ast.ConstExpression.Boolean
              float |>> Ast.ConstExpression.Float
              attempt (negativeInteger <|> positiveInteger) |>> Ast.ConstExpression.Number
              identifier |>> Ast.ConstExpression.Identifier ]

    pipe3
        (pstr "const" >>. spaces1)
        (screamingSnakeCase .>> spaces .>> pchar '=' .>> spaces)
        constExprParser
        (fun _ name expr -> fun isExported -> Ast.Const((isExported, name, expr)))

let (normalVarDeclaration: Parser<Ast.NonToplevelDeclaration>) =
    pipe4
        (pstr "let" >>. spaces1 >>. maybeFn (typeParser .>> spaces1) Some None)
        (variableDeclarationPattern .>> spaces)
        (pchar '=' .>> sep)
        expressionParser
        (fun t' varName _ f -> Ast.NormalVar(t', varName, f))
    <??> "Variable declaration"

let (zeroVarDeclaration: Parser<Ast.NonToplevelDeclaration>) =
    pipe3
        (pstr "zero" >.< spaces1)
        (typeParser .>> spaces1)
        (variableDeclarationPattern)
        (fun _ typ varName -> Ast.ZeroVar(varName, typ))
    <??> "Zero variable declaration"

let (functionDeclaration: Parser<ExportedStatus -> Ast.ToplevelDeclaration>) =
    pipe5
        (pstr "fun" >>. spaces1 >>. typeParser .>> spaces1)
        (camelCase .>>. (spaces >>. tryParse generics [] .>> spaces))
        ((pchar '(' >>. (sepBy functParam (pstr "," >.< spaces)) .>> pchar ')')
         <??> "Return type annotation")
        spaces1
        constructsBetweenBraces
        (fun returnType (funName, generics) parameters _ contents ->
            fun isExported ->
                Ast.Function(isExported, funName, generics, parameters, returnType, (contents, newRandomNumber ())))


(* let (methodDeclaration: Parser<ExportedStatus -> Ast.ToplevelDeclaration>) =
    pipe4
        (pstr "def" >>. spaces1 >>. camelCase .>> spaces .>> pchar '(' .>> spaces)
        ((typeNameWithGenericsInstantiationData (preturn id)
          .>>. (spaces1 >>. identifierNameParser .>> spaces .>> pchar ',' .>> spaces))
         .>>. sepBy functParam (pstr "," .>> spaces))
        ((pchar ')' >>. spaces >>. pchar ':') >>. spaces >>. typeParser .>> spaces)
        constructsBetweenBraces
        (fun name ((receiver, receiverParameterName), parameters) returnType contents ->
            fun exportStatus ->
                Ast.ToplevelDeclaration.Method(
                    (exportStatus,
                     name,
                     (UnresolvedMethodReceiverParameter(receiver, receiverParameterName), parameters),
                     returnType,
                     (contents, newRandomNumber ()))
                )) *)

let (methodDeclaration: Parser<ExportedStatus -> Ast.ToplevelDeclaration>) =
    pipe4
        ((pstr "def" >>. spaces1 >>. typeParser) .>>. (spaces1 >>. camelCase .>> spaces .>> pchar '(' .>> spaces))
        ((typeNameWithGenericsInstantiationData (preturn id)) .>>. (spaces1 >>. identifierNameParser .>> spaces))
        ((tryParse (pchar ',' >.< spaces) ()) >>. sepBy functParam (pstr "," .>> spaces) .>> pchar ')' .>> spaces)
        constructsBetweenBraces
        (fun (returnType, name) (receiver, receiverParameterName) parameters contents ->
            fun exportStatus ->
                Ast.ToplevelDeclaration.Method(
                    (exportStatus,
                     name,
                     (UnresolvedMethodReceiverParameter(receiver, receiverParameterName), parameters),
                     returnType,
                     (contents, newRandomNumber ()))
                ))

// When code is working, fix the spaces
let (structDeclaration: Parser<ExportedStatus -> Ast.ToplevelDeclaration>) =
    let structConstruct =
        (attempt (maybeExport (attempt structFieldDeclarationWithTag <|> structFieldDeclarationWithoutTag)))
        <|> (typeNameWithGenericsInstantiationData (preturn id)
             |>> Ast.StructConstruct.Embedded)

    pipe4
        (pstr "struct" .>> spaces1)
        ((pascalCase .>> spaces)
         .>>. ((tryParse generics []) .>> spaces .>> pchar '{' .>> newlines))
        (sepByNewLines (structConstruct))
        (pchar '}')
        (fun _ (structName, generics) constructs _ ->
            fun exportStatus -> Ast.Struct(exportStatus, structName, generics, constructs))

let importDeclaration =
    ((pstr "import") >>. spaces1 >>. str .>> spaces1 .>> pstr "as")
    .>>. (spaces1 >>. flatCase)
    |>> Ast.Import
    <??> "Import declaration"

let interfaceDeclaration =

    (* let typeUnion =
        let typeTermParser =  .>>. typeParser 
        (sepBy (typeTermParser .>> spaces) (pstr "|" >.< spaces))

 *)
    let (embeddedType: Parser<Ast.InterfaceConstruct>) =
        typeNameWithGenericsInstantiationData (preturn id)
        |>> Ast.InterfaceConstruct.Embedded

    let typeSet = 
        pchar '|' >>. spaces >>. sepEndBy1 (maybe (pchar '~') true false .>>. typeParser .>> spaces) (pchar '|' >.< spaces)
        |>> Ast.InterfaceConstruct.TypeSet 

    let (interfaceMethod: Parser<ExportedStatus -> Ast.InterfaceConstruct>) =
        pipe4
            (typeParser .>> spaces1)
            (camelCase .>> spaces)
            (pchar '(' >>. spaces >>. (sepEndBy functParam (pstr "," >.< spaces)) .>> spaces)
            (pchar ')')
            (fun t methodName parameters _ ->
                fun isExported -> Ast.InterfaceConstruct.Method(isExported, methodName, parameters, t))


    pipe4
        (pstr "interface" >>. spaces)
        ((pascalCase .>> spaces) .>>. (tryParse generics []) .>> spaces)
        (pchar '{'
         >>. spaces
         >>. newlines
         >>. sepByNewLines ((typeSet <|> attempt (maybeExport interfaceMethod)) <|> embeddedType)
         .>> spaces)
        (pchar '}')
        (fun _ (name, generics) lines _ -> fun isExported -> Ast.Interface(isExported, name, generics, lines))

let alias =
    (pstr "alias"
     >>. spaces1
     >>. ((pascalCase .>> spaces) .>>. (tryParse generics []) ))
    .>>. (spaces >>. pchar '=' >>. spaces >>. typeParser)
    |>> (fun ((name, generics), t) -> fun isExported -> Ast.Alias(isExported, name, generics, t))

let wrappedType =
    pipe2
        (pstr "wrapper" >>. spaces1 >>. typeParser .>> spaces1)
        (pascalCase .>>. tryParse generics [])
        (fun t (name, generics) -> fun isExported -> Ast.WrapperType(isExported, name, generics, t))

let (packageDeclaration: Parser<PackageName>) =
    (pstr "package" >.< spaces) >>. flatCase .>> spaces <??> "Package declaration"

let initializeDeclarationParser () =

    toplevelDeclarationParserRef.Value <-
        choice
            [ maybeExport (
                  functionDeclaration
                <|> methodDeclaration
                  <|> structDeclaration
                  <|> interfaceDeclaration
                  <|> alias
                  <|> wrappedType
                  <|> constParser
              )
              importDeclaration ]

    nonToplevelDeclarationParserRef.Value <- choice [ normalVarDeclaration; zeroVarDeclaration ]
