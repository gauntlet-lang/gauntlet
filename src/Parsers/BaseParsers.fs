(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.BaseParsers

open FParsec
open Types
open Types.Base
open Utils.Misc

let (statementParser: Parser<Ast.Statement>), statementParserRef =
    createParserForwardedToRef ()

let (expressionParser: Parser<Ast.Expression>), expressionParserRef =
    createParserForwardedToRef ()

let (toplevelDeclarationParser: Parser<Ast.ToplevelDeclaration>), toplevelDeclarationParserRef =
    createParserForwardedToRef ()

let (nonToplevelDeclarationParser: Parser<Ast.NonToplevelDeclaration>), nonToplevelDeclarationParserRef =
    createParserForwardedToRef ()

let (scopeConstructParser: Parser<Ast.ScopeConstruct>), scopeConstructParserRef =
    createParserForwardedToRef ()

let (typeParser: Parser<UnresolvedType>), typeParserRef =
    createParserForwardedToRef ()

(* let (constructParser: Parser<LanguageConstruct>), constructParserRef =
    createParserForwardedToRef () *)

(* let (toplevelParser: Parser<LanguageConstruct>), toplevelParserRef =
    createParserForwardedToRef () *)

let (>.<) a b = pipe2 a b (fun _ _ -> ())

let spaces = many (pchar ' ') >>. preturn ()

let lowerLetters =
    [ 'a' .. 'z' ] |> List.map pchar |> choice <??> "lowercase letters"

let upperLetters =
    [ 'A' .. 'Z' ] |> List.map pchar |> choice <??> "uppercase letters"

let pstr = pstring

let (sep: Parser<unit>) = many (newline <|> pchar ' ') >>. preturn ()

let (operators: Parser<string> list) =
    [ "&&"
      "&"
      "=="
      "!="
      "-"
      "||"
      "|"
      "<="
      "*"
      "^"
      ">="
      "/"
      "<<"
      "<-"
      "<"
      "+"
      "%"
      ">>"
      ">"
      "--"
      "!"
      "&^"
      "~" ]
    |> List.map pstr


let (assignmentOperators: Parser<string> list) =
    [ "+="
      "&="
      "-="
      "|="
      "*="
      "^="
      "/="
      "<<="
      "%="
      ">>="
      "&^="
      "="
      "++" ]
    |> List.map pstr

let removeParserAndMakeParser (parsers: list<Parser<'T> * string>) (parserName: string) =
    parsers
    |> List.filter (fun (_, pn) -> pn <> parserName)
    |> List.map fst
    |> choice

let integers = [ 0..9 ] |> List.map string

let integerParser = integers |> List.map pstr |> choice 

let (camelCase: Parser<string>) =
    let tailChars = (lowerLetters <|> upperLetters <|> digit <|> pchar '_')
    (pstr "_") <|> (attempt <| many1Chars2 lowerLetters tailChars)
    <|> (pchar '_' .>>. many1 tailChars
         |>> (fun (a, b) -> string a + (List.map string >> String.concat "" <| b)))
    <??> "camelCase identifier "
    
    

let (pascalCase: Parser<string>) =
    many1Chars2 upperLetters (lowerLetters <|> upperLetters <|> digit)
    <??> "PascalCase identifier"

let ellipsisParser = pstr "..."

let screamingSnakeCase =
    many1 (upperLetters <|> pchar '_')
    |>> (fun x -> x |> List.map string |> String.concat "")

let (str: Parser<string>) =
    let stringMarker = pchar '"'

    pipe3 stringMarker (many (satisfy (fun c -> c <> '"'))) stringMarker (fun _ content _ ->
        content |> List.map string |> String.concat "")
    <??> "String"

let (newlines: Parser<_>) = many newline

let (backtickStr: Parser<string>) =
    pipe3 (pchar '`') (many (satisfy ((<>) '`'))) (pchar '`') (fun _ content _ ->
        content |> List.map string |> String.concat "")


let debug message = preturn () |>> fun _ -> printfn $"{message}"

let maybe<'a, 'b> (p: Parser<'a>) (successValue: 'b) (failureValue: 'b) =
    (attempt p >>. preturn successValue) <|> preturn failureValue

let maybeFn<'a, 'b> (p: Parser<'a>) (successFn: 'a -> 'b) (failureValue: 'b) =
    (attempt p |>> successFn) <|> preturn failureValue

let tryParse p failureValue = attempt p <|> preturn failureValue

let maybeExport p =
    let exportKeyword = "export"

    (maybe (pstr exportKeyword) Exported Unexported .>> spaces) .>>. p
    |>> (fun (isExported, parserResult) -> parserResult isExported)



let (negativeInteger: Parser<Ast.NumberData>) =
    pipe3
        (pchar '-')
        (many1 (choice (List.map pstr integers)))
        ((maybeFn (pchar 'u') (fun _ -> Some Base.Unsigned) None)
        <|> (maybeFn (pchar 'i') (fun _ -> Some Base.Imaginary) None))
        (fun _ r numberProperty -> true, String.concat "" r, numberProperty)
    <??> "Negative Integer"

let (positiveInteger: Parser<Ast.NumberData>) =
    pipe2
        (many1 (choice (List.map pstr integers)))
        ((maybeFn (pchar 'u') (fun _ -> Some Base.Unsigned) None)
        <|> (maybeFn (pchar 'i') (fun _ -> Some Base.Imaginary) None))
        (fun r numberProperty -> false, String.concat "" r, numberProperty)
    <??> "Postive Integer"

let (float: Parser<Ast.FloatData>) =
    pipe3 (maybe (pchar '-') true false) pfloat (maybe (pchar 'i') true false) (fun isNeg point isImaginary ->
        isNeg, point, isImaginary)
    <??> "Float"

let betweenParentheses inside =
    (pchar '(' >>. spaces) >>. (inside .>> spaces) .>> (pchar ')') <??> "Parenthesis"

let (identifier: Parser<string>) = camelCase


let (generics: Parser<Generics>) =
    pchar '['
    >>. spaces
    >>. (sepBy ((typeParser .>> spaces) .>>. pascalCase) (pchar ',' >.< spaces))
    .>> pchar ']'
    <??> "Generic parameters"

let (genericsInstantiation: Parser<GenericsInstantiationData>) =
    pchar '[' >>. spaces >>. (sepBy (typeParser) (spaces >.< pchar ',' >.< spaces))
    .>> pchar ']'

let typeNameWithGenericsInstantiationData (suffixParser: Parser<TypeNameWithGenericsInstantiationData -> 'a>) =
    pipe3 pascalCase (tryParse genericsInstantiation []) suffixParser (fun typeName g s -> s (typeName, g))


let exprArgumentsCall (* (atLeast1Arg:bool) *) =
    pipe3
        (pchar '(' >.< spaces)
        ((sepBy) (expressionParser .>>. (maybe ellipsisParser true false)) (pstr "," .>> spaces))
        (pchar ')')
        (fun  _ arguments _ -> arguments:Ast.ExpressionCallArguments)

let pipeArgParser =
        ((pchar '_' |>> fun _ -> Ast.PipeFunctionCallArgument.Placeholder))
        <|> (expressionParser |>> Ast.PipeFunctionCallArgument.Normal)
        
let flatCase = many1 lowerLetters |>> List.map string |>>  String.concat ""

let pipeArgumentsCall (* (atLeast1Arg:bool) *) =
    pipe3
        (pchar '(' >.< spaces)
        ((sepBy) (pipeArgParser .>>. (maybe ellipsisParser true false)) (pstr "," .>> spaces))
        (pchar ')')
        (fun  _ arguments _ -> arguments:Ast.PipeCallArguments)

let wordAlone (s:string) = pstr s .>> notFollowedBy (camelCase <|> pascalCase <|> integerParser) 

let functionArgumentsWithoutGenericsInstantiation (argumentParser: Parser<'T>) =
    pipe3
        (pchar '(' >.< spaces)
        (sepBy (argumentParser .>>. (maybe ellipsisParser true false)) (pstr "," .>> spaces))
        (pchar ')')
        (fun _ _ arguments _ -> arguments)

let (booleanParser: Parser<Ast.BooleanData>) =
    choice [
        (wordAlone "true" >>. preturn true)
        (wordAlone "false" >>. preturn false)
    ]
     

let sepByNewLines p =
    choice [
        attempt (spaces >>. p .>> notFollowedBy (sep >.< p) |>>(fun x -> [x])) // Allow for one liners if just one construct
        (many1 (newline >>. preturn None <|> (spaces >>. p |>> Some)) |>> List.choose id)
    ]

//many (newline >>. preturn None <|> (spaces >>. p |>> Some)) |>> List.choose id


// Parse the NAME only
let identifierNameParser = camelCase


// For parsing a pattern of variables
let (variableDeclarationPattern: Parser<VariablePattern>) =
    identifierNameParser
    .>>. many (attempt (pchar ',' >>. spaces >>. identifierNameParser .>> spaces))
    |>> (fun (f, rest) -> List.append [ f ] rest)
    <??> "Variable declaration pattern"

let structIdentifier = pascalCase

let (structLiteralField: Parser<Ast.StructLiteralField>) =
    pipe3 (pascalCase .>> spaces) (pchar '=' >>. spaces) expressionParser (fun fieldName _ value -> (fieldName, value))

let (structLiteral: Parser<TypeNameWithGenericsInstantiationData -> Ast.StructLiteralData>) =
    pipe3
        (pchar '{' >>. spaces)
        (sepBy structLiteralField ((((pchar ',' >.< spaces) <|> (newline >>. preturn ())) >>. preturn ()))
         .>> spaces)
        (pchar '}')
        (fun _ fields _ -> fun typeIdentifier -> UnresolvedStructReference(typeIdentifier), fields)


let constructsBetweenBraces =
    pipe3 (pchar '{' >.< sep) (sepByNewLines scopeConstructParser .>> sep) ((pchar '}')) (fun _ r _ -> r)

let (functParam: Parser<Parameter>) =
    pipe2
        ((maybe ellipsisParser true false .>>. typeParser) .>> spaces1)
        identifierNameParser
        (fun (hasEllipsis, t) varName -> hasEllipsis, t, varName)


let (structFieldDeclarationWithoutTag: Parser<ExportedStatus -> Ast.StructConstruct>) =
    pipe2 (typeParser .>> spaces) pascalCase (fun t name ->
        fun exportStatus -> Ast.Field(exportStatus, (t, name), None))

let (structFieldDeclarationWithTag: Parser<ExportedStatus -> Ast.StructConstruct>) =
    pipe3 (typeParser .>> spaces) (pascalCase .>> spaces) (backtickStr <??> "Struct tag") (fun t name tag ->
        fun exportStatus -> Ast.Field(exportStatus, (t, name), Some tag))

let (iota:Parser<_>) = 
    wordAlone "iota"
