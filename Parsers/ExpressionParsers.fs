(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.ExpressionParsers

open FParsec
open Types
open Parsers.BaseParsers
open Types.Base
open Utils.Misc

let mutable (expressionParsers: ((Parser<Ast.Expression> * string) list)) = []

(* let operatorParser = OperatorPrecedenceParser<float, unit, unit>()

operatorParser.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> 1)) *)



(* let booleanReference =
    (booleanParser |>> BooleanReference.Bool)
    <|> (identifierNameParser |>> BooleanReference.IdentifierName) *)

let makeFunction =
    pipe3
        (pstr "make" >>. spaces >>. pchar '(' >>. spaces >>. typeParser)
        (maybeFn (attempt (pchar ',') >>. ((sepEndBy (spaces >>. expressionParser) (pchar ',')))) Some None)
        (pchar ')')
        (fun t exprs _ -> Ast.MakeFunction(t, Option.defaultValue [] exprs))

let newFunction =
    pipe2 (pstr "new" >>. spaces >>. pchar '(' >>. spaces >>. typeParser) (pchar ')') (fun t _ -> Ast.NewFunction(t))


let ternaryOperator =
    pipe5
        ((pchar '@' >>. typeParser .>> spaces1) .>>. (expressionParser .>> spaces))
        (pchar '?' .>> spaces)
        (expressionParser .>> spaces)
        (pchar ':' .>> spaces)
        expressionParser
        (fun (returnType, condition) _ firstResult _ secondResult ->
            Ast.TernaryOperator(returnType, condition, firstResult, secondResult))
    <??> "Ternary Operator"


let typeAssertion =
    pipe3 (identifierNameParser .>> spaces1) (wordAlone "typeassert" >.< spaces1) typeParser (fun vName _ t ->
        Ast.TypeAssertion(vName, t))

let lambda =
    pipe4
        (pstr "lambda" >>. spaces1 >>. typeParser .>> spaces)
        (pchar '(' >>. (sepBy functParam (pchar ',')))
        (pchar ')' >>. spaces)
        constructsBetweenBraces
        (fun returnType parameters _ contents -> parameters, returnType, (contents, newRandomNumber ()))
    <??> "Lambda"

let (mapLiteral: Parser<Ast.MapLiteralData>) =
    let keyValueEntry =
        pipe3 (expressionParser .>> spaces) (pchar ':' .>> spaces) expressionParser (fun k _ v -> k, v)

    pipe4
        ((pstr "map[" >>. typeParser .>> pchar ']') .>>. typeParser)
        (pchar '{' >.< sep)
        (sepEndBy keyValueEntry ((pchar ',' >.< sep) <|> (newline >>. preturn ())))
        (sep >.< pchar '}')
        (fun (keyType, valueType) _ entries _ -> keyType, valueType, entries)


// I know this isn't the technical name of it, but I like it
let wrapperTypeLiteral =
    pipe2 (pchar '(' .>> spaces) (expressionParser .>> spaces .>> pchar ')') (fun _ expr ->
        fun typeIdentifier -> Ast.WrapperTypeLiteral(typeIdentifier, expr))

let typeConversion =
    pipe2
        (betweenParentheses typeParser)
        (betweenParentheses expressionParser)
        (fun t expr -> Ast.TypeConversion(t, expr))
        <??> "Type conversion"

let nullParser = pstr "null" >>. preturn Ast.Null

let tuple =
    pipe2
        (pchar '(' >>. spaces >>. expressionParser .>> spaces .>> pchar ',' .>> spaces)
        (sepBy expressionParser (pchar ',' >.< spaces) .>> pchar ')')
        (fun firstExpr restExpr -> Ast.Tuple(List.append [firstExpr] restExpr)) <??> "Tuple"

let dereferenceOf = pchar '&' >>. identifierNameParser |>> Ast.DereferenceOf

let (pointerTo: Parser<Ast.PointerToData>) = pchar '*' >>. identifierNameParser

let channelRecieve =
    pstr "<-" >>. spaces >>. expressionParser |>> Ast.ChannelRecieve

let exprFunctionCall (* (atLeastOneArg:bool) *) : Parser<Ast.FunctionExprCallData> =
    pipe3
        ((camelCase |>> Ast.PossibleCallableIdentifier |>> Ast.PossibleFunctionReference.Identifier)
         <|> (betweenParentheses expressionParser |>> Ast.PossibleFunctionReference.Parentheses)) // Must resolve to a function type
        (maybeFn genericsInstantiation Some None)
        (exprArgumentsCall (* atLeastOneArg *))
        (fun functionResolvable genericsInstantiation' arguments -> functionResolvable, genericsInstantiation', arguments)
    <??> "Function call"

let pipeFunctionCall (* (atLeastOneArg:bool) *) : Parser<Ast.FunctionPipeCallData> =
    pipe3
        ((attempt camelCase |>> Ast.PossibleCallableIdentifier |>> Ast.PossibleFunctionReference.Identifier)
         <|> (betweenParentheses expressionParser |>> Ast.PossibleFunctionReference.Parentheses)) // Must resolve to a function type
        (maybeFn genericsInstantiation Some None)
        (pipeArgumentsCall (* atLeastOneArg *))
        (fun functionResolvable genericsInstantiation' arguments -> functionResolvable, genericsInstantiation', arguments)
    <??> "Function call"

let whenCase =
    let caseParser =
        let regularCase =
            pipe3
                (pstr "is" >.< spaces1)
                (expressionParser .>> spaces)
                (pstr "->" >>. spaces >>. expressionParser) (fun _ expr1 expr2 ->
                Ast.WhenCase(expr1, expr2))

        let defaultCase =
            pipe2 (pstr "default" >.< spaces1 >.< pstr "->" >.< spaces) expressionParser (fun _ expr ->
                expr)

        sepByNewLines regularCase .>>. defaultCase

    (* pipe3
            (spaces >.< ((pstr "is" .>> spaces1) <|> pstr "default"))
            (maybeFn expressionParser Some None .>> spaces .>> pchar ':' .>> spaces)
            constructsBetweenBraces
            (fun _ conditionOpt content ->
                match conditionOpt with
                | Some condition -> Ast.NormalCase(condition, (content, newRandomNumber ()))
                | None -> Ast.DefaultCase(content, newRandomNumber ())) *)

    pipe5
        //(pstr "switch" >>. ((pchar '(' >>. typeNameParser .>> pchar ')')) .>> spaces1)
        (pstr "when" .>> spaces1)
        ((expressionParser .>> spaces1)
         .>>. (pchar '@' >>. spaces >>. typeParser .>> spaces))
        (pchar '{' >.< sep)
        caseParser
        (sep >.< pchar '}')
        (fun _ (expr, unresolvedType) _ (regularCases, defaultCase') _ -> Ast.Expression.When(expr, unresolvedType, regularCases, defaultCase'))
    <??> "When-case"
let charParser = 
    pchar ''' >>. anyChar .>> pchar ''' |>> Ast.Char 

let (arrayLiteral: Parser<Ast.ArrayLiteralData>) =
    let lengthParser =
        (attempt pint64 |>> ArrayLiteralLength.Number)
        <|> (pstr "..." |>> fun _ -> ArrayLiteralLength.Ellipsis)

    pipe4
        (pchar '[' >>. spaces >>. lengthParser .>> spaces .>> pchar ']')
        typeParser
        (pchar '{' >>. spaces >>. sepBy expressionParser (pchar ',' >.< sep)
         .>> spaces)
        (pchar '}')
        (fun length t items _ -> length, t, items)
    <??> "Array literal"

let (sliceLiteral: Parser<Ast.SliceLiteralData>) =
    pipe3
        (pchar '[' >>. spaces >>. pchar ']' >>. spaces >>. typeParser .>> spaces)
        (pchar '{' >>. sepBy expressionParser (pchar ',' >.< sep))
        (pchar '}')
        (fun typeName items _ -> typeName, items)
    <??> "Array literal"


let negatedExpression =
    pchar '!' >>. spaces >>. expressionParser |>> Ast.NegatedExpression

let unit = pstr "unit" >>. preturn Ast.Unit


let slice =
    let sliceableReference =
        choice
            [ attempt sliceLiteral |>> Ast.SliceableReference.SliceLiteral
              attempt str |>> Ast.SliceableReference.String
              arrayLiteral |>> Ast.SliceableReference.ArrayLiteral
              attempt identifier |>> Ast.SliceableReference.Identifier]

    pipe4
        sliceableReference
        (pchar '[' >.< spaces)
        (expressionParser .>>. (spaces >>. pchar ':' >>. spaces >>. expressionParser .>> spaces))
        (pchar ']')
        (fun reference _ (firstIndexExpr, secondIndexExpr) _ ->
            Ast.Slice(reference, firstIndexExpr, secondIndexExpr))

let constantIdentifier = screamingSnakeCase |>> Ast.Expression.ConstantIdentifier 

let expressionParentheses = 
    betweenParentheses expressionParser |>> Ast.Expression.Parentheses


expressionParsers <-
    [ (* (attempt (typeNameWithGenericsInstantiationData compositeLiteral)
       |>> Ast.CompositeLiteral),
      "compositeLiteral" *)
      mapLiteral |>> Ast.Expression.MapLiteral, "mapLiteral"
      charParser, "charParser"
      attempt ternaryOperator, "ternaryOperator"
      attempt makeFunction, "makeFunction"
      attempt newFunction, "newFunction"
      attempt typeAssertion, "typeAssertion"
      attempt typeConversion, "typeConversion"
      attempt lambda |>> Ast.Expression.Lambda, "lambda"
      //attempt index, "index"
      attempt slice, "slice"
      attempt sliceLiteral |>> Ast.Expression.SliceLiteral, "sliceLiteral"
      attempt (typeNameWithGenericsInstantiationData wrapperTypeLiteral), "newtypeLiteral"
      attempt unit, "unit"
      negatedExpression, "negatedExpression"
      attempt booleanParser |>> Ast.Expression.Boolean, "boolean"
      nullParser, "null"
      (attempt (exprFunctionCall (* false *)) |>> Ast.Expression.FunctionCall), "functionCall"
      attempt tuple, "tuple"
      expressionParentheses, "parentheses"
      str |>> Ast.Expression.String, "str"
      attempt negativeInteger |>> Ast.Expression.Number, "negativeInteger"
      attempt positiveInteger |>> Ast.Expression.Number, "positiveInteger"
      float |>> Ast.Expression.Float, "float"
      dereferenceOf, "dereferenceOf"
      pointerTo |>> Ast.Expression.PointerTo, "pointerTo"
      channelRecieve, "channelReceive"
      whenCase, "whenCase"
      (attempt (typeNameWithGenericsInstantiationData structLiteral)
       |>> Ast.Expression.StructLiteral),
      "structLiteral"
      constantIdentifier, "constantIdentifier"
      (attempt identifier |>> Ast.Expression.Identifier), "identifier"
      attempt arrayLiteral |>> Ast.Expression.ArrayLiteral, "arrayLiteral" ]

(* let (mapLookup: Parser<Ast.MapLookupData>) =
    let applicableExpressions = removeParserAndMakeParser expressionParsers "mapLookup"

    pipe3
        (applicableExpressions .>> spaces)
        (pchar '[' >>. spaces >>. expressionParser .>> spaces)
        (pchar ']')
        (fun a b _ -> a, b)
    <??> "Map Lookup"

 *)

let index =
    let applicableExpressions = removeParserAndMakeParser expressionParsers "index"
    
    
    pipe3
        (applicableExpressions)
        (pchar '[' >>. spaces >>. expressionParser .>> spaces)
        (pchar ']')
        (fun a b _ -> Ast.Index(a, b))

expressionParsers <- List.append [ attempt index, "index" ] expressionParsers

let unaryOperator =

    let applicableExpressions =
        removeParserAndMakeParser expressionParsers "unaryOperator"

    pipe3
        (applicableExpressions .>> spaces)
        (choice assignmentOperators .>> spaces)
        (maybeFn expressionParser Some None)
        (fun expr1 op expr2 -> Ast.Expression.UnaryOperator(expr1, op, expr2))

    <??> "Unary Operator Expression"



let exprAccessors =
    let applicableExpressions = removeParserAndMakeParser expressionParsers "accessors"

    let field =
        pipe3
            pascalCase
            (maybeFn genericsInstantiation Some None)
            ( maybeFn exprArgumentsCall (* atLeastOneArg *) Some None)
            (fun fieldName genericsInstantiation' args' -> Ast.FieldAccessor(fieldName, genericsInstantiation', args'))

    let funct =
        pipe3
            (camelCase <|> attempt screamingSnakeCase)
            (maybeFn genericsInstantiation Some None)
            (maybeFn exprArgumentsCall (* atLeastOneArg *) Some None)
            (fun functName genericsInstantiation' arguments' -> Ast.MethodAccessor(functName, genericsInstantiation', arguments'))

    let accessorParser =
        (attempt field <|> funct)

    pipe3 applicableExpressions (pchar '.') (sepBy1 accessorParser (pchar '.')) (fun firstExpr _ fields ->
        firstExpr, fields)
    <??> "Attribute accessors"



let pipeAccessors =
    let applicableExpressions = removeParserAndMakeParser expressionParsers "accessors"

    let field =
        pipe2
            pascalCase
            (maybeFn genericsInstantiation Some None .>>. pipeArgumentsCall (* atLeastOneArg *))
            (fun fieldName (genericsInstantiation', args) -> Ast.AccessorItemPipeArgs.FieldAccessor(fieldName, genericsInstantiation', args))

    let funct =
        pipe2
            (camelCase <|> attempt screamingSnakeCase)
            (maybeFn genericsInstantiation Some None .>>. pipeArgumentsCall (* atLeastOneArg *))
            // ^^^^^ Remove this. It should not be optional. And then deal with the type consequences ;) 
            (fun functName (genericsInstantiation', args) -> Ast.AccessorItemPipeArgs.MethodAccessor(functName, genericsInstantiation', args))

    let accessorParser =
        (attempt field <|> funct)

    pipe3 applicableExpressions (pchar '.') (sepBy1 accessorParser (pchar '.')) (fun firstExpr _ fields ->
        firstExpr, fields)
    <??> "Attribute accessors"



expressionParsers <-
    List.append [ attempt (exprAccessors (* false *)) |>> Ast.Accessors, "accessors" ] expressionParsers

let pipeOperator =

    let applicableExpressions =
        removeParserAndMakeParser expressionParsers "pipeOperator"

    (* pipe3
            (maybeFn (camelCase .>> spaces .>> pchar '.') Some None)
            (camelCase .>> spaces) // Must resolve to a function type
            (pchar '(' >>. sepBy (expressionParser .>> spaces) (pchar ',') .>> spaces .>> pchar ')')
            (fun moduleName functionName arguments -> FunctionCall(functionResolvable, arguments)) *)


    pipe2
        (applicableExpressions .>> sep)
        //(pstr "=>" >>. spaces >>. shortHandFunctionParser)
        (many1 (
            attempt (
                pstr "=>"
                >>. spaces
                >>. ((attempt (pipeAccessors (* true *)) |>> Ast.PipeCall.AccessorCall)
                     <|> ((pipeFunctionCall (* true *) |>> Ast.PipeCall.FunctionCall))
                )
                .>> sep
            )
        ))
        (* (sepBy ((spaces >>. maybePath shortHandFunctionParser)) (sep >>.pstr "=>")) *)
        (fun firstExpr (* firstFunction *) rest -> Ast.PipeOperator(firstExpr, rest))

    <??> "Pipe operator"

// Must be added after the definition to avoid infinite recursion


let operatorSequence =
    let applicableExpressions =
        removeParserAndMakeParser expressionParsers "operatorSequence"

    pipe2
        (applicableExpressions .>> spaces)
        //(pstr "=>" >>. spaces >>. shortHandFunctionParser)
        (many1 (attempt ((spaces >>. (choice operators) .>>. (spaces >>. expressionParser)))))
        (* (sepBy ((spaces >>. maybePath shortHandFunctionParser)) (sep >>.pstr "=>")) *)
        (fun firstExpr data -> Ast.OperatorSequence(firstExpr, data))
    (* (applicableExpressions .>> spaces)
    .>>. (many1 ((choice operators .>> spaces) .>>. applicableExpressions .>> spaces))
    |>> OperatorSequence *)
    <??> "Operator Sequence"


expressionParsers <- List.append [ attempt unaryOperator, "unaryOperator" ] expressionParsers

expressionParsers <- List.append [ attempt pipeOperator, "pipeOperator" ] expressionParsers

expressionParsers <- List.append [ attempt operatorSequence, "operatorSequence" ] expressionParsers

// expressionParsers |> List.iter (fun (x, y) -> printfn $"{y}")

let initializeExpressionParser () =
    expressionParserRef.Value <- choice <| List.map fst expressionParsers
