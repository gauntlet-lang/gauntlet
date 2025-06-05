(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Parsers.TypeParsers

open FParsec
open BaseParsers
open Types
open Types.Base

let (typeLengthParser: Parser<int64>) =
    pchar '[' >>. spaces >>. pint64 .>> spaces .>> pchar ']'

let (arrayType: Parser<ArrayTypeData>) =
    (typeLengthParser |>> ArrayLiteralLength.Number) .>>. typeParser

let mapType =
    (pstr "map" >>. pchar '[' >>. typeParser) .>>. (pchar ']' >>. typeParser)
    |>> MapType

let functionType =
    (pstr "fn"
     >>. spaces
     >>. pchar '('
     >>. spaces
     >>. (sepBy1 ((maybe ellipsisParser true false .>>. typeParser) .>> spaces) (pstr "," .>> spaces)))
    .>>. (spaces >>. spaces >>. pstr "->" >>. spaces >>. typeParser .>> pchar ')')
    |>> FunctionType

let qualifiedType =
    pipe3
        flatCase
        (pchar '.')
        pascalCase
        (fun moduleName _ typeName -> QualifiedType(moduleName, typeName)) <??> "Qualified Type"

let (pstrType: Parser<Base.UnresolvedType>) =
    choice
        [ wordAlone "String" >>. preturn StringType
          wordAlone "Bool" >>. preturn BooleanType
          wordAlone "Error" >>. preturn ErrorType
          wordAlone "Null" >>. preturn NullType
          wordAlone "Uint8" >>. preturn (IntegerType Uint8Type)
          wordAlone "Uint16" >>. preturn (IntegerType Uint16Type)
          wordAlone "Uint32" >>. preturn (IntegerType Uint32Type)
          wordAlone "Uint64" >>. preturn (IntegerType Uint64Type)
          wordAlone "Int8" >>. preturn (IntegerType Int8Type)
          wordAlone "Int16" >>. preturn (IntegerType Int16Type)
          wordAlone "Int32" >>. preturn (IntegerType Int32Type)
          wordAlone "Int64" >>. preturn (IntegerType Int64Type)
          wordAlone "Int" >>. preturn (IntegerType IntType)
          wordAlone "Float32" >>. preturn (IntegerType Float32Type)
          wordAlone "Float64" >>. preturn (IntegerType Float64Type)
          wordAlone "Complex64" >>. preturn (IntegerType Complex64Type)
          wordAlone "Complex128" >>. preturn (IntegerType Complex128Type)
          wordAlone "Byte" >>. preturn (IntegerType ByteType)
          wordAlone "Rune" >>. preturn (IntegerType Int32Type) // It's an alias for int32 
          wordAlone "Unit" >>. preturn UnitType
          wordAlone "Any" >>. preturn AnyType
          (many1 (pstr "[]")) >>. typeParser |>> SliceType
          (((attempt (pstr "chan<-" >>. preturn SendOnly)
             <|> (pstr "<-chan" >>. preturn ReceiveOnly)
             <|> (pstr "chan" >>. preturn BiDirectional))
            .>> spaces1)
           .>>. typeParser
           |>> ChanType)
          (pchar '(' >>. spaces >>. (sepBy typeParser (pchar ',' >.< spaces))
           .>> spaces
           .>> pchar ')'
           |>> TupleType)
          pchar '*' >>. typeParser |>> PointerType
          (typeNameWithGenericsInstantiationData (preturn id) |>> PendingResolutionType)
          (arrayType |>> ArrayType)
          mapType ]


let initializeTypeParser () =
    typeParserRef.Value <-
        attempt pstrType
        <|> attempt functionType
        <|> qualifiedType
        <??> "CamelCase type name"
