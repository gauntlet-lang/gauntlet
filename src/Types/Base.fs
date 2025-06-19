(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

module Types.Base

open FParsec

type ExportedStatus =
    | Exported
    | Unexported

type ExportStatus =
    | Exportable of ExportedStatus
    | NonExportable

and NumberProperty = 
    | Imaginary
    | Unsigned

type UserState = unit
type Parser<'t> = Parser<'t, UserState>
type IsDereference = bool
type IsPtr = bool
type FieldName = string
type Prefix = char
type HasEllipsis = bool
type IdentifierName = string
type FunctionName = string
type ModuleName = string
// Only for type names comprised of letters
type TypeName = string
type PackageDeclaration = string
type Operator = string
type StructName = string
type VariablePattern = IdentifierName list
type EnumFieldName = string
type EnumValue = ExportedStatus * EnumFieldName
type InterfaceName = string
type AliasName = string
type WrapperTypeName = string
type GenericTypeName = string
type PackageName = string
type TranspiledConstruct = string
type ReturnTypeName = string
type ScopeId = int
type StructTag = string
type InterfaceFieldName = string
type EndsWithU = bool
type IsNegative = bool
type IsImaginary = bool

type MethodName = string
type AttributeName = string

type ChannelType =
    | SendOnly
    | ReceiveOnly
    | BiDirectional

type ArrayLiteralLength =
    | Number of int64
    | Ellipsis


type UnresolvedType = // Change name to just "Type" when refractoring is finished
    // For built-in types, and custom ones
    //| NonBuiltInType of ModuleName option * TypeName * GenericsInstantiationData
    | SliceType of SliceTypeData
    | ImportedType of ModuleName * TypeName
    | ArrayType of ArrayTypeData
    | MapType of UnresolvedType * UnresolvedType
    | ChanType of ChannelType * UnresolvedType
    | TupleType of UnresolvedType list
    | PointerType of UnresolvedType
    | FunctionType of FunctionTypeData
    | EnumType
    | BooleanType
    | ErrorType
    | StringType
    | AnyType
    | UnitType
    | NullType
    | IntegerType of IntType
    | PendingResolutionType of TypeNameWithGenericsInstantiationData
    // ONLY for use with types that come from a module
    | UnknownType
    // ONLY for use with types that have been imported
    | QualifiedType of QualifiedTypeData



and TypeNameWithGenericsInstantiationData = TypeName * GenericsInstantiationData
and UnresolvedStructReference = UnresolvedStructReference of TypeNameWithGenericsInstantiationData
and UnresolvedInterfaceReference = UnresolvedInterfaceReference of TypeNameWithGenericsInstantiationData

and UnresolvedMethodReceiverParameter = TypeNameWithGenericsInstantiationData * string

and GenericsInstantiationData = UnresolvedType list
and QualifiedTypeData = ModuleName * TypeName
and ReturnType = UnresolvedType
and MethodTypeData = ParameterFunctionTypeData list * ReturnType
and ArrayTypeData = ArrayLiteralLength * UnresolvedType
and SliceTypeData = UnresolvedType
and ParameterFunctionTypeData = HasEllipsis * UnresolvedType
and FunctionTypeData = ParameterFunctionTypeData list * ReturnType
and IntType = 
    | Uint8Type
    | Uint16Type
    | Uint32Type
    | Uint64Type
    | IntType
    | Int8Type
    | Int16Type
    | Int32Type
    | Int64Type
    | Float32Type
    | Float64Type
    | Complex64Type
    | Complex128Type
    | ByteType

type GenericData = UnresolvedType * GenericTypeName
type Generics = GenericData list
type Parameter = HasEllipsis * UnresolvedType * IdentifierName

type AdditionalOption = 
    Run


