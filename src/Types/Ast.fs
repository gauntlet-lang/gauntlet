(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

[<RequireQualifiedAccessAttribute>]
module Types.Ast

open Types.Base

open FParsec


[<RequireQualifiedAccess>]
type PipeFunctionCallArgument =
    | Normal of Expression
    | Placeholder


and PipeCall =
    | FunctionCall of FunctionPipeCallData
    | AccessorCall of PipeArgsAccessorsData


and PipeOperatorData = Expression * PipeCall list

and PossibleCallableIdentifier = PossibleCallableIdentifier of string

and PossibleFunctionReference =
    | Identifier of PossibleCallableIdentifier
    | Parentheses of Expression

and IndexableReference =
    | Identifier of IdentifierName
    | Parentheses of Expression
    | ArrayLiteral of ArrayLiteralData
    | SliceLiteral of SliceLiteralData
    | String of StringData
    | MapLiteral of MapLiteralData
    | PointerTo of PointerToData

and SliceableReference =
    | SliceLiteral of SliceLiteralData
    | String of StringData
    | ArrayLiteral of ArrayLiteralData
    | Identifier of IdentifierName

and Expression =
    | Number of NumberData
    | Float of FloatData
    | String of StringData
    | FunctionCall of FunctionExprCallData
    | Identifier of string
    | Index of IndexData
    | Slice of SliceData
    | Boolean of BooleanData
    | Null
    | OperatorSequence of OperatorSequenceData
    | Parentheses of ParenthesesData
    | TernaryOperator of TernaryOperatorData
    | StructLiteral of StructLiteralData
    | ArrayLiteral of ArrayLiteralData
    | SliceLiteral of SliceLiteralData
    | MapLiteral of MapLiteralData
    | DereferenceOf of DereferenceOfData
    | PointerTo of PointerToData
    | ChannelRecieve of ChannelRecieveData
    | When of WhenData
//    | CompositeLiteral of CompositeLiteralData
    | Lambda of LambdaData
    | PipeOperator of PipeOperatorData
    | ConstantIdentifier of ConstantIdentifierData
    | Tuple of TupleData
    | TypeAssertion of TypeAssertionData
    | MakeFunction of MakeFunctionData
    | NewFunction of NewFunctionData
    | WrapperTypeLiteral of WrapperTypeLiteralData
    | Accessors of ExprArgsAccessorsData
    | NegatedExpression of NegatedExpressionData
    | TypeConversion of TypeConversionData
    | UnaryOperator of UnaryOperatorData
    | EnumCaseLiteral of ExprEnumCaseLiteralData
    | PatternMatch of PatternMatchData
    | Char of CharData
    | Unit

and NonToplevelDeclaration =
    | NormalVar of UnresolvedType option * VariablePattern * Expression
    | ZeroVar of VariablePattern * UnresolvedType

and MatchingPattern = 
    | Number of NumberData 
    | Float of FloatData 
    | String of StringData 
    | Destructure of IdentifierName
    | Boolean of BooleanData
    | Char of char
    //| ArrayLiteral of ArrayLiteralData
    //| SliceLiteral of SliceLiteralData
    | EnumCaseLiteral of PatternEnumCaseLiteralData

and PatternEnumCaseLiteralData = string * (string * MatchingPattern) list

and ExpressionCallArguments = (Expression * HasEllipsis) list
and PatternMatchData = Expression * (MatchingPattern * RawScopeData) list
and PipeCallArguments = (PipeFunctionCallArgument * HasEllipsis) list
and FunctionExprCallData = PossibleFunctionReference * GenericsInstantiationData option * ExpressionCallArguments
and FunctionPipeCallData = PossibleFunctionReference * GenericsInstantiationData option * PipeCallArguments
and OperatorSequenceData = Expression * (Operator * Expression) list
and ParenthesesData = Expression
and CharData = char
and TernaryOperatorData = UnresolvedType * Expression * Expression * Expression
and StructLiteralData = UnresolvedStructReference * StructLiteralField list
and ArrayLiteralData = ArrayLiteralLength * UnresolvedType * Expression list
and PlaceholderParameterLocation = int
and SliceLiteralData = SliceTypeData * Expression list
and DereferenceOfData = IdentifierName
and PointerToData = IdentifierName
and ConstantIdentifierData = string

and ExpressionSwitchCaseData = Expression * RegularCase list * DefaultCase option
and TypeSwitchCaseData = Expression * TypeSwitchCase list * DefaultCase option
and SwitchCaseType = 
    | ExpressionSwitch of ExpressionSwitchCaseData
    | TypeSwitch of TypeSwitchCaseData

and LambdaData = Parameter list * ReturnType * RawScopeData
and TupleDChannelRecieveDataata = Expression list
and TypeAssertionData = IdentifierName * UnresolvedType
and MakeFunctionData = UnresolvedType * Expression list
and ExprEnumCaseLiteralData = string * (string * Expression) list
and NewFunctionData = UnresolvedType
and WrapperTypeLiteralData = TypeNameWithGenericsInstantiationData * Expression
and ExprArgsAccessorsData = Expression * AccessorItemExprArgs list
and PipeArgsAccessorsData = Expression * AccessorItemPipeArgs list

and TupleData = Expression list


and AccessorItemExprArgs =
    | FieldAccessor of FieldName * GenericsInstantiationData option * ExpressionCallArguments option
    | MethodAccessor of MethodName * GenericsInstantiationData option * ExpressionCallArguments option 

// At least one parameter is required, thus call data is not optional
and AccessorItemPipeArgs =
    | FieldAccessor of FieldName * GenericsInstantiationData option * PipeCallArguments
    | MethodAccessor of MethodName * GenericsInstantiationData option * PipeCallArguments

and NumberData = IsNegative * string * NumberProperty option
and FloatData = IsNegative * float * IsImaginary
and StringData = string
and BooleanData = bool


and NegatedExpressionData = Expression
and IndexNumber = int
and UnaryOperatorData = Expression * Operator * Expression option
and ChannelRecieveData = Expression
and TypeConversionData = UnresolvedType * Expression
and IndexData = Expression * Expression
and ArrayTypeData = ArrayLiteralLength * UnresolvedType
and SliceData = SliceableReference * Expression * Expression
and InterfaceFieldData = ExportedStatus * (UnresolvedType * FieldName)
and InterfaceMethodData = ExportedStatus * MethodName * Parameter list * ReturnType
and TypeSetData = (bool * UnresolvedType) list
and StructLiteralField = string * Expression
and MapLookupData = Expression * Expression
and StructFieldData = UnresolvedType * FieldName

and ScopeConstruct =
    | ScopedExpression of Expression
    | ScopedDeclaration of NonToplevelDeclaration
    | ScopedSatement of Statement

and ScopeContent = ScopeConstruct list
and RawScopeData = ScopeContent * ScopeId
and DefaultCase = DefaultCase of RawScopeData

and RegularSelectCase = VariablePattern option * Expression * RawScopeData
and SelectCaseData = RegularSelectCase list * DefaultCase option

and RegularCase = Expression * RawScopeData
and TypeSwitchCase = UnresolvedType * RawScopeData

and WhenCase = Expression * Expression
and DefaultWhenExpression = Expression

and WhenData = Expression * ReturnType * WhenCase list * DefaultWhenExpression

and ForLoopData = ForLoopStyle * RawScopeData
and WhileLoopData = Expression * RawScopeData
and TryData = (IdentifierName * IdentifierName) option * Expression * Expression
and ForceData = (IdentifierName * IdentifierName) * Expression * Expression
and IfData = (Expression * RawScopeData) * (((Expression * RawScopeData) list) option) * ((RawScopeData) option)
and StatementData = Expression option

and SelectData = SelectCaseData


and Statement =
    | Return of StatementData
    | If of IfData
    | SwitchCase of SwitchCaseType
    | ForLoop of ForLoopData
    | WhileLoop of WhileLoopData
    | Select of SelectCaseData
    | Break
    | Continue
    | Go of RawScopeData
    | Defer of RawScopeData
    | Try of TryData
    | Force of ForceData

and ForLoopStyle =
    | Traditional of (VariablePattern * Expression) * Expression * Expression
    | ShortHand of VariablePattern * Expression


and StructConstruct =
    | Field of ExportedStatus * StructFieldData * StructTag option
    | Embedded of TypeNameWithGenericsInstantiationData


and InterfaceConstruct =
    | Embedded of TypeNameWithGenericsInstantiationData
    | Method of InterfaceMethodData
    | TypeSet of TypeSetData

and MapLiteralData = UnresolvedType * UnresolvedType * MapKeyValueEntry list

and MapKeyValueEntry = Expression * Expression

type IsInPipeSequence = bool
type Name = string
type Path = Expression * FieldName list
type ErrorMessage = string
type InnerValue = Expression
type TypeLength = int64


type ConstExpression =
    | Number of NumberData
    | String of StringData
    | Boolean of BooleanData
    | Float of FloatData
    | Identifier of IdentifierName

type TypeDeclarationData = ExportedStatus * TypeName * Generics option
type StructData = ExportedStatus * StructName * Generics * StructConstruct list
type InterfaceData = ExportedStatus * InterfaceName * Generics * InterfaceConstruct list
type MethodParameters = UnresolvedMethodReceiverParameter * Parameter list
type MethodData = ExportedStatus * MethodName * MethodParameters * ReturnType * RawScopeData
type FunctionData = ExportedStatus * FunctionName * Generics * Parameter list * ReturnType * RawScopeData
type AliasData = ExportedStatus * AliasName * Generics * UnresolvedType
type WrapperTypeData = ExportedStatus * WrapperTypeName * Generics * UnresolvedType
type ConstData = ExportedStatus * IdentifierName * ConstExpression

type ASTFieldsAndFunctionsData =
    { Structs: list<StructData>
      StructFields: list<StructName * StructFieldData>
      InterfaceMethods: list<InterfaceName * MethodData>
      Functions: FunctionData list
      Methods: list<UnresolvedMethodReceiverParameter * MethodData> }



type ConstName = string

type EnumCase = string * (string * UnresolvedType) list * int
type EnumData = string * EnumCase list 

type ToplevelDeclaration =
    | Function of FunctionData
    | Import of ModuleName * IdentifierName
    | Struct of StructData
    | Interface of InterfaceData
    | Alias of AliasData
    | WrapperType of WrapperTypeData
    | Method of MethodData
    | Const of ConstData
    | Enum of EnumData

type AST = ToplevelDeclaration list

type IdentifierData =
    { VarName: string
      Type : UnresolvedType
      ExportStatus: ExportStatus }

type ASTData =
    { Functions: Map<string, FunctionData>
      Structs: Map<string, StructData> 
      Interfaces: Map<string, InterfaceData>
      Enums : Map<string, EnumData>
      WrapperTypes: Map<string, WrapperTypeData>
      Consts: Map<string, ConstData>
      RawAST: AST
      ImportedModules: Map<IdentifierName, (ModuleName * IdentifierName)>
      Aliases: Map<string, AliasData>
      Methods: Map<string, MethodData> }

type GauntletFileData =
    { ModuleName: ModuleName
      ASTData: ASTData }

