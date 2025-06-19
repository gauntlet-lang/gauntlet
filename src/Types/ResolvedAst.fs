(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


[<RequireQualifiedAccess>]
module Types.ResolvedAst

open FsToolkit.ErrorHandling

// Resolved AST means that:
// - No variable issues (e.g. variables exist and they don't not come one after the other)
// - All structures must resolve to the declaration it refers to (1 layer only), along with some metadata
// - Also, for the variables in the scope, make sure to keep their types and their mapped declaration (1 layer only) handy 
// - Being "resolved" does NOT mean that everything has been type-checked.


open Types.Base


/// Represents - internally - a type that has been verified to exist.
[<RequireQualifiedAccess>]
type ResolvedType =
    | SliceType of SliceTypeData
    | ImportedType of {|ModuleName: string; TypeName: string|}
    | ArrayType of ArrayTypeData
    | MapType of MapTypeData
    | ChanType of ChannelType * ResolvedType
    | TupleType of ResolvedType list
    | PointerType of ResolvedType
    | FunctionType of FunctionTypeData
    | MethodType of MethodTypeData
    | StructType of StructTypeData
    | InterfaceType of InterfaceTypeData
    | WrapperTypeType of WrapperTypeTypeData
    | AliasType of AliasTypeData
    /// ONLY for use when getting a type from a module
    | UnknownType
    | BooleanType
    | ErrorType
    | StringType
    | UnitType
    | AnyType
    | NullType
    | EnumType
    | IntegerType of IntType
    | ModuleType of ModuleName
    | GenericType of GenericTypeData
    | QualifiedType of QualifiedTypeData

and AliasTypeData = {|AliasName : string; ExportedStatus : ExportedStatus; GenericsInstantiation: GenericsInstantiationData|}
and GenericTypeData = {Name:string; ConstraintType:ResolvedType}
and StructTypeData = { StructName : string; ExportedStatus: ExportedStatus; GenericsInstantiation: GenericsInstantiationData }
and InterfaceTypeData = { InterfaceName : string; ExportedStatus: ExportedStatus; GenericsInstantiation: GenericsInstantiationData }
and WrapperTypeTypeData = {WrapperTypeName : string; ExportedStatus: ExportedStatus; GenericsInstantiation: GenericsInstantiationData }
and QualifiedTypeData = {ModuleName : string; TypeName: string}

and GenericsInstantiationData = ResolvedType list

and MapTypeData = {KeyType: ResolvedType; ValueType: ResolvedType}

and GenericData = {
    Type : ResolvedType
    TypeName : string
}

and Generics = GenericData list


and SliceTypeData = { Type: ResolvedType }

and ArrayTypeData =
    { Length: ArrayLiteralLength
      Type: ResolvedType }


and FunctionTypeParameter =
    {
        HasEllipsis : bool
        Type : ResolvedType
    }

and FunctionTypeData =
    { Parameters: FunctionTypeParameter list
      ReturnType: ResolvedType }

and MethodTypeData =
    { Name: string
      ReceiverParameterName: string
      ReceiverType: MethodReceiverType
      ParameterTypes: Parameter list
      ReturnType: ResolvedType }

and MethodReceiverType =
    | StructReceiver of StructDeclarationData * GenericsInstantiationData
    | WrapperTypeReceiver of WrapperTypeDeclarationData * GenericsInstantiationData

/// An expression is said to be "resolved" if all of it's components
/// have been mapped back to it's declaration by 1 layer.
/// While there are a few exceptions, type-checking has largely NOT been done
/// on these.
and Expression =
    | Number of NumberData 
    | Float of FloatData 
    | String of StringData 
    | FunctionCall of FunctionExpressionCallData
    | Variable of ScopedVariable
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
    | When of WhenData
    | ChannelRecieve of ChannelRecieveData
    | Lambda of LambdaData
    | PipeOperator of PipeOperatorData
    | Tuple of TupleData
    | TypeAssertion of TypeAssertionData
    | MakeFunction of MakeFunctionData
    | NewFunction of NewFunctionData
    | ConstantIdentifier of ConstantIdentifierData
    | WrapperTypeLiteral of WrapperTypeLiteralData
    | Accessors of ExprArgsAccessorsData
    | NegatedExpression of NegatedExpressionData
    | TypeConversion of TypeConversionData 
    | UnaryOperator of UnaryOperatorData
    | PatternMatch of PatternMatchData
    | ModuleAccessor of ModuleAccessorDataExprArgs
    | ModuleReference of ModuleReferenceData
    | Char of char
    | EnumCaseLiteral of ExprEnumCaseLiteralData
    | Unit

and PatternMatchData = {
    MatchExpr: Expression;
    Cases: {|Pattern: MatchingPattern; Scope: ScopeData|} list
    }


and MatchingPattern = 
    | Number of NumberData 
    | Float of FloatData 
    | String of StringData 
    | Boolean of BooleanData
    | Destructure of IdentifierName
    | Char of char
    //| ArrayLiteral of ArrayLiteralData
    //| SliceLiteral of SliceLiteralData
    | EnumCaseLiteral of PatternEnumCaseLiteralData


and PatternEnumCaseLiteralData = {
    Name : string
    CaseId : int
    Fields : {|Name : string; Value : MatchingPattern|} list}

and ConstantIdentifierData = {
    Identifier : string
    Reference : ConstDeclarationData
}

and ModuleReferenceData = {
    ModuleIdentifier : string
}

// `Possibly` callable because it may not refer to a function type
and PossiblyCallableExpression = 
    | Parentheses of Expression
    | Var of ScopedVariable
    | FunctionDeclaration of FunctionDeclarationDataWithoutScope
    | BuiltInFunction of BuiltInFunctionData

and FunctionExpressionCallData = {
    Reference: PossiblyCallableExpression
    GenericsInstantiation : GenericsInstantiationData option
    Arguments : ExpressionCallArguments
}

and FunctionPipeCallData = {
    Reference: PossiblyCallableExpression
    GenericsInstantiation : GenericsInstantiationData option
    Arguments : PipeCallArguments
}

and ModuleAccessorItemExprArgs = {
    AttributeName: string
    GenericsInstantiation : GenericsInstantiationData option
    Arguments: ExpressionCallArguments option
}

and ModuleAccessorItemPipeArgs = {
    AttributeName: string
    GenericsInstantiation : GenericsInstantiationData option
    Arguments: PipeCallArguments
}

and ModuleAccessorDataExprArgs = 
    {
        IdentifierName: string
        Accessors: ModuleAccessorItemExprArgs list
    }

and ModuleAccessorDataPipeArgs = 
    {
        IdentifierName: string
        Accessors: ModuleAccessorItemPipeArgs list
    }

and EnumCase = {
    Id : int
    Name : string
    Fields : {|Name : string; Type: ResolvedType|} list
}

and EnumDeclarationData = {
    Name : string
    Cases : EnumCase list
}


and InterfaceDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: string
      Generics: Generics
      EmbeddedInterfaces: InterfaceReference list 
      ActualMethods: MethodFieldData list
      TypeSets : TypeSetData list
      EmbeddedStructs: StructReference list
       } 

    member this.GetMember(seperatelyDefinedMethods:Map<string * string, MethodDeclarationDataWithoutScopeData>, accessorName:string): Option<Member> = 
        let fromActualMethods' = 
            this.ActualMethods
            |> List.tryFind(fun method -> method.Name = accessorName)
            |> Option.map (Member.InterfaceMethod)

        let fromEmbeddedInterfaces' = 
            this.EmbeddedInterfaces
            |> List.tryFind (fun s -> s.InterfaceDeclaration.Name.ToLower() = accessorName.ToLower())
            |> Option.map Member.InterfaceDeclaration
        
        let fromEmbeddedStructs' = 
            this.EmbeddedStructs
            |> List.tryFind (fun s -> s.StructDeclaration.Name.ToLower() = accessorName.ToLower())
            |> Option.map Member.StructDeclaration

        fromActualMethods'
        |> Option.orElse fromEmbeddedInterfaces'
        |> Option.orElse fromEmbeddedStructs'

/// Represents a physical reference to a struct in Gauntlet code
and StructReference = {
    StructDeclaration : StructDeclarationData
    GenericsInstantiation: GenericsInstantiationData
}

/// Represents a physical reference to an interface in Gauntlet code
and InterfaceReference = {
    InterfaceDeclaration: InterfaceDeclarationData
    GenericsInstantiation: GenericsInstantiationData
}

and TypeSetData = {
    Types : {|Type : ResolvedType; IncludeUnderlyingType: bool|} list
}

and ResolvedDeclarations = {
    Structs: Map<string, StructDeclarationData>
    Interfaces: Map<string, InterfaceDeclarationData>
    Enums: Map<string, EnumDeclarationData>
    WrapperTypes: Map<string, WrapperTypeDeclarationData>
    Functions: Map<string, FunctionDeclarationDataWithoutScope>
    Consts : Map<string, ConstDeclarationData>
    Aliases: Map<string, AliasDeclarationData>
    ImportedModules: Map<IdentifierName, ImportDeclarationData>
    BuiltInFunctions: Map<string, BuiltInFunctionData>
    // string * string means the name of the receiver type (obtained by the transpile type function)
    // and the name of the method
    Methods: Map<string * string, MethodDeclarationDataWithoutScopeData> 
}

and Context = {
    ResolvedDeclarations : ResolvedDeclarations
    ScopeVariables : ScopeVariables
    ScopeGenerics: Base.Generics
    AstData: Ast.ASTData
}

and StructDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: StructName
      Generics: Generics
      ActualFields: StructFieldDeclarationData list 
      EmbeddedStructs: StructReference list
      EmbeddedInterfaces: InterfaceReference list
       }
    member this.GetMember(seperatelyDefinedMethods:Map<string * string, MethodDeclarationDataWithoutScopeData>, accessorName:string): Option<Member>  =
        let fromActualField' = 
            this.ActualFields
            |> List.tryFind(fun field -> field.Name = accessorName)
            |> Option.map (fun field -> Member.StructField(field))

        let fromEmbeddedStruct' = 
            this.EmbeddedStructs
            |> List.tryFind (fun s -> s.StructDeclaration.Name.ToLower() = accessorName.ToLower())
            |> Option.map Member.StructDeclaration

        let fromEmbeddedInterface' = 
            this.EmbeddedInterfaces
            |> List.tryFind (fun i -> i.InterfaceDeclaration.Name.ToLower() = accessorName.ToLower())
            |> Option.map Member.InterfaceDeclaration
        
        let fromSeperatelyDefinedMethod' = 
            seperatelyDefinedMethods
            |> Map.tryFind(this.Name, accessorName)
            |> Option.map(Member.Method)

        fromActualField'
        |> Option.orElse fromEmbeddedStruct'
        |> Option.orElse fromEmbeddedInterface'
        |> Option.orElse fromSeperatelyDefinedMethod'

        


and StructFieldDeclarationData = {
    ExportedStatus: ExportedStatus
    Type: ResolvedType
    Name:string
    Tag: StructTag option
}


and Parameter = 
    {
        HasEllipsis : bool
        Type : ResolvedType
        Name : IdentifierName
    }

and MethodFieldData =
    { ExportedStatus: ExportedStatus
      Name: MethodName
      Parameters: Parameter list
      ReturnType: ResolvedType }


and WrapperTypeDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: string
      Generics: Generics
      Type: ResolvedType }

        member this.GetMember(seperatelyDefinedMethods:Map<string * string, MethodDeclarationDataWithoutScopeData>, accessorName:string) = 
            seperatelyDefinedMethods
            |> Map.tryFind(this.Name, accessorName)
            |> Option.map Member.Method


and WrapperTypeReference =
    { WrapperDeclaration: WrapperTypeDeclarationData
      GenericsInstantiation: GenericsInstantiationData
     }

and ScopeVariables = private {
   mutable Variables: Map<string, ScopedVariable>
} 
    with
        member this.TryFindVariable((* consts:Map<string, ConstDeclarationData>,  *)variableName:string) =
            this.Variables
            |> Map.tryFind variableName
            //|> Option.orElse (Map.tryFind variableName consts)
            |> function
               | Some x -> Ok x
               | None ->
//                    failwith "Bro"
                    Error [$"Variable '{variableName}' was not found in scope"]
        member this.StoreVariable(scopedVariable:ScopedVariable) =
            let variableName = 
                match scopedVariable with
                | ExprVariable({Name = name})
                | ZeroVariable({Name = name})
                | DestructuredVariable({Name = name})
                | ParameterVariable({Name = name}) -> name
            this.Variables <- this.Variables.Add(variableName, scopedVariable)
        
        member this.StoreVariables(scopedVariables:ScopedVariable list) =
            for v in scopedVariables do
                if v.GetName() <> "_" then // We do not want to store these
                    this.StoreVariable(v)

        member this.GetAllVariables() = 
            this.Variables.Values
            |> Seq.toList
        
        member this.RemoveVariable(v:ScopedVariable) =
            this.Variables <- this.Variables.Remove(v.GetName())

        member this.RemoveVariables(input:ScopedVariable list) = 
            for v in input do
                this.RemoveVariable(v)
        
        member this.Clear() = 
            this.Variables <- Map.empty

        static member CreateEmpty() = {
            Variables = Map.empty
        }

        member this.Copy() =
            let newScope = ScopeVariables.CreateEmpty()
            for v in this.Variables.Values do
                newScope.StoreVariable(v)
            newScope




and ScopedVariable = 
    | ExprVariable of ExprVariableData
    | ZeroVariable of ZeroVariableData
    | ParameterVariable of ParameterVariableData 
    | DestructuredVariable of DestructuredVariableData
    
    member this.GetName() = 
        match this with
        | ScopedVariable.ExprVariable(data) -> data.Name
        | ScopedVariable.ParameterVariable(data) -> data.Name
        | ScopedVariable.ZeroVariable(data) -> data.Name 
        | ScopedVariable.DestructuredVariable(data) -> data.Name
    

// Finally change the name 
and ExprVariableData =
    { Name: string
      Expression : Expression }

and ZeroVariableData =
    { Name: string
      Type: ResolvedType }

and ParameterVariableData =
    { Name: string
      HasEllipsis : bool
      Type: ResolvedType }

and DestructuredVariableData =
    { Name: string
      Type: ResolvedType }

and ScopeData =
    { Id: ScopeId
      Content: ScopeContent
      Variables : ScopeVariables
      VariablesDefinedInThisScope: ScopedVariable list
       }


and NumberData =
    { IsNegative: IsNegative
      StringRepresentation: string
      Property: NumberProperty option }

and FloatData =
    { IsNegative: IsNegative
      Float: float
      IsImaginary: IsImaginary }

and StringData = string

and CaseData = Expression * ScopeData
and TypeCaseData = ResolvedType * ScopeData



and FunctionDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: string
      Generics: Generics
      Parameters: Parameter list
      ReturnType: ResolvedType
      ScopeData: ScopeData }

and FunctionDeclarationDataWithoutScope =
    { ExportedStatus: ExportedStatus
      Name: string
      Generics: Generics
      Parameters: Parameter list
      ReturnType: ResolvedType }


and MapKeyValueEntry = {
    Key: Expression
    Value: Expression  
} 

and CompositeLiteralFields = { Name: string; Value: Expression }


and ExpressionCallArguments = {|Argument : Expression; HasEllipsis : HasEllipsis|} list
and PipeCallArguments = {|Argument : PipeFunctionCallArgument; HasEllipsis : HasEllipsis|} list


and PipeFunctionCallArgument =
    | Normal of Expression
    | Placeholder

and Member =
    // | WrapperType of WrapperTypeReference
    | StructDeclaration of StructReference
    | InterfaceDeclaration of InterfaceReference
    | InterfaceMethod of Declaration: MethodFieldData 
    | Method of Declaration: MethodDeclarationDataWithoutScopeData
    | StructField of Declaration: StructFieldDeclarationData

// I think the diff is AccessorItem represents the caller (hence why it has the callData) and the other 
// represents the "declaration" of the member

and AccessorItemExprArgs =
    | StructDeclaration of StructReference
    | InterfaceDeclaration of InterfaceReference
    | InterfaceMethod of Declaration: MethodFieldData * GenericsInstantiation: GenericsInstantiationData option * Arguments : ExpressionCallArguments option
    | Method of Declaration: MethodDeclarationDataWithoutScopeData * GenericsInstantiation: GenericsInstantiationData option * Arguments : ExpressionCallArguments option
    | StructField of Declaration: StructFieldDeclarationData * GenericsInstantiation: GenericsInstantiationData option * Arguments : ExpressionCallArguments option

and AccessorItemPipeArgs =
    | StructDeclaration of StructReference
    | InterfaceDeclaration of InterfaceReference
    | InterfaceMethod of Declaration: MethodFieldData * GenericsInstantiation : GenericsInstantiationData option * Arguments : PipeCallArguments 
    | Method of Declaration: MethodDeclarationDataWithoutScopeData * GenericsInstantiation : GenericsInstantiationData option * Arguments : PipeCallArguments
    | StructField of Declaration: StructFieldDeclarationData * GenericsInstantiation : GenericsInstantiationData option * Arguments : PipeCallArguments

and ExprArgsAccessorsData =
    { BaseExpression: Expression
      Accessors: AccessorItemExprArgs list }

and PipeArgsAccessorsData =
    { BaseExpression: Expression
      Accessors: AccessorItemPipeArgs list }

and PipeCall = 
    | FunctionCall of FunctionPipeCallData
    | AccessorCall of PipeArgsAccessorsData
    | ModuleAccessorCall of ModuleAccessorDataPipeArgs


and ImportDeclarationData =
    { ModuleName: string
      IdentifierName: string }


and AliasDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: string
      Generics: Generics
      Type: ResolvedType }


and MethodDeclarationData =
    { ExportedStatus: ExportedStatus
      Name: string
      ReceiverType: MethodReceiverType
      Parameters: Parameter list
      ReturnType: ResolvedType
      ScopeData: ScopeData
      ReceiverParameterName: string }

and MethodDeclarationDataWithoutScopeData =
    { ExportedStatus: ExportedStatus
      Name: string
      ReceiverType: MethodReceiverType
      Parameters: Parameter list
      ReturnType: ResolvedType
      ReceiverParameterName: string }

and BooleanData = { Value: bool }

and ConstExpression =
    | Number of NumberData
    | String of StringData
    | Boolean of BooleanData
    | Float of FloatData
    | Const of ConstDeclarationData
    //| Iota


and ConstDeclarationData =
    { ExportedStatus: ExportedStatus
      IdentifierName: string
      Expression: ConstExpression }

and IndexData =
    { ReferenceExpr: Expression 
      LookupExpr: Expression }

and SliceData =
    { Reference: Expression 
      StartIndex: Expression
      EndIndex: Expression }


and OperatorSequenceData =
    { BaseExpression: Expression
      OperatorsAndExpressions: {|Operator: Operator; Expression : Expression|} list }

and ParenthesesData = { InnerExpression: Expression }

and TernaryOperatorData =
    { ExpressionType: ResolvedType
      ConditionExpression: Expression
      SuccessExpression: Expression
      FailureExpression: Expression }

and StructLiteralField = {ExportedStatus: ExportedStatus; Name: string; Value: Expression }

and StructLiteralData =
    { Struct: StructReference
      Fields: StructLiteralField list }

and ArrayLiteralData =
    { ItemsType: ResolvedType
      Length: ArrayLiteralLength
      Items: Expression list }

and SliceLiteralData =
    { SliceItemsType: ResolvedType
      Items: Expression list }

and MapLiteralData =
    { KeyType: ResolvedType
      ValueType: ResolvedType
      Items: MapKeyValueEntry list }


and DereferenceOfData = { ReferenceIdentifier: ScopedVariable }

and PointerToData = { ReferenceIdentifier: ScopedVariable }

and ChannelRecieveData = { Expression: Expression }

and SwitchExpressionCaseData =
    { SwitchExpression: Expression
      Cases: CaseData list
      DefaultCase: DefaultCase option }

and SwitchTypeCaseData =
    { Expression: Expression
      Cases: TypeCaseData list
      DefaultCase: DefaultCase option }

and SwitchCaseType =
    | SwitchExpression of SwitchExpressionCaseData
    | SwitchType of SwitchTypeCaseData

and LambdaData =
    { ReturnType: ResolvedType
      Parameters: Parameter list
      ScopeData: ScopeData }


and PipeOperatorData =
    { BaseExpression: Expression
      Pipes: PipeCall list } // TODO

and TupleData =
    { Expressions: Expression list }

and BuiltInFunctionData = {
    Name : FunctionName
    Generics: Generics
    Parameters : Parameter list
    ReturnType: ResolvedType
}


and MapLookupData =
    { Reference: Expression
      Lookup: Expression }

and TypeAssertionData =
    { Identifier: ScopedVariable
      Type: ResolvedType }

and MakeFunctionData =
    { Type: ResolvedType
      Args: Expression list }

and NewFunctionData = { Type: ResolvedType }

and WrapperTypeLiteralData =
    { WrapperType: WrapperTypeReference
      Argument: Expression }

and NegatedExpressionData = { Expression: Expression }

and TypeConversionData =
    { Type: ResolvedType
      Expression: Expression }

and ExprEnumCaseLiteralData = {
        CaseName : string
        CaseId : int
        Fields : {|Name : string; Value : Expression|} list
    }



and UnaryOperatorData =
    { BaseExpression: Expression
      Operator: Operator
      SecondExpression: Expression option}

// These two records are needed because ExprVarData and ZeroVariableData
// do not support patterns
and NormalVarData = {
    VariablePattern : IdentifierName list
    Type : ResolvedType option
    Expression: Expression
}

and ZeroVarData = {
    VariablePattern : IdentifierName list
    Type : ResolvedType
}

and NonToplevelDeclaration =
    | NormalVar of NormalVarData
    | ZeroVar of ZeroVarData

and ScopeConstruct =
    | ScopedExpression of Expression
    | ScopedDeclaration of NonToplevelDeclaration
    | ScopedStatement of Statement

and Statement =
    | Return of ReturnData
    | If of IfData
    | ForLoop of ForLoopData
    | WhileLoop of WhileLoopData
    | Select of SelectData
    | SwitchCase of SwitchCaseType
    | Break
    | Continue
    | Go of GoData
    | Defer of DeferData
    | Try of TryData
    | Force of ForceData

and TypeUnionTermData = 
    {
        IsApproximation : bool
        Type : ResolvedType
    }

and TypeUnionData = {
    ExportedStatus : ExportedStatus
    Name: string
    Terms : TypeUnionTermData list
}

and DeferData = ScopeData
and GoData = ScopeData

and ReturnData =
    { ReturnedExpression: Expression option }

and IfData =
    { If: {|
        ConditionalExpr: Expression
        ScopeData: ScopeData
          |}
      ElseIfs : ({|
        ConditionalExpr : Expression
        ScopeData : ScopeData 
    |} list) option
      Else : {|
        ScopeData : ScopeData 
      |} option }

and ExprVariablePattern = ExprVariableData list

and ForLoopStyle =
    | Traditional of (ExprVariablePattern * Expression) * Expression * Expression
    | ShortHand of ExprVariablePattern * Expression

and ForLoopData =
    { Style: ForLoopStyle
      ScopeData: ScopeData }

and WhileLoopData =
    { Conditional: Expression
      ScopeData: ScopeData }

and RegularSelectCase = {
    VariablePattern: VariablePattern option
    Expression : Expression
    Scope : ScopeData
}

and SelectData = { Cases: RegularSelectCase list; DefaultCase: DefaultCase option }

and DefaultCase = (* DefaultCase of *) ScopeData

and RegularCase = {
    Expression: Expression
    Scope: ScopeData
} 

and WhenCase = {CaseExpr : Expression; ResultExpr : Expression}


and WhenData =
    { Expression: Expression
      Cases: WhenCase list
      ReturnType : ResolvedType
      DefaultExpression: Expression }

and TryData =
    { Identifiers: (IdentifierName * IdentifierName) option
      ExpressionToTry: Expression
      FailureExpression: Expression }

and ForceData =
    { Identifiers: IdentifierName * IdentifierName
      ExpressionToTry: Expression
      PanicExpression: Expression }

and ScopeContent = ScopeConstruct list



[<RequireQualifiedAccess>]
type ToplevelDeclaration =
    | Function of FunctionDeclarationData
    | Import of ImportDeclarationData
    | Struct of StructDeclarationData
    | Interface of InterfaceDeclarationData
    | Alias of AliasDeclarationData
    | WrapperType of WrapperTypeDeclarationData
    | Method of MethodDeclarationData
    | Const of ConstDeclarationData
    | Enum of EnumDeclarationData



type AttributeData = 
    {
        Name : string
        Type : ResolvedType
    }

type AttributeDeclaration = 
    | ModuleImport of StructDeclarationData
    | Attribute of AttributeData list


type AccessorSupportedType = 
    | StructType of StructTypeData
    | InterfaceType of InterfaceTypeData
    | WrapperType of WrapperTypeTypeData






