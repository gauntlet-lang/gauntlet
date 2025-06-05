(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

module Globals

open Types
// false, GenericType "Type", "v"

let mutable (builtInFunctions: ResolvedAst.BuiltInFunctionData list) =

    [ { Name = "append"
        Generics = [{Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"}]
        Parameters = (([{
          HasEllipsis = false
          Type = ResolvedAst.ResolvedType.SliceType({Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}})
          Name = "slice"
        }]):ResolvedAst.Parameter list)
        ReturnType = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}}
      {
        Name = "cap";
        Generics = [];
        Parameters = [{HasEllipsis = false; Type = ResolvedAst.ResolvedType.AnyType; Name = "v"}];
        ReturnType = ResolvedAst.ResolvedType.IntegerType Base.IntType}
      {Name = "clear"
       Generics = [ {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"} ]
       Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "t"} ]
       ReturnType = ResolvedAst.ResolvedType.AnyType}
      { Name = "close"
        Generics = [ {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"} ]
        Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.ChanType(Base.ReceiveOnly, ResolvedAst.ResolvedType.AnyType); Name = "c"} ]
        ReturnType = (ResolvedAst.ResolvedType.GenericType {Name = "T"; ConstraintType = ResolvedAst.ResolvedType.AnyType})}
      {Name ="complex"
       Generics = []
       Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.IntegerType Base.Float64Type; Name = "r"};
       {HasEllipsis = false; Type = ResolvedAst.ResolvedType.IntegerType Base.Float64Type; Name = "i"} ]
       ReturnType = ResolvedAst.ResolvedType.IntegerType Base.Complex64Type};
     { Name = "copy";
       Generics = [{Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"}];
       Parameters = [{HasEllipsis = false; Type = ResolvedAst.ResolvedType.SliceType({Type = 
       ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}}); Name = "dst"};
        {HasEllipsis = false; Type = ResolvedAst.ResolvedType.SliceType({Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}}); Name = "src"} ];
       ReturnType = ResolvedAst.ResolvedType.IntegerType Base.IntType};
      { Name = "delete";
        Generics = [ {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"}; {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type1"} ];
        Parameters = [
          {HasEllipsis = false; Type = ResolvedAst.ResolvedType.MapType({KeyType = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; ValueType = ResolvedAst.ResolvedType.GenericType {Name = "Type1"; ConstraintType = ResolvedAst.ResolvedType.AnyType}}); Name = "m"}
          { HasEllipsis = false; Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "key"} ]
        ReturnType = ResolvedAst.ResolvedType.NullType};
      {Name = "imag"; Generics = []; Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.IntegerType Base.Complex128Type; Name = "c"} ]; ReturnType = ResolvedAst.ResolvedType.IntegerType Base.Float64Type};
      {Name = "len"; Generics = []; Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.AnyType; Name = "v"} ]; ReturnType = ResolvedAst.ResolvedType.IntegerType Base.Int64Type};
      {Name = "max"; Generics = [ {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "Type"} ]; Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "x"}; {HasEllipsis = true; Type = ResolvedAst.ResolvedType.GenericType {Name = "T"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "y"} ]; ReturnType = ResolvedAst.ResolvedType.GenericType {Name = "T"; ConstraintType = ResolvedAst.ResolvedType.AnyType}};
      {Name = "min"; Generics = [ {Type = ResolvedAst.ResolvedType.AnyType; TypeName = "T"} ]; Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.GenericType {Name = "Type"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "x"}; {HasEllipsis = true; Type = ResolvedAst.ResolvedType.GenericType {Name = "T"; ConstraintType = ResolvedAst.ResolvedType.AnyType}; Name = "y"} ]; ReturnType = ResolvedAst.ResolvedType.GenericType {Name = "T"; ConstraintType = ResolvedAst.ResolvedType.AnyType}};
      {Name = "panic"; Generics = []; Parameters = [ {HasEllipsis = false; Type = ResolvedAst.ResolvedType.AnyType; Name = "v"} ]; ReturnType = ResolvedAst.ResolvedType.AnyType};
      {Name = "print"; Generics = []; Parameters = [ {HasEllipsis = true; Type = ResolvedAst.ResolvedType.AnyType; Name = "args"} ]; ReturnType = ResolvedAst.ResolvedType.AnyType};
      {Name = "println"; Generics = []; Parameters = [ {HasEllipsis = true; Type = ResolvedAst.ResolvedType.AnyType; Name = "args"} ]; ReturnType = ResolvedAst.ResolvedType.AnyType};
      {Name = "recover"; Generics = []; Parameters = []; ReturnType = ResolvedAst.ResolvedType.AnyType} ]


// List of modules that have been used at least once

let random = System.Random()

// Every time the program starts, generate a set of numbers
// to be used
let randomNumbers = [ 0..50 ] |> List.map (fun _ -> random.Next(1000, 100000))


