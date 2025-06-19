(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Transpilers.BaseTranspilers

open Types
open Utils
open FsToolkit.ErrorHandling

let transpileFloat (input:ResolvedAst.FloatData) = 
    let prefix = if input.IsNegative then "-" else ""
    let suffix = if input.IsImaginary then "i" else ""
    prefix + string input.Float + suffix

let transpileNumber (input:ResolvedAst.NumberData) = 
    let suffix = 
        match input.Property with
        | Some Base.Imaginary -> "i"
        | Some Base.Unsigned -> "u"
        | None -> ""
    let prefix = if input.IsNegative then "-" else ""
    prefix + input.StringRepresentation + suffix

let transpileGenerics (input: ResolvedAst.Generics)  =
    if input.Length = 0 then
        ""
    else
        input
        |> List.map (fun d -> $"{d.TypeName} {Ast.transpileType d.Type}")
        |> String.concat ", "
        |> fun x -> $"[{x}]"


let transpileParameters (input:ResolvedAst.Parameter list) = 
    input
    |> List.map (fun p ->
        let prefix = if p.HasEllipsis then "..." else ""
        let transpiledType = Ast.transpileType p.Type
        p.Name + " " + prefix + transpiledType
        )
    |> String.concat ", "

let seperateVarsByCommas (input:ResolvedAst.ScopedVariable list) = 
    input
    |> List.map (_.GetName())
    |> String.concat ", "
    

let createVariableDiscarder (input:list<ResolvedAst.ScopedVariable>) = 
    let seperaredVarNames = seperateVarsByCommas input
    let underscores = List.map (fun _ -> "_") input |> String.concat ", "
    if input.Length = 0 then
        ""
    else
        $"
        // Eliminates any 'unused variable' errors
        {underscores} = {seperaredVarNames}"
