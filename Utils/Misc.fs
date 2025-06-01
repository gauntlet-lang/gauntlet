(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)

module Utils.Misc

open Types
open Globals

let random = System.Random()

let (|ImportedModule|_|) (astData: Ast.ASTData) (identifierName: string) =
    astData.ImportedModules
    |> Map.tryFind identifierName

let (|DeclaredStruct|) (declarations:ResolvedAst.ResolvedDeclarations) (structName:string) = 
    declarations.Structs
    |> Map.find structName

let (|DeclaredInterface|) (declarations:ResolvedAst.ResolvedDeclarations) (interfaceName:string) = 
    declarations.Interfaces
    |> Map.find interfaceName

let (|DeclaredWrapperType|) (declarations:ResolvedAst.ResolvedDeclarations) (wrapperTypeName:string) = 
    declarations.WrapperTypes
    |> Map.find wrapperTypeName

let (|AstStruct|_|) (astData:Ast.ASTData) (structName:string) = 
    astData.Structs
    |> Map.tryFind structName

let (|AstInterface|_|) (astData:Ast.ASTData) (interfaceName:string) = 
    astData.Interfaces
    |> Map.tryFind interfaceName

let (|AstWrapperType|_|) (astData:Ast.ASTData) (wrapperTypeName:string) = 
    astData.WrapperTypes
    |> Map.tryFind wrapperTypeName

let (|AstAlias|_|) (astData:Ast.ASTData) (aliasName:string) = 
    astData.Aliases
    |> Map.tryFind aliasName



/// Applies a pred to value. If pred returns true, then the value is returned; otherwise,
/// the errMsg is thrown
let guard (pred: 'a -> bool) (errMsg: string) (contFunction: 'a -> 'b) (value: 'a) =
    let x = pred value

    if x then contFunction value else failwith errMsg

/// Makes the first letter of a string uppercase
let capitalizeStr (input: string) =
    try
        (string input[0]).ToUpper() + input[1..]
    with _ ->
        input.ToUpper()

/// Makes the first letter of a string lowercase
let lowercaseStr (input: string) =
    try
        (string input[0]).ToLower() + input[1..]
    with _ ->
        input.ToUpper()

/// Get all items in a list, excluding the last
let itemsExceptLast (input: 'T list) =
    let length = input.Length
    input[0 .. length - 2]

/// If isExported is true, then the string is capitalized; otherwise it is
/// lowercase
let adjustName (exported: Base.ExportedStatus) (name: string) =
    match exported with
    | Base.ExportedStatus.Exported -> capitalizeStr name
    | Base.ExportedStatus.Unexported -> lowercaseStr name

// let allItemsExceptLast (input: List<'a>) = input[0 .. input.Length - 2]


/// Flattens a list (only one layer)
let flattenList<'a> (input: 'a list list) =
    if input.Length > 0 then
        input |> List.reduce List.append
    else
        []

/// Runs a shell command
let runShellCommand (fileName: string) (arguments: string) (workingDir:string) (ignoreOutput: bool) =
    try
        let p = new System.Diagnostics.Process()
        p.StartInfo.FileName <- fileName

        if ignoreOutput then
            p.StartInfo.UseShellExecute <- false
            p.StartInfo.RedirectStandardOutput <- true

        p.StartInfo.Arguments <- arguments
        p.StartInfo.WorkingDirectory <- workingDir

        p.Start() |> ignore
        p.WaitForExit()

        if ignoreOutput then
            p.StandardOutput.ReadToEnd() |> ignore

        Ok()
    with e ->
        Error [e.ToString()]


(* let error (input:Base.ProgramError) = 
    match input with
    | Base.ProgramError.CompileTime(Base.Unresolved(errMsg)) ->
        printfn $"ERROR: Unresolved: {errMsg}"
        System.Environment.Exit 1
    | Base.ProgramError.RunTime(errMsg) ->
        printfn $"{errMsg}"
        System.Environment.Exit 1 *)

let error (errMsg: string) = failwith errMsg


/// If the given opt is None, then throws the message. Otherwise returns the inner value
let expect<'T> (message: string) (opt: 'T option) =
    match opt with
    | Some v -> v
    | None -> error message

/// Generates a new random number up to 1000000000
// NOTE: Not Wrapper in an effect since it would
// cause too many type-errors and issues with parsers
let newRandomNumber () =

    let maxScopeId = 1000000000
    random.Next maxScopeId


/// Adds a value to a list
let listAdd<'T> (l: 'T list) (item: 'T) = List.append [ item ] l

let removeLineComments (input: string) =
    let mutable isInString = false
    let mutable charIndex = 0
    let mutable ignoring = false
    let builtString = System.Text.StringBuilder()
    for currentChar in input do
        let nextCharacter = if charIndex + 1 < input.Length then input[charIndex + 1] else '\000'
        if ignoring && currentChar = '\n' then
            ignoring <- false
        if currentChar = '"' then
            isInString <- not isInString
        if (currentChar = '/') && (nextCharacter = '/') && (not isInString) then
            ignoring <- true
        if not ignoring then
            ignore <| builtString.Append(currentChar)
        charIndex <- charIndex + 1
    builtString
    |> _.ToString()
    |> _.Split("\n")
    |> Array.map(_.Trim())
    |> String.concat "\n"

(* let removeLineComments (input: string) =
    input.Split("\n")
    |> Array.map (fun (line: string) -> (line.Split("\-")[0]).Trim())
    |> String.concat "\n" *)

(* let removeMultiLineComments (input: string) =
    input
    |> _.Split("-/*")
    |> Array.map (fun chunk ->
        try
            chunk.Split("*-/")[1]
        with _ ->
            chunk)
    |> String.concat "" *)

let removeComments = removeLineComments


let getRandomNumber (index: int) = List.item index randomNumbers

/// Takes two strings and compares the lowercase version of them
let compareLowercase (string1: string) (string2: string) =
    lowercaseStr string1 = lowercaseStr string2

/// Takes two options and returns the first option that is not None
let optionOr (option1: 'T option) (option2: 'T option) =
    match option1, option2 with
    | Some x, _ -> Some x
    | _, Some x -> Some x
    | _, _ -> None

/// Prefixes a given string with another string
let prependString (prefix: string) (str: string) = prefix + str


// Throws custom error if pred fails for any item, otherwise returns the input list
let satisfiesAllOrError (pred: 'T -> bool) (errorMsgFunc: 'T -> string) (l: 'T list) =
    for x in l do
        if pred x then () else error (errorMsgFunc x)

    l

let fromOptionToResult (noneValue:'b) (input:Option<'a>) =
    match input with
    | Some x -> Ok x
    | None ->
        Error noneValue


let todo (errMsg:string) = error $"TODO: {errMsg}"

let mapFromKey (chooser: 'a -> 'b) (l:'a list) = 
    l
    |> List.map (fun x -> chooser x, x)
    |> Map.ofList

let tryGetLastItem (l: 'a list) =
    l
    |> List.tryItem (l.Length - 1)

let getLastItem (l: 'a list) =
    l
    |> List.item (l.Length - 1)

let tryFindOrError (key:'K) (errMsg:string) (m:Map<'K, 'V>) = 
    m
    |> Map.tryFind key
    |> fromOptionToResult errMsg
    |> Result.mapError (fun x -> [x])

let tryFindAndMap (key:'K) (mapTo:'V -> 'P) (errMsg:string) (m:Map<'K, 'V>) = 
    m
    |> tryFindOrError key errMsg
    |> Result.map mapTo

let parametersToFunctionTypeParameter (parameters:ResolvedAst.Parameter list) = 
    parameters
    |> List.map (fun paramData ->
        ({
            HasEllipsis = paramData.HasEllipsis
            Type = paramData.Type
        }):ResolvedAst.FunctionTypeParameter)

let applyResultToOption  (r: 'T -> Result<'U, 'err>) (o: 'T option) =
    match o with
    | Some x ->
        r x
        |> Result.map Some
    | None -> Ok None

let print a = printfn $"{a}"

let (|FilePath|_|) (splitter:string) (input:string) = 
    let pieces = input.Split(splitter) |> Array.toList
    let file = pieces |> List.last
    if file.EndsWith(".gaunt") then
        let fileNameWithRemovedEnding = file.Replace(".gaunt", "")
        Some <| FilePath(itemsExceptLast pieces, fileNameWithRemovedEnding)
    else
        None

let getFullFilePath (splitter:string) (relativePath:string) (currentDir:string) =
    currentDir + splitter + relativePath

let differenceOfTwoLists (a:'T list) (b:'T list) = 
    List.ofSeq <| Set.difference (Set.ofList a) (Set.ofList b)
