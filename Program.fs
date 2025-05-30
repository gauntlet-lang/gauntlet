(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


open FParsec
open Utils.Misc
open Globals
open Types
open Utils
open Transpilers
open Test
open Types.Base
open Resolvers
open FsToolkit.ErrorHandling
open Parsers.ConstructParsers
open System.CommandLine;
open System.CommandLine.Invocation;
open System.IO

/// Returns the transpiled code
let gauntletTranspiler (ast: Ast.AST) (moduleName: ModuleName) =
    result {
        

        // ast |> List.iter (fun x -> printfn $"{x}")

        let astData = Ast.mkASTData ast
        let! resolvedDeclarations = ToplevelResolver.tryResolveDeclarations astData
        let toplevelContext = 
            ({
                ResolvedDeclarations = resolvedDeclarations
                ScopeVariables = ResolvedAst.ScopeVariables.CreateEmpty()
                AstData = astData
                ScopeGenerics = []
            }):ResolvedAst.Context


        let! resolvedAst = 
            ast
            |> List.map (ToplevelResolver.tryResolveToplevelDeclaration toplevelContext)
            |> List.sequenceResultA
            |> Result.mapError flattenList
        
        
        return
            resolvedAst
            |> List.map ToplevelDeclarationTranspilers.transpileToplevelDeclaration
            |> String.concat "\n"
            |> _.Replace("func main() ()", "func main()")
            |> prependString $"package {moduleName}\n"


    }

    
let runGauntlet (fullFilePath:string) (fileName:string) = result {
    
    let fullFilePathToGoFile = fullFilePath.Replace(".gaunt", ".go")


    let! moduleName, ast =
        fullFilePath
        |> System.IO.File.ReadLines
        |> Seq.map (_.Trim())
        |> String.concat "\n"
        |> removeComments
        |> fun x ->
            //print x
            let resultToReturn = run gauntletParser x
            // testSomething()
            //print x
            resultToReturn
        |> function
            | Success((moduleName, ast), _, _) -> Result.Ok (moduleName, ast)
            | Failure (e, _, _) -> Result.Error [e]



    print $"Successfully parsed file '{fileName}.gaunt'."

    print $"Transpiling..."
    let! transpiled = gauntletTranspiler ast moduleName
    System.IO.File.WriteAllText(fullFilePathToGoFile, transpiled)
    print $"Successfully transpiled to file '{fileName}'"

    

}

let handleGauntletResult (input:Result<unit,list<string>>) = 
    match input with
    | Result.Ok _ -> true
    | Result.Error errors ->
        print "Compile Errors Found:"
        let mutable errorNum = 1
        for error in errors do
            print $"Error {errorNum}: {error}"
            errorNum <- errorNum + 1
        false

let doFormat (fileInfo:FileInfo) =
    let goFileFullPath = fileInfo.FullName.Replace(".gaunt", ".go")
    ignore <| runShellCommand "go" $" fmt {goFileFullPath}" fileInfo.Directory.FullName true

[<EntryPoint>]
let main (args: string[]) =
    print "Gauntlet Programming Language (ALPHA): Made by TricolorHen061"

    let noFormatOptionRoot = Option<bool>("--no-format", "Skips formatting of .go file")
    let noFormatOptionRun = Option<bool>("--no-format", "Skips formatting of .go file")

    let gauntletFileArg =
        Argument<FileInfo>("Gauntlet file", "Gauntlet file ending in .gaunt")
    
    gauntletFileArg.ExistingOnly() |> ignore
    gauntletFileArg.LegalFilePathsOnly() |> ignore
    gauntletFileArg.AddValidator(fun result ->
            let file' = Option.ofNull <| result.GetValueOrDefault<FileInfo>()
            match file' with
            | Some file when not (file.Extension.EndsWith(".gaunt")) ->
                result.ErrorMessage <- "File must end in .gaunt"
            | Some _ -> ()
            | None -> ())

    let runCommand = Command("run", "Runs the transpiled Go code")
    runCommand.AddOption(noFormatOptionRun)
    runCommand.SetHandler((fun (context: InvocationContext) ->

        let fileInfo = context.ParseResult.GetValueForArgument gauntletFileArg
        let noFormat = context.ParseResult.GetValueForOption noFormatOptionRun

        let gauntEndingRemoved = fileInfo.Name.Replace(".gaunt", "")

        let wasSuccessful = 
            runGauntlet fileInfo.FullName (fileInfo.Name.Replace(".gaunt", ".go"))
            |> handleGauntletResult
        if wasSuccessful then
            let message = $"Running '{gauntEndingRemoved}.go'"
            
            let goFileFullPath = fileInfo.FullName.Replace(".gaunt", ".go")

            let underline = String.replicate message.Length "-"

            print $"{message}\n{underline}"
            runShellCommand "go" $" run {goFileFullPath}" fileInfo.Directory.FullName false
            |> ignore

            if not noFormat then doFormat fileInfo
            
    ))


    let rootCommand = RootCommand("The Gauntlet Programming Language")
    rootCommand.Name <- "gauntlet"
    rootCommand.AddCommand(runCommand)
    rootCommand.AddOption(noFormatOptionRoot)
    rootCommand.AddArgument(gauntletFileArg)
    rootCommand.SetHandler((fun (fileInfo:FileInfo) (noFormat:bool) -> (
        let wasSuccessful = 
            runGauntlet fileInfo.FullName (fileInfo.Name.Replace(".gaunt", ".go"))
            |> handleGauntletResult
        if wasSuccessful then
            if not noFormat then doFormat fileInfo
    )), gauntletFileArg, noFormatOptionRoot)
    
    rootCommand.Invoke(args)



    //runCommand.AddArgument(Argument<string>())

  (*   let splitter = if System.OperatingSystem.IsWindows() then "\\" else "/"
    let currentDirectory = System.IO.Directory.GetCurrentDirectory()

    let finalResult = 
        match args with
        | [|FilePath splitter (_, fileName) as relativePath|] ->
            let fullFilePath = getFullFilePath splitter relativePath currentDirectory
            runGauntlet fullFilePath fileName None
        | [|"run"; FilePath splitter (_, fileName) as relativePath|] ->
            let fullFilePath = getFullFilePath splitter relativePath currentDirectory
            runGauntlet fullFilePath fileName (Some Run)
        | _ ->
            Result.Error [$"Unrecognized combination of commands. Refer below for help. \n {commandUsage}"]
    
 *)
        


