(*
 * Copyright (C) 2025 TricolorHen061 - tricolorhen061@duck.com
 *
 * Licensed under the GNU General Public License version 3 (GPLv3)
 * See LICENSE file for details.
*)


module Test

open FParsec
open Parsers.ExpressionParsers


let testSomething () =
    printfn "--------"

    let m =
        """shoppingList[1]"""

    run (index) m
    |> fun x -> printfn $"{x}"

    printfn "--------"
 
    ()