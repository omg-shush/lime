module Preprocessor

open System

open Types

type Comment =
    | NoComment
    | LineComment
    | BlockComment of CodePosition

let Preprocess (controls: Controls) =
    let preprocessedCode, _, possibleTrailingComment =
        controls.input
        |> IO.File.ReadAllText
        |> Array.ofSeq
        |> Array.fold (fun (codeSoFar: char list, lastChar: char, inComment: Comment) (thisChar: char) ->
            match inComment with
            | NoComment ->
                (codeSoFar, thisChar, inComment)//TODO
            | LineComment ->
                // Ignore chars until newline is found
                let inComment = if (thisChar = '\n') then NoComment else LineComment
                (codeSoFar, thisChar, inComment)
            | BlockComment codePosition ->
                // Ignore chars until pattern `*#' is matched
                let inComment = if (lastChar = '*' && thisChar = '#') then NoComment else BlockComment codePosition
                (codeSoFar, thisChar, inComment)
        ) (List.empty, '\u0000', NoComment)
    match possibleTrailingComment with
    | BlockComment cp ->
        Logger.Log Warning (cp.ToString () + "Block comment is never closed")
    | _ -> ()
    preprocessedCode
