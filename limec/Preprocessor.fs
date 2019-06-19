module Preprocessor

open System

open Types

type Comment =
    | NoComment
    | LineComment
    | BlockComment of CodePosition

// TODO strings, indentation
let Preprocess (controls: Controls) =
    let isWhitespace (c: char) =
        c = ' ' || c = '\n' || c = '\r'
    let preprocessedCode, _, possibleTrailingComment, fileSize =
        controls.input
        |> IO.File.ReadAllText
        |> Array.ofSeq
        |> Array.fold (fun (codeSoFar: char list, lastChar: char, inComment: Comment, pos: CodePosition) (thisChar: char) ->
            let nextPos = if (thisChar = '\n') then pos.NextLine else pos.NextChar
            match inComment with
            | NoComment ->
                match lastChar, thisChar with
                | _, '\t' -> exit(666)
                | '/', '/' ->
                    // Enter line comment, removing previous '/'
                    (codeSoFar.Tail, thisChar, LineComment, nextPos)
                | '/', '*' ->
                    // Enter block comment, removing previous '/'
                    (codeSoFar.Tail, thisChar, BlockComment pos.PrevChar, nextPos)
                | x, y when (isWhitespace x && isWhitespace y) ->
                    // Duplicate whitespace, ignore
                    (codeSoFar, thisChar, NoComment, nextPos)
                | _, '\r' ->
                    // Treat carriage returns as newlines
                    ('\n' :: codeSoFar, thisChar, NoComment, nextPos)
                | _ ->
                    // Code character
                    (thisChar :: codeSoFar, thisChar, NoComment, nextPos)
            | LineComment ->
                // Ignore chars until newline is found
                if (thisChar = '\n') then
                    (codeSoFar, thisChar, NoComment, nextPos)
                else
                    (codeSoFar, thisChar, LineComment, nextPos)
            | BlockComment codePosition ->
                // Ignore chars until pattern `*/' is matched
                if (lastChar = '*' && thisChar = '/') then
                    // Use whitespace character as prevChar, so that any
                    // extra whitespace is removed & tokens surrounding comment are separated
                    (codeSoFar, ' ', NoComment, nextPos)
                else
                    (codeSoFar, thisChar, BlockComment codePosition, nextPos)
        ) (List.empty, '\u0000', NoComment, CodePosition.Start)
    match possibleTrailingComment with
    | BlockComment cp ->
        Logger.Log Warning (cp.ToString () + "Block comment is never closed")
    | _ -> ()
    List.rev preprocessedCode // Reverse since list appends to the front
