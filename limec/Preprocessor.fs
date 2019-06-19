module Preprocessor

open System

open Types

type Comment =
    | NoComment
    | LineComment
    | BlockComment of CodePosition

type StringLiteral =
    | NoString
    | SingleQuoteString of CodePosition
    | DoubleQuoteString of CodePosition

// TODO indentation
let Preprocess (controls: Controls) =
    let isWhitespace (c: char) =
        c = ' ' || c = '\n' || c = '\r'
    let preprocessedCode, _, possibleTrailingComment, possibleTrailingString, fileSize =
        controls.input
        |> IO.File.ReadAllText
        |> Array.ofSeq
        |> Array.fold (fun (codeSoFar: char list, lastChar: char, inComment: Comment, inString: StringLiteral, pos: CodePosition) (thisChar: char) ->
            let nextPos = if (thisChar = '\n') then pos.NextLine else pos.NextChar
            match inString with
            | NoString ->
                match inComment with
                | NoComment ->
                    match lastChar, thisChar with
                    | _, '\t' ->
                        // Disallowed character (outside string/char literals)
                        Logger.Log Error (pos.ToString () + "Tab character not allowed")
                        (codeSoFar, thisChar, NoComment, NoString, nextPos)
                    | _, '"' ->
                        // Start of a string literal
                        (thisChar :: codeSoFar, thisChar, NoComment, DoubleQuoteString pos, nextPos)
                    | _, '\'' ->
                        // Start of a character literal
                        (thisChar :: codeSoFar, thisChar, NoComment, SingleQuoteString pos, nextPos)
                    | '/', '/' ->
                        // Enter line comment, removing previous '/'
                        (codeSoFar.Tail, thisChar, LineComment, NoString, nextPos)
                    | '/', '*' ->
                        // Enter block comment, removing previous '/'
                        (codeSoFar.Tail, thisChar, BlockComment pos.PrevChar, NoString, nextPos)
                    | x, y when y = '\r' || y = '\n' ->
                        // Check whether line had trailing whitespace
                        if (isWhitespace x && not codeSoFar.IsEmpty) then
                            // Change line ending to just a newline
                            ('\n' :: codeSoFar.Tail, thisChar, NoComment, NoString, nextPos)
                        else
                            // Keep previous char, just switch this char to '\n' if necessary
                            ('\n' :: codeSoFar, thisChar, NoComment, NoString, nextPos)
                    | x, y when (isWhitespace x && isWhitespace y) ->
                        // Duplicate whitespace, ignore
                        (codeSoFar, thisChar, NoComment, NoString, nextPos)
                    | _ ->
                        // Code character
                        (thisChar :: codeSoFar, thisChar, NoComment, NoString, nextPos)
                | LineComment ->
                    // Ignore chars until newline is found
                    if (thisChar = '\n') then
                        (codeSoFar, thisChar, NoComment, NoString, nextPos)
                    else
                        (codeSoFar, thisChar, LineComment, NoString, nextPos)
                | BlockComment codePosition ->
                    // Ignore chars until pattern `*/' is matched
                    if (lastChar = '*' && thisChar = '/') then
                        // Use whitespace character as prevChar, so that any
                        // extra whitespace is removed & tokens surrounding comment are separated
                        (codeSoFar, ' ', NoComment, NoString, nextPos)
                    else
                        (codeSoFar, thisChar, BlockComment codePosition, NoString, nextPos)
            | DoubleQuoteString cp ->
                // Add chars until unescaped double quote is found
                match lastChar, thisChar with
                | lastChar, '"' when lastChar <> '\\' ->
                    (thisChar :: codeSoFar, thisChar, NoComment, NoString, nextPos)
                | _ ->
                    (thisChar :: codeSoFar, thisChar, NoComment, DoubleQuoteString cp, nextPos)
            | SingleQuoteString cp ->
                // Add chars until unescaped single quote is found
                match lastChar, thisChar with
                | lastChar, '\'' when lastChar <> '\\' ->
                    (thisChar :: codeSoFar, thisChar, NoComment, NoString, nextPos)
                | _ ->
                    (thisChar :: codeSoFar, thisChar, NoComment, SingleQuoteString cp, nextPos)
        ) (List.empty, '\u0000', NoComment, NoString, CodePosition.Start)
    match possibleTrailingComment with
    | BlockComment cp ->
        Logger.Log Warning (cp.ToString () + "Block comment is never closed")
    | _ -> ()
    match possibleTrailingString with
    | SingleQuoteString cp | DoubleQuoteString cp ->
        Logger.Log Warning (cp.ToString () + "String literal is never closed")
    | _ -> ()
    List.rev preprocessedCode // Reverse since list appends to the front (TODO: switch to using a Stack instead)
