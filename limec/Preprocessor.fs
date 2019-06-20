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

/// <summary>
/// Preprocesses the entire input file specified via controls by completing the following steps:
/// <para> Removes unnecessary whitespace characters </para>
/// <para> Removes comments </para>
/// <para> Normalizes line endings </para>
/// <para> Recognizes string/character literals </para>
/// <para> Interprets indentations </para>
/// </summary>
let Preprocess (controls: Controls) =
    let isWhitespace (c: char) =
        c = ' ' || c = '\n' || c = '\r'
    let preprocessedCode, _, possibleTrailingComment, possibleTrailingString, possibleTrailingScopes, _, fileSize =
        controls.input
        |> IO.File.ReadAllText
        |> Array.ofSeq
        |> Array.fold (fun (codeSoFar: char list, lastChar: char, inComment: Comment, inString: StringLiteral, indents: int list, currentIndent: int Option, pos: CodePosition) (thisChar: char) ->
            // Calculate file position of next character
            let nextPos = if (thisChar = '\n') then pos.NextLine else pos.NextChar

            let totalIndents (indents: int list) : int = List.fold (fun sum indent -> sum + indent) 0 indents

            // Interpret indentation
            let indents, currentIndent, codeSoFar =
                match currentIndent with
                | Some currentIndent ->
                    match inComment, lastChar, thisChar with
                    | BlockComment _, _, _ | NoComment, ' ', _ ->
                        indents, Some (currentIndent + 1), codeSoFar
                    | NoComment, x, y when x <> '/' || y <> '/' && y <> '*' -> // Ensure not just hitting a comment
                        if (currentIndent = totalIndents indents) then
                            // Do nothing
                            indents, None, codeSoFar
                        elif (currentIndent > totalIndents indents) then
                            // Insert '\t' for "begin"
                            (currentIndent - totalIndents indents) :: indents, None, '\t' :: codeSoFar
                        else
                            // Try to insert as many '\r's as necessary
                            let rec unindent (indents: int list) (remainingIndent: int) (codeSoFar: char list) : int list * int Option * char list =
                                if (remainingIndent = 0) then
                                    // We're done!
                                    indents, None, codeSoFar
                                elif (not indents.IsEmpty && remainingIndent >= indents.Head) then
                                    unindent indents.Tail (remainingIndent - indents.Head) ('\r' :: codeSoFar)
                                else
                                    // Improper indent
                                    Logger.Log Error (sprintf "%sImproper indentation of %d, expected any of %A" (pos.ToString ()) currentIndent indents)
                                    indents, None, codeSoFar
                            unindent indents currentIndent codeSoFar
                    | _ ->
                        // Do nothing
                        indents, Some currentIndent, codeSoFar
                | None -> indents, currentIndent, codeSoFar

            // Handle strings
            match inString with
            | NoString ->
                // Handle comments
                match inComment with
                | NoComment ->
                    match lastChar, thisChar with
                    | _, '\t' ->
                        // Disallowed character (outside string/char literals)
                        Logger.Log Error (pos.ToString () + "Tab character not allowed")
                        (codeSoFar, thisChar, NoComment, NoString, indents, None, nextPos)
                    | _, '"' ->
                        // Start of a string literal
                        (thisChar :: codeSoFar, thisChar, NoComment, DoubleQuoteString pos, indents, None, nextPos)
                    | _, '\'' ->
                        // Start of a character literal
                        (thisChar :: codeSoFar, thisChar, NoComment, SingleQuoteString pos, indents, None, nextPos)
                    | '/', '/' ->
                        // Enter line comment, removing previous '/'
                        (codeSoFar.Tail, thisChar, LineComment, NoString, indents, None, nextPos)
                    | '/', '*' ->
                        // Enter block comment, removing previous '/'
                        (codeSoFar.Tail, thisChar, BlockComment pos.PrevChar, NoString, indents, currentIndent, nextPos)
                    | x, y when y = '\r' || y = '\n' ->
                        // End of line, reset currentIndent
                        // Check whether line had trailing whitespace
                        if (isWhitespace x && not codeSoFar.IsEmpty) then
                            // Change line ending to just a newline
                            ('\n' :: codeSoFar.Tail, thisChar, NoComment, NoString, indents, Some 0, nextPos)
                        else
                            // Keep previous char, just switch this char to '\n' if necessary
                            ('\n' :: codeSoFar, thisChar, NoComment, NoString, indents, Some 0, nextPos)
                    | x, y when (isWhitespace x && isWhitespace y) ->
                        // Duplicate whitespace, ignore
                        (codeSoFar, thisChar, NoComment, NoString, indents, currentIndent, nextPos)
                    | _ ->
                        // Code character
                        (thisChar :: codeSoFar, thisChar, NoComment, NoString, indents, currentIndent, nextPos)
                | LineComment ->
                    // Ignore chars until newline is found
                    if (thisChar = '\n') then
                        (codeSoFar, thisChar, NoComment, NoString, indents, Some 0, nextPos)
                    else
                        (codeSoFar, thisChar, LineComment, NoString, indents, currentIndent, nextPos)
                | BlockComment codePosition ->
                    // Ignore chars until pattern `*/' is matched
                    if (lastChar = '*' && thisChar = '/') then
                        // Use whitespace character as prevChar, so that any
                        // extra whitespace is removed & tokens surrounding comment are separated
                        (codeSoFar, ' ', NoComment, NoString, indents, currentIndent, nextPos)
                    elif (thisChar = '\n') then
                        // Reset currentIndent, so that block comments can be included in indentation
                        (codeSoFar, ' ', BlockComment codePosition, NoString, indents, Some 0, nextPos)
                    else
                        match currentIndent with
                        | Some currentIndent -> (codeSoFar, thisChar, BlockComment codePosition, NoString, indents, Some (currentIndent + 1), nextPos)
                        | None -> (codeSoFar, thisChar, BlockComment codePosition, NoString, indents, None, nextPos)
            | DoubleQuoteString cp ->
                // Add chars until unescaped double quote is found
                match lastChar, thisChar with
                | lastChar, '"' when lastChar <> '\\' ->
                    (thisChar :: codeSoFar, thisChar, NoComment, NoString, indents, currentIndent, nextPos)
                | _ ->
                    (thisChar :: codeSoFar, thisChar, NoComment, DoubleQuoteString cp, indents, currentIndent, nextPos)
            | SingleQuoteString cp ->
                // Add chars until unescaped single quote is found
                match lastChar, thisChar with
                | lastChar, '\'' when lastChar <> '\\' ->
                    (thisChar :: codeSoFar, thisChar, NoComment, NoString, indents, currentIndent, nextPos)
                | _ ->
                    (thisChar :: codeSoFar, thisChar, NoComment, SingleQuoteString cp, indents, currentIndent, nextPos)
        ) (List.empty, '\u0000', NoComment, NoString, List.empty, Some 0, CodePosition.Start)
    match possibleTrailingComment with
    | BlockComment cp ->
        Logger.Log Warning (cp.ToString () + "Block comment is never closed")
    | _ -> ()
    match possibleTrailingString with
    | SingleQuoteString cp | DoubleQuoteString cp ->
        Logger.Log Warning (cp.ToString () + "String literal is never closed")
    | _ -> ()
    // Remove a last whitespace character, if it exists, and put a final closing newline
    let preprocessedCode =
        if (not preprocessedCode.IsEmpty && preprocessedCode.Head = '\n') then
            preprocessedCode.Tail
        elif (not preprocessedCode.IsEmpty && preprocessedCode.Head = ' ') then
            '\n' :: preprocessedCode.Tail
        else
            preprocessedCode
    // Close any leftover scopes by adding the right number of closing characters
    let preprocessedCode = List.append (List.init possibleTrailingScopes.Length (fun _ -> '\r')) preprocessedCode
    List.rev preprocessedCode // Reverse since list appends to the front (TODO: switch to using a Stack instead)
