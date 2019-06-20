namespace limec

open System

module Preprocessor =

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
        let isWhitespace c =
            c = ' ' || c = '\r' || c = '\n'

        let preprocessCommentsIntoWhitespace (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            let output, _, potentialTrailingComment, potentialTrailingString =
                List.fold (fun (output: (CodePosition * char) Stack, prevChar: char, inComment: Comment, inString: StringLiteral) (pos: CodePosition, nextChar: char) ->
                    match prevChar, nextChar, inComment, inString with
                    // Entering states
                    | '/', '/', NoComment, NoString ->
                        // Enter line comment, replacing the '/' which was mistakenly written before
                        Cons (Cons (output.Bottom, (pos, ' ')), (pos, ' ')), nextChar, LineComment, NoString
                    | '/', '*', NoComment, NoString ->
                        // Enter block comment, replacing the '/' which was mistakenly written before
                        Cons (Cons (output.Bottom, (pos, ' ')), (pos, ' ')), nextChar, BlockComment pos, NoString
                    | _, '\'', NoComment, NoString ->
                        // Enter single quote string
                        Cons (output, (pos, nextChar)), nextChar, NoComment, SingleQuoteString pos
                    | _, '"', NoComment, NoString ->
                        // Enter double quote string
                        Cons (output, (pos, nextChar)), nextChar, NoComment, DoubleQuoteString pos

                    // Exiting states
                    | _, '\n', LineComment, NoString ->
                        // Exit line comment, keeping the newline
                        Cons (output, (pos, nextChar)), nextChar, NoComment, NoString
                    | '*', '/', BlockComment _, NoString ->
                        // Exit block comment
                        output, nextChar, NoComment, NoString
                    | x, '\'', NoComment, SingleQuoteString _ when x <> '\\' ->
                        // Exit single quote string
                        Cons (output, (pos, nextChar)), nextChar, NoComment, NoString
                    | x, '"', NoComment, DoubleQuoteString _ when x <> '\\' ->
                        // Exit double quote string
                        Cons (output, (pos, nextChar)), nextChar, NoComment, NoString

                    // Maintaining states
                    | _, _, LineComment, NoString ->
                        // Stay in line comment, replacing all characters with whitespace
                        let replaceChar = if (isWhitespace nextChar) then nextChar else ' '
                        Cons (output, (pos, ' ')), nextChar, LineComment, NoString
                    | _, _, BlockComment bc, NoString ->
                        // Stay in block comment, replacing all characters with whitespace
                        Cons (output, (pos, ' ')), nextChar, BlockComment bc, NoString
                    | _, _, NoComment, SingleQuoteString _
                    | _, _, NoComment, DoubleQuoteString _ ->
                        // Stay in string
                        Cons (output, (pos, nextChar)), nextChar, NoComment, inString // Stay in same type of string though
                    | _ -> Cons (output, (pos, nextChar)), nextChar, inComment, inString // TODO
                ) (Stack.Empty, ' ', NoComment, NoString) (Stack.makeList input)

            // Check for dangling comment
            match potentialTrailingComment with
            | BlockComment pos -> Logger.Log Error (pos.ToString () + "Block comment never closed, use `*/'")
            | _ -> () // OK

            // Check for dangling string
            match potentialTrailingString with
            | NoString -> () // OK
            | SingleQuoteString pos -> Logger.Log Error (pos.ToString () + "Character literal never closed, use `''")
            | DoubleQuoteString pos -> Logger.Log Error (pos.ToString () + "String literal never closed, use `\"'")

            output

        let preprocessIndentationIntoControlChars (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            let output, remainingIndents, _ =
                List.fold (fun (code: (CodePosition * char) Stack, indents: int list, currentIndent: int Option) (pos: CodePosition, nextChar: char) ->
                    match currentIndent, nextChar with
                    | Some currentIndent, ' ' -> code, indents, Some (currentIndent + 1)
                    | _, '\n' -> Cons (code, (pos, nextChar)), indents, Some 0 // Reset indentation
                    | _, '\r' -> code, indents, Some 0 // Reset and delete '\r'
                    | Some currentIndent, _ ->
                        let totalIndent (indents: int list) = List.fold (fun sum i -> sum + i) 0 indents
                        // Process this line's indentation
                        if (currentIndent = totalIndent indents) then
                            // Same indent level, do nothing
                            Cons (code, (pos, nextChar)), indents, None
                        elif (currentIndent > totalIndent indents) then
                            // Increased indent, emit '\t' and push its level
                            Cons (Cons (code, (pos, '\t')), (pos, nextChar)), (currentIndent - totalIndent indents) :: indents, None
                        else
                            // Unindent
                            let rec unindent (code: (CodePosition * char) Stack) (indents: int list) (remainingIndent: int) : (CodePosition * char) Stack * int list * int Option =
                                if (remainingIndent = totalIndent indents) then
                                    // Done!
                                    Cons (code, (pos, nextChar)), indents, None
                                elif (not indents.IsEmpty && remainingIndent > totalIndent indents) then
                                    // Can pop off another level and then recurse
                                    unindent (Cons (code, (pos, '\r'))) indents.Tail (remainingIndent - indents.Head)
                                else
                                    // Invalid indentation
                                    Logger.Log Error (pos.ToString () + "Invalid indentation")
                                    code, indents, None
                            unindent code indents currentIndent
                    | _ -> Cons (code, (pos, nextChar)), indents, None // Normal code characters
                ) (Stack.Empty, List.Empty, Some 0) (Stack.makeList input)

            // Close any trailing scopes
            let output = List.fold (fun closedOutput _ -> Cons (closedOutput, (output.Top |> fst, '\r'))) output remainingIndents

            output

        let preprocessWhitespaceIntoMinimal (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            input

        controls.input
        |> IO.File.ReadAllText
        |> List.ofSeq
        |> List.mapFold (fun (cp: CodePosition) (c: char) ->
            let nextCp =
                match c with
                | '\n' -> cp.NextLine
                | _ -> cp.NextLine
            (cp, c), nextCp
        ) CodePosition.Start
        |> fst
        |> Stack.ofList
        |> preprocessCommentsIntoWhitespace
        |> preprocessIndentationIntoControlChars
        |> preprocessWhitespaceIntoMinimal
        |> Stack.makeList
