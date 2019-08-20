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
    let Preprocess (controls: Parameters) : PreprocessedCode =
        let isWhitespace c =
            c = ' ' || c = '\r' || c = '\n'

        let preprocessCommentsIntoWhitespace (input: (CodePosition * char) []) : (CodePosition * char) Stack =
            let output, _, potentialTrailingComment, potentialTrailingString =
                Array.fold (fun (output: (CodePosition * char) Stack, prevChar: char, inComment: Comment, inString: StringLiteral) (pos: CodePosition, nextChar: char) ->
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
                        output, ' ', NoComment, NoString // Change next char so that the code *//* does not interpret the "//" as a single line comment
                    | x, '\'', NoComment, SingleQuoteString _ when x <> '\\' ->
                        // Exit single quote string
                        Cons (output, (pos, nextChar)), ' ', NoComment, NoString
                    | x, '"', NoComment, DoubleQuoteString _ when x <> '\\' ->
                        // Exit double quote string
                        Cons (output, (pos, nextChar)), ' ', NoComment, NoString

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
                ) (Stack.Empty, ' ', NoComment, NoString) input

            // Check for dangling comment
            match potentialTrailingComment with
            | BlockComment pos -> Logger.Log Error (pos.ToString () + "Block comment never closed, use `*/'") controls
            | _ -> () // OK

            // Check for dangling string
            match potentialTrailingString with
            | NoString -> () // OK
            | SingleQuoteString pos -> Logger.Log Error (pos.ToString () + "Character literal never closed, use `''") controls
            | DoubleQuoteString pos -> Logger.Log Error (pos.ToString () + "String literal never closed, use `\"'") controls

            output

        let preprocessIndentationIntoControlChars (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            let output, remainingIndents, _ =
                Stack.fold (fun (code: (CodePosition * char) Stack, indents: int list, currentIndent: int Option) (pos: CodePosition, nextChar: char) ->
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
                            // Unindent; pop off indentation levels until the sum total matches the current indentation
                            let rec unindent (code: (CodePosition * char) Stack) (reducedIndents: int list) : (CodePosition * char) Stack * int list * int Option =
                                if (currentIndent = totalIndent reducedIndents) then
                                    // Done!
                                    Cons (code, (pos, nextChar)), reducedIndents, None
                                elif (not reducedIndents.IsEmpty && currentIndent < totalIndent reducedIndents) then
                                    // Can pop off another level and then recurse
                                    match code with
                                    // If block ends in a newline, duplicate it to after the block closes TODO newln has wrote CodePosition?
                                    | Cons (_, (_, '\n' as newln)) ->
                                        unindent (Cons (Cons (code, (pos, '\r')), newln)) reducedIndents.Tail
                                    // Otherwise, insert newlines
                                    | _ ->
                                        unindent (((code.Push (pos, '\n')).Push (pos, '\r')).Push (pos, '\n')) reducedIndents.Tail
                                else
                                    // Invalid indentation
                                    Logger.Log Error (sprintf "%sInvalid indentation of %d in context of %A" (pos.ToString ()) currentIndent reducedIndents) controls
                                    code, reducedIndents, None
                            unindent code indents
                    | _ -> Cons (code, (pos, nextChar)), indents, None // Normal code characters
                ) (Stack.Empty, List.Empty, Some 0) input

            // Close any trailing scopes
            let output =
                List.fold (fun (closedOutput: Stack<CodePosition * char>) _ ->
                    let cp: CodePosition = output.Top |> fst
                    ((closedOutput.Push (cp, '\n')).Push (cp, '\r')).Push (cp, '\n')
                ) output remainingIndents

            output

        let preprocessWhitespaceIntoMinimal (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            let output, lastChar =
                Stack.fold (fun (code: (CodePosition * char) Stack, lastChar: char) (pos: CodePosition, nextChar: char) ->
                    match lastChar, nextChar with
                    | ' ', '\n' -> Cons (code.Bottom, (pos, '\n')), nextChar // Replace previous whitespace with the newline
                    | ' ', ' ' | '\n', ' ' | '\n', '\n' -> code, nextChar // Skip this whitespace character
                    | _ -> Cons (code, (pos, nextChar)), nextChar // Keep code character
                ) (Stack.Empty, '\n') input

            output

        (controls.input
        |> IO.File.ReadAllText).ToCharArray ()
        |> Array.mapFold (fun (cp: CodePosition) (c: char) ->
            let nextCp =
                match c with // Update code position for each character
                | '\n' -> cp.NextLine
                | _ -> cp.NextChar
            (cp, c), nextCp
        ) { CodePosition.Start with file = controls.input }
        |> fst
        |> preprocessCommentsIntoWhitespace
        |> preprocessIndentationIntoControlChars
        |> preprocessWhitespaceIntoMinimal
        |> Stack.makeArray
        |> PreprocessedCode

