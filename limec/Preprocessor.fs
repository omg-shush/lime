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
        let preprocessCommentsIntoWhitespace (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            let output, _, potentialTrailingComment, potentialTrailingString =
                List.fold (fun (output: (CodePosition * char) Stack, prevChar: char, inComment: Comment, inString: StringLiteral) (pos: CodePosition, nextChar: char) ->
                    match prevChar, nextChar, inComment, inString with
                    | '/', '/', NoComment, NoString ->
                        // Enter line comment, replacing the '/' which was mistakenly written before
                        Cons (Cons (output.Bottom, (pos, ' ')), (pos, ' ')), nextChar, LineComment, NoString
                    | _, '\n', LineComment, NoString ->
                        // Exit line comment, keeping the newline
                        Cons (output, (pos, nextChar)), nextChar, NoComment, NoString
                    | _, _, LineComment, NoString ->
                        // Stay in line comment, replacing all characters with whitespace
                        Cons (output, (pos, ' ')), nextChar, LineComment, NoString
                    | '/', '*', NoComment, NoString ->
                        // Enter block comment, replacing the '/' which was mistakenly written before
                        Cons (Cons (output.Bottom, (pos, ' ')), (pos, ' ')), nextChar, BlockComment pos, NoString
                    | '*', '/', BlockComment _, NoString ->
                        // Exit block comment
                        output, nextChar, NoComment, NoString
                    | _, _, BlockComment bc, NoString ->
                        // Stay in block comment, replacing all characters with whitespace
                        Cons (output, (pos, ' ')), nextChar, BlockComment bc, NoString
                    | _ -> Cons (output, (pos, nextChar)), nextChar, inComment, inString // TODO
                ) (Stack.Empty, ' ', NoComment, NoString) (Stack.makeList input)
            output

        let preprocessIndentationIntoControlChars (input: (CodePosition * char) Stack) : (CodePosition * char) Stack =
            input

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
