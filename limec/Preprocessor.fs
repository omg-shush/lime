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
    let preprocessCommentsIntoWhitespace (input: (CodePosition * char)[]) : (CodePosition * char)[] =
        input

    let preprocessIndentationIntoControlChars (input: (CodePosition * char)[]) : (CodePosition * char)[] =
        input

    let preprocessWhitespaceIntoMinimal (input: (CodePosition * char)[]) : (CodePosition * char)[] =
        input

    controls.input
    |> IO.File.ReadAllText
    |> Array.ofSeq
    |> Array.mapFold (fun (cp: CodePosition) (c: char) ->
        let nextCp =
            match c with
            | '\n' -> cp.NextLine
            | _ -> cp.NextLine
        (cp, c), nextCp
    ) CodePosition.Start
    |> fst
    |> preprocessCommentsIntoWhitespace
    |> preprocessIndentationIntoControlChars
    |> preprocessWhitespaceIntoMinimal
    |> Seq.ofArray
