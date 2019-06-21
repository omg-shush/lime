namespace limec

open System

module Lexer =

    let Lex (code: PreprocessedCode) (controls: Controls) : LexedCode =
        let code = match code with PreprocessedCode code -> code

        let lexNextToken (code: (CodePosition * char) seq) : Lexeme * (CodePosition * char) seq =
            let recognizeSpaces = Regex.ofString " *"
            let tokenStartCode = code |> Seq.skipWhile (fun (_: CodePosition, c: char) -> recognizeSpaces.Query (c.ToString ()))

            Unchecked.defaultof<Lexeme>, Seq.empty // TODO

        Seq.empty
        |> LexedCode
