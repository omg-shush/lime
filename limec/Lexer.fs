namespace limec

open System

module Lexer =

    let Lex (code: PreprocessedCode) (controls: Controls) : LexedCode =
        let code = match code with PreprocessedCode code -> code
        Seq.empty
        |> LexedCode
