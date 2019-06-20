namespace limec

open System

module Lexer =

    let Lex (code: PreprocessedCode) (controls: Controls) : LexedCode =
        Seq.empty
        |> LexedCode
