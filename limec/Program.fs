namespace limec

module Compiler =

    let seqToString (chars: PreprocessedCode) : string =
        let chars = match chars with PreprocessedCode chars -> chars
        Seq.fold (fun (s: string) (_: CodePosition, c: char) ->
            s + match c with
                | '\t' -> "\\t"
                | '\r' -> "\\r"
                | _ -> c.ToString ()
        ) "\n" chars
        + "------------------"

    let tokToString (lexemes: LexedCode) : string =
        let code = match lexemes with LexedCode code -> code
        Seq.fold (fun (s: string) (cp: CodePosition, l: Lexeme) ->
            s + "\n" + cp.ToString () + l.ToString ()
        ) "" code

    [<EntryPoint>]
    let main argv =
        let controls = Controller.Control argv
        Logger.Log Info (sprintf "%A" controls) controls

        let preprocessed = Preprocessor.Preprocess controls
        Logger.Log Info (seqToString preprocessed) controls

        let lexed = Lexer.Lex preprocessed controls
        Logger.Log Info (tokToString lexed) controls

        let parsed = Parser.Parse lexed controls
        Logger.Log Info (parsed.ToString ()) controls

        0 // return an integer exit code
