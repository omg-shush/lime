namespace limec

module Compiler =

    /// Pretty prints a sequence of characters with position information,
    /// converting escaped characters into their respective sequences
    let seqToString (chars: PreprocessedCode) : string =
        let chars = match chars with PreprocessedCode chars -> chars
        Seq.fold (fun (s: string) (_: CodePosition, c: char) ->
            s + match c with
                | '\t' -> "\\t"
                | '\r' -> "\\r"
                | _ -> c.ToString ()
        ) "\n" chars
        + "------------------"

    /// Pretty prints a sequence of lexemes with position information, one per each line
    let tokToString (lexemes: LexedCode) : string =
        let code = match lexemes with LexedCode code -> code
        Seq.fold (fun (s: string) (cp: CodePosition, l: Lexeme) ->
            s + "\n" + cp.ToString () + l.ToString ()
        ) "" code

    [<EntryPoint>]
    let main argv =
        // Parse arguments to get the configuration in which the compiler should run
        let controls = Controller.Control argv
        Logger.Log Info (sprintf "%A" controls) controls

        // Read and preprocess the raw text file into a positioned sequence of characters,
        // eliminating redundant characters and processing indentation as well
        let preprocessed = Preprocessor.Preprocess controls
        Logger.Log Info (seqToString preprocessed) controls

        // Merge characters together into a flat sequence of lexemes
        let lexed = Lexer.Lex preprocessed controls
        Logger.Log Info (tokToString lexed) controls

        let parsed = Parser.Parse lexed controls
        Logger.Log Info (parsed.ToString ()) controls

        let ast = SemanticAnalyzer.Analyze parsed controls
        Logger.Log Info (ast.ToString ()) controls

        0
