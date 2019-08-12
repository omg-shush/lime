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
        let parameters = Parametrizer.Parametrize argv
        Logger.Log Info (sprintf "%A" parameters) parameters

        match parameters.target with
        | Ast -> parameters |> Preprocessor.Preprocess |> Lexer.Lex parameters |> Parser.Parse parameters |> SyntaxAnalyzer.Analyze parameters |> ignore
        | Il -> To.Do()
        | Exe -> To.Do()
        | Intr -> 
            // Read and preprocess the raw text file into a positioned sequence of characters,
            // eliminating redundant characters and processing indentation as well
            let preprocessed = Preprocessor.Preprocess parameters
            Logger.Log Info (seqToString preprocessed) parameters

            // Merge characters together into a flat sequence of lexemes
            let lexed = Lexer.Lex parameters preprocessed
            Logger.Log Info (tokToString lexed) parameters

            let parsed = Parser.Parse parameters lexed
            Logger.Log Info (parsed.ToString ()) parameters

            let ast = SyntaxAnalyzer.Analyze parameters parsed
            Logger.Log Info (ast.ToString ()) parameters

            Interpreter.Interpret parameters ast |> ignore
        
        0
