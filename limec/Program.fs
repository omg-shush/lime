namespace limec

open System

module Compiler =

    [<EntryPoint>]
    let main argv =

        // Parse arguments to get the configuration in which the compiler should run
        let parameters = Parametrizer.Parametrize argv
        Logger.Log Info (sprintf "%A" parameters) parameters

        match parameters.target with
        | Ast ->
            let sw = System.Diagnostics.Stopwatch.StartNew ()
            Preprocessor.Preprocess parameters |> Lexer.Lex parameters |> Parser.Parse parameters |> SyntaxAnalyzer.Analyze parameters
            |> (fun x ->
                sw.Stop ()
                printfn "Finished compiling in %f ms" sw.Elapsed.TotalMilliseconds
                x
            )
            |> AbstractSyntaxFile.Write parameters
        | Il -> To.Do()
        | Exe -> To.Do()
        | Intr ->
            let openLlama fileName =
                let header = Array.zeroCreate AbstractSyntaxFile.AST_HEADER.Length
                let headerSize = (IO.File.OpenRead fileName).Read (header.AsSpan ())
                if headerSize = header.Length && header = AbstractSyntaxFile.AST_HEADER then
                    AbstractSyntaxFile.Read fileName
                else
                    let sw = System.Diagnostics.Stopwatch.StartNew ()
                    Preprocessor.Preprocess parameters |> Lexer.Lex parameters |> Parser.Parse parameters |> SyntaxAnalyzer.Analyze parameters
                    |> (fun x ->
                        sw.Stop ()
                        printfn "Finished compiling in %f ms" sw.Elapsed.TotalMilliseconds
                        x
                    )

            let llamas = List.map openLlama (List.rev (parameters.input :: parameters.libraries))
            printfn "Interpreting: %A" (List.rev (parameters.input :: parameters.libraries))
            // TODO cyclic dependencies??
            // Reverse so that order of interpretation is: [ first lib, second lib, ..., user code ]
            Interpreter.Interpret parameters llamas |> ignore //|> printfn "Process returned with value `%A'"
            ()
        
        0
