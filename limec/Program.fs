namespace limec

module Compiler =

    let seqToString (chars: (CodePosition * char) seq) : string =
        Seq.fold (fun (s: string) (_: CodePosition, c: char) ->
            s + match c with
                | '\t' -> "\\t"
                | '\r' -> "\\r"
                | _ -> c.ToString ()
        ) "" chars
        + "------------------"

    [<EntryPoint>]
    let main argv =
        let controls = Controller.Control argv
        Logger.Log Info (sprintf "%A" controls)

        let preprocessed = Preprocessor.Preprocess controls
        Logger.Log Info (seqToString preprocessed)
        0 // return an integer exit code
