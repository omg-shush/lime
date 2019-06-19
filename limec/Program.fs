// Learn more about F# at http://fsharp.org

open System

open Types

let listToString (chars: char list) : string =
    List.fold (fun (s: string) (c: char) ->
        s + c.ToString ()
    ) "" chars

[<EntryPoint>]
let main argv =
    let controls = Controller.Control argv
    Logger.Log Info (sprintf "%A" controls)

    let preprocessed = Preprocessor.Preprocess controls
    Logger.Log Info (listToString preprocessed)
    0 // return an integer exit code
