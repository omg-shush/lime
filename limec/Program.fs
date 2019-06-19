// Learn more about F# at http://fsharp.org

open System

open Types

[<EntryPoint>]
let main argv =
    let controls = Controller.Control argv
    Logger.Log Info (sprintf "%A" controls)

    let preprocessed = Preprocessor.Preprocess controls
    0 // return an integer exit code
