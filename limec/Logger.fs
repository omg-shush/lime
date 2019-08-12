namespace limec

open System

module Logger =

    let Log (level: LogLevel) (msg: string) (parameters: Parameters) =
        match level, parameters.verbosity with
        | Info, Verbose -> Console.WriteLine (" [ INFO ] " + msg)
        | Warning, _ -> Console.WriteLine (" [ WARN ] " + msg)
        | Error, _ -> Console.Error.WriteLine (" [ ERROR ] " + msg)
        | _ -> () // Suppress logging
