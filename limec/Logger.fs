namespace limec

open System

module Logger =

    let Log (level: LogLevel) (msg: string) (controls: Controls) =
        match level, controls.verbosity with
        | Info, Verbose -> Console.WriteLine (" [ INFO ] " + msg)
        | Warning, _ -> Console.WriteLine (" [ WARN ] " + msg)
        | Error, _ -> Console.Error.WriteLine (" [ ERROR ] " + msg)
        | _ -> () // Suppress logging
