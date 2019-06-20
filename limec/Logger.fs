namespace limec

open System

module Logger =

    let Log (level: LogLevel) (msg: string) =
        match level with
        | Info -> Console.WriteLine (" [ INFO ] " + msg)
        | Warning -> Console.WriteLine (" [ WARN ] " + msg)
        | Error -> Console.Error.WriteLine (" [ ERROR ] " + msg)
