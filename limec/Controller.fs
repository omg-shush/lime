module Controller

open System

open Types

let Control (argv: string[]) : Controls =
    let defaultControls =
        {
            input = "Program.lime";
            output = "Program.exe";
            target = Exe;
            mode = Debug;
            verbosity = Terse;
        }
    Array.fold (fun controls (argument: string) ->
        if (argument.Contains "->") then
            // I/O argument
            let ioargs = argument.Split "->"
            if (ioargs.Length <> 2) then
                Logger.Log Warning ("Argument `" + argument + "' misformed, use `input-file-name->output-file-name'")
                controls
            else
                { controls with input = ioargs.[0]; output = ioargs.[1] }
        elif (argument.StartsWith "--") then
            // Flag argument
            let flagargs = (argument.Substring 2).Split '='
            if (flagargs.Length > 0) then
                match flagargs.[0].ToLower () with
                | "target" -> 
                    if (flagargs.Length = 2) then
                        match flagargs.[1].ToLower () with
                        | "il" -> { controls with target = Il }
                        | "exe" -> { controls with target = Exe }
                        | _ ->
                            Logger.Log Warning ("Argument `" + argument + "' misformed, use `--target=EXE'")
                            controls
                    else
                        Logger.Log Warning ("Argument `" + argument + "' misformed, use `--target=EXE'")
                        controls
                | "mode" ->
                    if (flagargs.Length = 2) then
                        match flagargs.[1].ToLower () with
                        | "debug" -> { controls with mode = Debug }
                        | "release" -> {controls with mode = Release }
                        | _ ->
                            Logger.Log Warning ("Argument `" + argument + "' misformed, use `--mode=DEBUG|RELEASE'")
                            controls
                    else
                        Logger.Log Warning ("Argument `" + argument + "' misformed, use `--mode=DEBUG|RELEASE'")
                        controls
                | "verbose" ->
                    { controls with verbosity = Verbose }
                | _ ->
                    Logger.Log Warning ("Flag `" + flagargs.[0] + "' unknown, skipping")
                    controls
            else
                Logger.Log Warning ("Argument `" + argument + "' unknown, skipping")
                controls
        else
            // Unknown argument
            Logger.Log Warning ("Argument `" + argument + "' unknown, skipping")
            controls
    ) defaultControls argv
