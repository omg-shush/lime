module Types

type LogLevel =
    | Info
    | Warning
    | Error

type Target =
    | Il
    | Exe

type Mode =
    | Debug
    | Release

type Verbosity =
    | Verbose
    | Terse

type Controls =
    {
        input: string;
        output: string;
        target: Target;
        mode: Mode;
        verbosity: Verbosity;
    }
