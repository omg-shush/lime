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

type CodePosition =
    { line: int; character: int }
    override this.ToString () =
        sprintf "@ line %d, char %d: " this.line this.character

type PreprocessedCode = CodeCharacter of seq<char * CodePosition>
