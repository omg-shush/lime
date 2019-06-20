namespace limec

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
    static member Start = { line = 1; character = 1 }
    member this.PrevChar = { this with character = this.character - 1 }
    member this.NextChar = { this with character = this.character + 1 }
    member this.NextLine = { this with line = this.line + 1; character = CodePosition.Start.character }

type PreprocessedCode = CodeCharacter of seq<char * CodePosition>
