namespace limec

open System

module To =
    let Do () = raise (NotImplementedException ())

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
        sprintf "@ line %3d, char %3d: " this.line this.character
    static member Start = { line = 1; character = 1 }
    member this.PrevChar = { this with character = this.character - 1 }
    member this.NextChar = { this with character = this.character + 1 }
    member this.NextLine = { this with line = this.line + 1; character = CodePosition.Start.character }

type PreprocessedCode = PreprocessedCode of (CodePosition * char) seq

type Lexeme =
    | Identifier of string
    | StringLiteral of string
    | CharLiteral of char
    | Delimiter of char
    | Operator of string
    | Numerical of string
    | Complete
    | BeginBlock
    | EndBlock
    | Unknown

type LexedCode = LexedCode of (CodePosition * Lexeme) seq

type GrammarElement =
    | BindingList
    | ImmutableBinding
    | MutableBinding
    | OperationEquals
    | OperationColon
    | DelimitBeginType
    | DelimitEndType
    | TypeHint
    | DelimitBeginBlock
    | DelimitEndBlock
    | Block
    | Definition
    | Expression
    | Statement
    | StatementList
    | Complete

type ParsedCode = 
    | ParsedCode of ParseTree<GrammarElement, CodePosition * Lexeme>
    override this.ToString () = "ParsedCode\n" + (match this with ParsedCode pt -> pt.ToString ())

type Llama =
    {
        typ: Llama list
        def: AST
    }
and LlamaIdentifier =
    | LlamaName of string
    | LlamaOperator of string
and AST = AST of Association<string, Llama>
