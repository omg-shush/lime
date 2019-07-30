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
    | Binding
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
        typ: LlamaIdentifier list
        def: AbstractSyntaxTree
    }
    member this.toString indent =
        (String.replicate indent "    ") + (List.fold (fun state t -> state + " " + (t.ToString ())) " " this.typ)
        + " " + (this.def.ToString ())
    override this.ToString () = this.toString 0
and LlamaType =
    {
        typ: LlamaIdentifier list
        def: AbstractTypeTree
    }
    member this.toString indent =
        (String.replicate indent "    ") + (List.fold (fun state t -> state + " " + (t.ToString ())) " " this.typ)
        + " " + (this.def.ToString ())
    override this.ToString () = this.toString 0
and LlamaIdentifier =
    | LlamaName of string
    | LlamaOperator of string
    override this.ToString () = match this with LlamaName name -> name | LlamaOperator op -> "(" + op + ")"
and LlamaLiteral =
    | LlamaString of string
    | LlamaChar of char
    | LlamaInt of int64
    | LlamaDouble of double
    | LlamaBool of bool
and CodeStatement =
    | CodeBlock of AbstractSyntaxTree
    | CodeLine of LlamaExpression
and LlamaExpression = LlamaExpression of OperatorParseTree<LlamaLiteral, LlamaIdentifier>
and AbstractTypeTree =
    | AbstractTypeTree of Association<LlamaIdentifier, LlamaType> * Stack<Choice<LlamaLiteral, LlamaIdentifier> list>
    member this.Append att =
        match this, att with
        | AbstractTypeTree (thisLlamas, thisCode), AbstractTypeTree (otherLlamas, otherCode) ->
            AbstractTypeTree (thisLlamas.Append otherLlamas, thisCode.Append otherCode)
    override this.ToString () = "AST\n" + (match this with AbstractTypeTree (assoc, code) -> assoc.ToString () + "\n CODE\n" + code.ToString ())
and AbstractSyntaxTree =
    | AbstractSyntaxTree of Association<LlamaIdentifier, Llama> * Stack<CodeStatement>
    member this.Append ast =
        match this, ast with
        | AbstractSyntaxTree (thisLlamas, thisCode), AbstractSyntaxTree (otherLlamas, otherCode) ->
            AbstractSyntaxTree (thisLlamas.Append otherLlamas, thisCode.Append otherCode)
    override this.ToString () = "AST\n" + (match this with AbstractSyntaxTree (assoc, code) -> assoc.ToString () + "\n CODE\n" + code.ToString ())

module AbstractSyntaxTree =
    let Empty =
        AbstractSyntaxTree (Association.Empty, Stack.Empty)

module AbstractTypeTree =
    let Empty =
        AbstractTypeTree (Association.Empty, Stack.Empty)
