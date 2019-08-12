namespace limec

open System

module To =
    let Do () = raise (NotImplementedException ())

// TODO this baaaaad. very bad.
module Counter =
    let mutable counter = 0
    let next () =
        counter <- counter + 1
        counter

type LogLevel =
    | Info
    | Warning
    | Error

type Target =
    | Il
    | Exe
    | Intr
    | Ast

type Mode =
    | Debug
    | Release

type Verbosity =
    | Verbose
    | Terse

type Parameters =
    {
        input: string;
        output: string;
        target: Target;
        mode: Mode;
        verbosity: Verbosity;
    }

type CodePosition =
    { file: string; line: int; character: int }
    override this.ToString () =
        sprintf "@ file %s, line %3d, char %3d: " this.file this.line this.character
    static member Start = { file = "unknown file"; line = 1; character = 1 }
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
    | OperationEquals
    | OperationColon
    | DelimitBeginType
    | DelimitEndType
    | TypeHint
    | DelimitBeginBlock
    | DelimitEndBlock
    | Expression
    | Statement
    | LineBreak

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
and LlamaExpression = LlamaExpression of OperatorParseTree<LlamaLiteral, LlamaIdentifier>
and AbstractTypeTree =
    | AbstractTypeTree of CodePosition * Association<LlamaIdentifier, LlamaType> * Stack<Choice<LlamaLiteral, LlamaIdentifier> list>
    member this.Append att =
        match this, att with
        | AbstractTypeTree (thisCp, thisLlamas, thisCode), AbstractTypeTree (otherCp, otherLlamas, otherCode) ->
            AbstractTypeTree (thisCp, thisLlamas.Append otherLlamas, thisCode.Append otherCode)
    member this.GenUniqueId =
        let (AbstractTypeTree (_, bindings, _)) = this
        let rec genUniqueId id =
            if bindings.ContainsKey (LlamaName (id.ToString ())) then
                genUniqueId (id + 1)
            else
                LlamaName (id.ToString ())
        genUniqueId 0
    override this.ToString () = "AST\n" + (match this with AbstractTypeTree (cp, assoc, code) -> cp.ToString() + assoc.ToString () + "\n CODE\n" + code.ToString ())
and AbstractSyntaxTree =
    | AbstractSyntaxTree of CodePosition * Association<LlamaIdentifier, Llama> * LlamaExpression
    (*member this.Append ast =
        match this, ast with
        | AbstractSyntaxTree (thisLlamas, thisCode), AbstractSyntaxTree (otherLlamas, otherCode) ->
            AbstractSyntaxTree (thisLlamas.Append otherLlamas, thisCode.Append otherCode)*)
    member private ast.toString indent =
        let spaceIndent = String.replicate (indent - 1) "|   " + "|--+ "
        let assocToString (assoc: Association<LlamaIdentifier, Llama>) =
            (assoc.Array |> Array.map (fun { key = k; value = v } -> spaceIndent + k.ToString () + " " + v.typ.ToString () + "\n" + v.def.toString (indent + 1)) |> String.concat "")
        let codeToString (LlamaExpression code: LlamaExpression) =
            let rec lispify (code: OperatorParseTree<LlamaLiteral, LlamaIdentifier>) : string =
                let word =
                    match code.data with
                    | Operation (LlamaName word)
                    | Operation (LlamaOperator word)
                    | Atom (LlamaString word) ->
                        word
                    | Atom (LlamaChar c) -> c.ToString ()
                    | Atom (LlamaInt i) -> i.ToString ()
                    | Atom (LlamaDouble d) -> d.ToString ()
                    | Atom (LlamaBool b) -> b.ToString ()
                "(" + word + (code.children |> List.map (fun child -> " " + lispify child) |> String.concat "") + ")"
            lispify code
        let (AbstractSyntaxTree (cp, assoc, code)) = ast
        (assocToString assoc) + spaceIndent + "CODE" + (codeToString code) + "\n"

    override ast.ToString () =
        "AST\n====================\n" + ast.toString 1
