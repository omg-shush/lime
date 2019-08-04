﻿namespace limec

module SemanticAnalyzer =

    let Analyze (ParsedCode code: ParsedCode) (controls: Controls) : AbstractSyntaxTree =
        let typFromTHint tHint =
            match tHint with
            | Terminal (Expression, (_, Identifier typ)) -> [ LlamaName typ ]
            | _ -> To.Do ()

        let rec serializeExpression (e: ParseTree<GrammarElement, CodePosition * Lexeme>) =
            match e with
            | Nonterminal (Expression, [ e1; e2 ]) ->
                (serializeExpression e1) @ (serializeExpression e2)
            | Terminal (Expression, cpAndLexeme) -> [ cpAndLexeme ]
            | _ -> invalidArg "e" "Parse tree contains a non-expression"

        let prepareExpression =
            List.map (
                fun (cp: CodePosition, e: Lexeme) ->
                    cp,
                    match e with
                    | Identifier i -> LlamaName i |> Choice2Of2
                    | StringLiteral s -> LlamaString (s.Substring (1, s.Length - 2)) |> Choice1Of2 // Remove double quotes
                    | CharLiteral c -> LlamaChar c |> Choice1Of2
                    | Delimiter c -> LlamaOperator (c.ToString ()) |> Choice2Of2
                    | Operator o -> LlamaOperator o |> Choice2Of2
                    | Numerical n -> (* TODO actually parse the number into an int or float *) LlamaInt 42L |> Choice1Of2
                    | _ -> invalidArg "code" "Unexpected lexeme in expression"
            )

        /// Translates the raw ParseTree into an AbstractTypeTree, finding the definitions and relationships between types and bindings
        /// while leaving actual code expressions untouched
        let rec analyzeTypes (code: ParseTree<GrammarElement, CodePosition * Lexeme>) : AbstractTypeTree =
            match code with
            | Terminal (Expression, _) | Nonterminal (Expression, _) as exprTree ->
                let flatExpr = exprTree |> serializeExpression |> prepareExpression
                let cp = fst (List.last flatExpr)
                AbstractTypeTree (cp, Association.Empty, Stack.Empty.Push (List.unzip flatExpr |> snd)) // TODO carry individual lexeme positions into the expression parser?

            | Nonterminal (Statement, [ exprOrBinding; _ ]) | Nonterminal (Statement, [ exprOrBinding ]) -> analyzeTypes exprOrBinding

            | Nonterminal (StatementList, [ stORstl; lastStatement ]) -> (analyzeTypes stORstl).Append (analyzeTypes lastStatement)

            | Nonterminal (Block, [ _; stORstl; _ ]) -> analyzeTypes stORstl

            | Nonterminal (TypeHint, [ _; typ; _ ]) ->
                // WEE WOO WEE WOO using empty name to pass type up to stack frame of containing "definition"
                AbstractTypeTree (CodePosition.Start, Association.Empty.Put (LlamaName "") { typ = typFromTHint typ; def = AbstractTypeTree.Empty }, Stack.Empty)

            | Nonterminal (Definition, [ tHint; _; blockOrStatement ])
            | Nonterminal (Definition, [ tHint; blockOrStatement ]) ->
                let att = analyzeTypes blockOrStatement
                // WEE WOO WEE WOO using empty name to pass type and definition up to stack frame of containing "binding"
                let typ = match analyzeTypes tHint with AbstractTypeTree (_, llamas, _) -> (Option.get (llamas.Get (LlamaName ""))).typ // Extract type from "TypeHint" analysis
                AbstractTypeTree (CodePosition.Start, Association.Empty.Put (LlamaName "") { LlamaType.typ = typ; def = att }, Stack.Empty)

            | Nonterminal (ImmutableBinding, [ lhs; Terminal (bindingType, (cp, _)); def ]) | Nonterminal (MutableBinding, [ lhs; Terminal (bindingType, (cp, _)); def ]) ->
                let name =
                    match lhs with
                    | Terminal (Expression, (_, Identifier name)) ->
                        LlamaName name // matches name = <def>
                    | Nonterminal (Expression, [ Nonterminal (Expression, [ Terminal (Expression, (_, Delimiter '(')); Terminal (Expression, (_, Operator op)) ]); Terminal (Expression, (_, Delimiter ')')) ]) ->
                        LlamaOperator op // matches (op) = <def>
                    | _ -> invalidArg "code" "Unknown lhs" // TODO pattern matching
                let ast = match analyzeTypes def with AbstractTypeTree (_, singleBinding, _) -> singleBinding
                let { LlamaType.typ = llamaTyp; def = AbstractTypeTree (subcp, subtyps, subcode) } = Option.get (ast.Get (LlamaName ""))
                let llamaBinding = {
                    typ = (LlamaName (if bindingType = OperationEquals then "immutable" else "mutable")) :: llamaTyp // Add on extra type based on mutability of binding
                    def = AbstractTypeTree (subcp, subtyps, subcode)
                }
                //let init = [ Choice2Of2 (LlamaOperator "unthunk"); Choice2Of2 (name) ]
                let init = [ Choice2Of2 (LlamaOperator "$init"); Choice2Of2 name ]
                AbstractTypeTree (cp, Association.Empty.Put name llamaBinding, Stack.Empty.Push init)

            | Nonterminal (Binding, [ bindingType ]) -> analyzeTypes bindingType

            | _ -> invalidArg "code" "Improper parse tree"

        /// Given that all types have been parsed to the extent that we know how they behave within expressions,
        /// we now parse each expression into a LlamaExpression to complete the AbstractSyntaxTree
        let rec analyzeExpressions (visibleOperations: Operation<LlamaIdentifier> list) (AbstractTypeTree (cp, types, code): AbstractTypeTree) =
            /// Returns a Operation Option. If some, add to the operator priority list; if none, ignore
            let getOp (kvpair: KeyValue<LlamaIdentifier, LlamaType>) =
                match (kvpair.key, kvpair.value) with
                | LlamaOperator _, { typ = typ; def = _ } | LlamaName _, { typ = typ; def = _ } when List.contains (LlamaName "operator") typ && not (List.contains (LlamaName "override") typ) ->
                    let OpType =
                        if List.contains (LlamaName "infix") typ then Infix
                        elif List.contains (LlamaName "prefix") typ then Prefix
                        elif List.contains (LlamaName "postfix") typ then Postfix
                        else Infix
                    Some (OpType kvpair.key)
                | _ -> None

            // First, find all operations defined in this scope
            let newOps = List.choose getOp types.KeyValueSet

            // Then, append them onto the previously defined ops, giving the closest ones highest precedence (* TODO they should really have more consistent sorting *)
            let visibleOps = newOps @ visibleOperations

            // Now, parse the expressions at this scope level
            let parsedCode = List.concat code.List |> OperatorParseTree.Parse visibleOps |> LlamaExpression

            // Next, recurse over all sub-bindings
            let parsedTypes =
                List.map (fun (kvpair: KeyValue<LlamaIdentifier, LlamaType>) ->
                    kvpair.key, { Llama.typ = kvpair.value.typ; def = analyzeExpressions visibleOps kvpair.value.def } // K, V tuple
                ) types.KeyValueSet
                |> Association.Empty.PutAll

            AbstractSyntaxTree (cp, parsedTypes, parsedCode)

        let att = code |> analyzeTypes

        let builtinOperations = [
            Circumfix (LlamaOperator "(", LlamaOperator ")")

            Prefix (LlamaOperator "$init")

            Infix (LlamaOperator ".")

            Postfix (LlamaOperator "!")

            Infix (LlamaOperator "->")

            RemainingAdjacent (LlamaOperator "next")
        ]

        analyzeExpressions builtinOperations att
