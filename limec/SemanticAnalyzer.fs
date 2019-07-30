namespace limec

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
            | Terminal (Expression, (cp, lexeme)) -> [ lexeme ]
            | _ -> invalidArg "e" "Parse tree contains a non-expression"

        let prepareExpression =
            List.map (
                fun (e: Lexeme) ->
                    match e with
                    | Identifier i -> LlamaName i |> Choice2Of2
                    | StringLiteral s -> LlamaString s |> Choice1Of2
                    | CharLiteral c -> LlamaChar c |> Choice1Of2
                    | Delimiter c -> invalidArg "code" ("Unexpected delimiter " + (c.ToString ()) + "in expression")
                    | Operator o -> LlamaOperator o |> Choice2Of2
                    | Numerical n -> (* TODO *) LlamaInt 42L |> Choice1Of2
                    | _ -> invalidArg "code" "Unexpected lexeme in expression"
            )

        /// Translates the raw ParseTree into an AbstractTypeTree, finding the definitions and relationships between types and bindings
        /// while leaving actual code expressions untouched
        let rec analyzeTypes (code: ParseTree<GrammarElement, CodePosition * Lexeme>) : AbstractTypeTree =
            match code with
            | Terminal (Expression, _) | Nonterminal (Expression, _) as exprTree ->
                let flatExpr = exprTree |> serializeExpression |> prepareExpression
                AbstractTypeTree (Association.Empty, Stack.Empty.Push flatExpr)

            | Nonterminal (Statement, [ e; _ ]) -> analyzeTypes e

            | Nonterminal (StatementList, [ stORstl; lastStatement ]) -> (analyzeTypes stORstl).Append (analyzeTypes lastStatement)

            | Nonterminal (Block, [ _; stORstl; _ ]) -> analyzeTypes stORstl

            | Nonterminal (TypeHint, [ _; typ; _ ]) ->
                // WEE WOO WEE WOO using empty name to pass type up to stack frame of containing "definition"
                AbstractTypeTree (Association.Empty.Put (LlamaName "") { typ = typFromTHint typ; def = AbstractTypeTree.Empty }, Stack.Empty)

            | Nonterminal (Definition, [ tHint; _; blockOrStatement ])
            | Nonterminal (Definition, [ tHint; blockOrStatement ]) ->
                let att = analyzeTypes blockOrStatement
                // WEE WOO WEE WOO using empty name to pass type and definition up to stack frame of containing "binding"
                let typ = match analyzeTypes tHint with AbstractTypeTree (llamas, _) -> (Option.get (llamas.Get (LlamaName ""))).typ // Extract type from "TypeHint" analysis
                AbstractTypeTree (Association.Empty.Put (LlamaName "") { LlamaType.typ = typ; def = att }, Stack.Empty)

            | Nonterminal (ImmutableBinding, [ lhs; Terminal (bindingType, _); def ]) | Nonterminal (MutableBinding, [ lhs; Terminal (bindingType, _); def ]) ->
                let name =
                    match lhs with
                    | Terminal (Expression, (_, Identifier name)) ->
                        LlamaName name // matches name = <def>
                    | Nonterminal (Expression, [ Nonterminal (Expression, [ Terminal (Expression, (_, Delimiter '(')); Terminal (Expression, (_, Operator op)) ]); Terminal (Expression, (_, Delimiter ')')) ]) ->
                        LlamaOperator op // matches (op) = <def>
                    | _ -> invalidArg "code" "Unknown lhs" // TODO pattern matching
                let ast = match analyzeTypes def with AbstractTypeTree (singleBinding, _) -> singleBinding
                let { LlamaType.typ = llamaTyp; def = AbstractTypeTree (subtyps, subcode) } = Option.get (ast.Get (LlamaName ""))
                let llamaBinding = {
                    typ = (LlamaName (if bindingType = OperationEquals then "immutable" else "mutable")) :: llamaTyp // Add on extra type based on mutability of binding
                    def = AbstractTypeTree (subtyps, subcode.Bottom.Push (Choice2Of2 (LlamaName "thunk") :: subcode.Top)) // Thunk it to prevent evaluating until actual position in code reached
                }
                let init = [ Choice2Of2 (LlamaName "unthunk"); Choice2Of2 (name) ]
                AbstractTypeTree (Association.Empty.Put name llamaBinding, Stack.Empty.Push init)
        
            | Nonterminal (Binding, [ b ]) | Nonterminal (BindingList, [ b ]) -> analyzeTypes b
            | Nonterminal (BindingList, [ bs1; bs2 ]) -> (analyzeTypes bs1).Append (analyzeTypes bs2)

            | _ -> invalidArg "code" "Improper parse tree"

        /// Given that all types have been parsed to the extent that we know how they behave within expressions,
        /// we now parse each expression into a LlamaExpression to complete the AbstractSyntaxTree
        let rec analyzeExpressions (visibleOperations: Operation<LlamaIdentifier> list) (AbstractTypeTree (types, code): AbstractTypeTree) =
            /// Returns a Operation Option. If some, add to the operator priority list; if none, ignore
            let getOp (kvpair: KeyValue<LlamaIdentifier, LlamaType>) =
                match (kvpair.key, kvpair.value) with
                | LlamaOperator _, { typ = typ; def = _ } | LlamaName _, { typ = typ; def = _ } when List.contains (LlamaName "operator") typ ->
                    let OpType =
                        if List.contains (LlamaName "infix") typ then Infix
                        elif List.contains (LlamaName "prefix") typ then Prefix
                        elif List.contains (LlamaName "postfix") typ then Postfix
                        else Infix
                    Some (OpType kvpair.key)
                | _ -> None

            // First, find all operations defined in this scope
            let theseOps = List.choose getOp types.KeyValueSet

            // Then, append them onto the previously defined ops, giving the closest ones highest precedence (* TODO they should really have more consistent sorting *)
            let allOps = theseOps @ visibleOperations

            // Now, parse the expressions at this scope level


            AbstractSyntaxTree.Empty

        code |> analyzeTypes |> analyzeExpressions [ Infix (LlamaOperator "->") ]

    (*let rec Analyze (ParsedCode tree: ParsedCode) (controls: Controls) : AST =
        let getASTFromParseTrees ptlist =
            List.fold (
                fun (ast: AST) statement ->
                    ast.Append (Analyze (ParsedCode statement) controls)
            ) ptlist

        let getLlamaFromBinding lhs rhs =
            let name =
                match lhs with
                | Terminal (Expression, (cp, Identifier name)) -> LlamaName name
                | _ -> invalidArg "tree" "lhs is not an identifier TODO"
            let llama =
                match rhs with // rhs is the children of a Definition
                | (Nonterminal (TypeHint, [ _; Terminal (Expression, (cp, Identifier typ)); _ ])) :: value ->
                    let def =
                        match value with
                        | [ Terminal (Complete, _); Nonterminal (Block, statements) ] ->
                            getASTFromParseTrees statements
                        | [ Nonterminal (Statement, [ statement; Terminal (Complete, _) ]) ] ->
                            Analyze (ParsedCode statement) controls
                        | _ -> invalidArg "tree" "improper definition"

                    { typ = [ LlamaName typ ]; def = def }
                | _ -> invalidArg "tree" "rhs's type hint is not an identifier TODO"
            name, llama

        let getLlamaFromExpression expr =
            () // TODO

        match tree with
        // Single binding cases
        | Nonterminal (Binding, [
            Nonterminal (
                ImmutableBinding, [ lhs; _; Nonterminal (Definition, rhs) ]
            )
        ]) ->
            let name, llama = getLlamaFromBinding lhs rhs
            let llama = { llama with typ = (LlamaName "immutable") :: llama.typ }
            AST (Association.Empty.Put name llama, Stack.Empty)

        | Nonterminal (Binding, [
            Nonterminal (
                MutableBinding, [ lhs; _; Nonterminal (Definition, rhs) ]
            )
        ]) ->
            let name, llama = getLlamaFromBinding lhs rhs
            AST (Association.Empty.Put name llama, Stack.Empty)

        // Recursive binding lists

        | Nonterminal (BindingList, [
            Nonterminal (BindingList, tailBindings)
            binding // Either a MutableBinding or ImmutableBinding
        ]) ->
            let headAST = Analyze (ParsedCode binding) controls
            let tailAST = getASTFromParseTrees tailBindings
            tailAST.Append headAST

        // Expressions

        | Terminal (Expression, expr) ->
            AST Association.Empty // TODO

        | Nonterminal (Expression, [ Nonterminal (Expression, tail); Terminal (Expression, head) ]) ->
            AST Association.Empty // TODO

        | _ -> AST Association.Empty // TODO
        *)