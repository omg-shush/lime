namespace limec

module SyntaxAnalyzer =

    let Analyze (controls: Parameters) (ParsedCode code: ParsedCode) : AbstractSyntaxTree =
        // Extracts a type list from a given TypeHint parse tree
        let typFromTHint tHint =
            match tHint with
            | Nonterminal (TypeHint, [ Terminal (DelimitBeginType, _); Terminal (Expression, (_, Identifier typ)); Terminal (DelimitEndType, _) ]) ->
                [ LlamaName typ ] // TODO accept more complicated types, including Statement expressions
            //| Terminal (Expression, (_, Identifier typ)) -> [ LlamaName typ ]
            | _ -> To.Do ()

        let rec serializeExpression (e: ParseTree<GrammarElement, CodePosition * Lexeme>) =
            match e with
            | Nonterminal (Expression, [ e1; e2 ]) ->
            //| Nonterminal (Expression, [ e1; Terminal (LineBreak, _); e2; Terminal (LineBreak, _) ]) -> // Absorb line breaks between expressions
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
                    | Numerical n -> (* TODO actually parse the number more betterer *)
                        LlamaInt (int64 n) |> Choice1Of2
                    | _ -> invalidArg "code" "Unexpected lexeme in expression"
            )

        /// Translates the raw ParseTree into an AbstractTypeTree, finding the definitions and relationships between types and bindings
        /// while leaving actual code expressions untouched
        let rec analyzeTypes (code: ParseTree<GrammarElement, CodePosition * Lexeme>) : AbstractTypeTree =
            match code with

            // Takes a unitary expression parse tree and flattens it into a list, ready for operator parsing
            | Terminal (Expression, _) | Nonterminal (Expression, _) as exprTree ->
                let flatExpr = exprTree |> serializeExpression |> prepareExpression
                let cp = fst (List.last flatExpr)
                AbstractTypeTree (cp, Association.Empty, Stack.Empty.Push (List.unzip flatExpr |> snd)) // TODO carry individual lexeme positions into the expression parser?

            | Nonterminal (TypeHint, _) ->
                invalidArg "code" "Type hint is lonely! Not associated with any expression"

            | Nonterminal (Statement, [ Nonterminal (TypeHint, _) as typeHint; Nonterminal (Statement, _) as stmt ])
            | Nonterminal (Statement, [ Nonterminal (TypeHint, _) as typeHint; Terminal (LineBreak, _); Nonterminal (Statement, _) as stmt ]) ->
                // First, analyze the child statement
                let (AbstractTypeTree (cp, _, _)) as analyzedStmt = analyzeTypes stmt
                // Then, wrap it in an ATT to store the type hint, which will evaluate to the stmt being hinted at
                let dummyName = LlamaName ("$" + (Counter.next ()).ToString ())
                AbstractTypeTree (
                    cp,
                    Association.Empty.Put dummyName { typ = typFromTHint typeHint; def = analyzedStmt },
                    Stack.Empty.Push [ Choice2Of2 (LlamaOperator "("); Choice2Of2 (LlamaOperator "$init"); Choice2Of2 dummyName; Choice2Of2 dummyName; Choice2Of2 (LlamaOperator ")") ]
                )

            | Nonterminal (Statement, [ expr; Terminal (LineBreak, _) ]) ->
                analyzeTypes expr

            | Nonterminal (Statement, [ Terminal (DelimitBeginBlock, _); expr; Terminal (DelimitEndBlock, _); Terminal (LineBreak, _) ]) ->
                let (AbstractTypeTree (cp, _, _) as contained) = analyzeTypes expr
                let dummyName = LlamaName ("$" + (Counter.next ()).ToString ())
                AbstractTypeTree (
                    cp,
                    Association.Empty.Put dummyName { typ = []; def = contained },
                    Stack.Empty.Push [ Choice2Of2 (LlamaOperator "("); Choice2Of2 (LlamaOperator "$init"); Choice2Of2 dummyName; Choice2Of2 dummyName; Choice2Of2 (LlamaOperator ")") ]
                )

            | Nonterminal (Statement, [ stmt1; stmt2 ]) ->
                let att1 = analyzeTypes stmt1
                let att2 = analyzeTypes stmt2
                att1.Append att2

            | Nonterminal (Statement, [ lhs; bindingType; rhs ]) ->
                let lhsOps = [
                    Circumfix (LlamaOperator "(", LlamaOperator ")")
                    Customfix (RightAssociative, LlamaOperator "$tuple", [ Argument; Form (LlamaOperator ","); Argument ])
                ]
                let pattern =
                    lhs
                    |> serializeExpression
                    |> prepareExpression
                    |> List.unzip |> snd
                    |> OperatorParseTree.Parse lhsOps

                let rec bindPattern (pat: OperatorParseTree<LlamaLiteral, LlamaIdentifier>)
                                    (value: AbstractTypeTree)
                                    (types: LlamaIdentifier list)
                                    (cp: CodePosition)
                                    (bindings: Association<LlamaIdentifier, LlamaType>)
                                    (code: Choice<LlamaLiteral, LlamaIdentifier> list)
                                    : Association<LlamaIdentifier, LlamaType> * Choice<LlamaLiteral, LlamaIdentifier> list =
                    match pat.data with
                    | Operation (LlamaOperator "$tuple") ->
                        let dummyName = LlamaName ("$" + (Counter.next ()).ToString ())
                        let finalBindings, finalCode, _ =
                            List.fold (fun (bindings, code, index) child ->
                                let bindings', code' =
                                    bindPattern child (
                                        AbstractTypeTree (cp, Association.Empty, Stack.Empty.Push [
                                            Choice2Of2 (LlamaOperator "(")
                                            Choice2Of2 (LlamaOperator "(")
                                            Choice1Of2 (LlamaInt index)
                                            Choice2Of2 (LlamaOperator ",")
                                            Choice2Of2 dummyName
                                            Choice2Of2 (LlamaOperator ")")
                                            Choice2Of2 (LlamaOperator "->")
                                            Choice2Of2 (LlamaName "nth")
                                            Choice2Of2 (LlamaOperator ")")
                                        ])) types cp bindings code
                                bindings', code', index + 1L
                            ) (
                                Association.Empty.Put dummyName { typ = []; def = value },
                                [ Choice2Of2 (LlamaOperator "$init"); Choice2Of2 dummyName ],
                                1L
                            ) pat.children
                        finalBindings, finalCode

                    | Operation (LlamaName _ as var) ->
                        bindings.Put var { typ = types; def = value },
                        code @ [ Choice2Of2 (LlamaOperator "$init"); Choice2Of2 var ]
                    | _ -> invalidArg "code" "Invalid pattern match"
                //let cp, varName = match lhs with Terminal (Expression, (cp, Identifier name)) -> cp, LlamaName name | _ -> To.Do() // TODO pattern matching / custom operators
                //let att = analyzeTypes rhs
                let additionalTypes =
                    match bindingType with
                    | Terminal (OperationColon, _) -> [ LlamaName "mutable" ]
                    | Terminal (OperationEquals, _) -> [ LlamaName "immutable" ]
                    | _ -> To.Do() // TODO different binding types?
                let bindings, code =
                    bindPattern pattern (analyzeTypes rhs) additionalTypes (CodePosition.Start (*TODO*)) Association.Empty List.Empty
                AbstractTypeTree (CodePosition.Start (*TODO*), bindings, Stack.Empty.Push code)

            | _ -> invalidArg "code" (sprintf "Improper parse tree: %A" code)

        /// Given that all types have been parsed to the extent that we know how they behave within expressions,
        /// we now parse each expression into a LlamaExpression to complete the AbstractSyntaxTree
        let rec analyzeExpressions (visibleOperations: Operation<LlamaIdentifier> list) (AbstractTypeTree (cp, types, code): AbstractTypeTree) =
            /// Returns a Operation Option. If some, add to the operator priority list; if none, ignore
            let getOp (kvpair: KeyValue<LlamaIdentifier, LlamaType>) =
                match (kvpair.key, kvpair.value) with
                | LlamaOperator _, { typ = typ; def = _ } // TODO define ops where lhs is pattern?? is that nonsensical?
                | LlamaName _, { typ = typ; def = _ } when List.contains (LlamaName "operator") typ && not (List.contains (LlamaName "override") typ) ->
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
            let allExpressions = List.concat code.List
            // TODO what if allExpressions is empty?
            let parsedCode = allExpressions |> OperatorParseTree.Parse visibleOps |> LlamaExpression

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

            Infix (LlamaOperator "==")

            Infix (LlamaOperator "->")

            Postfix (LlamaOperator "!")

            Customfix (RightAssociative, LlamaOperator "$tuple", [ Argument; Form (LlamaOperator ","); Argument ])

            Customfix (RightAssociative, LlamaOperator "$if-then-else", [ Form (LlamaName "if"); Argument; Form (LlamaName "then"); Argument; Form (LlamaName "else"); Argument ])

            RemainingAdjacent (LlamaOperator "next")
        ]

        analyzeExpressions builtinOperations att
