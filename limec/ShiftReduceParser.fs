namespace limec

type ContextFreeGrammar<'element when 'element: comparison> =
    ContextFreeGrammar of Association<'element, Choice<ContextFreeGrammar<'element>, 'element>>

type ParseTree<'element, 'token> =
    | Terminal of 'element * 'token
    | Nonterminal of 'element * ParseTree<'element, 'token> list

    member this.Element =
        match this with
        | Terminal (e, _) -> e
        | Nonterminal (e, _) -> e

    override this.ToString () =
        let rec toString (pt: ParseTree<_, _>) (indent: int) : string =
            (if indent > 0 then (String.replicate (indent - 1) "|  ") + "|--+ " else " ")
            + (pt.Element.ToString ())
            + (
                match pt with
                | Terminal (_, token) -> token.ToString ()
                | Nonterminal (_, children) -> List.fold (fun acc child -> acc + "\n" + (toString child (indent + 1))) "" children
            )
        toString this 0

type ShiftReduceParser<'element when 'element: comparison> =
    {
        grammar: ContextFreeGrammar<'element>;
    }

    /// Parses the sequence of pairs of elements and tokens. Element must be an enum type which determines
    /// which rules apply to patterns of pairs, while tokens can carry any unrelated data associated with each element
    member this.Parse (input: ('element * 'token) seq) =
        let parsedStack =
            Seq.fold (
                fun (parseStack: Stack<ParseTree<'element, 'token>>) (elementAndToken) ->
                    // Attempts to reduce the given parseStack by searching for a matching pattern in the given context free grammar
                    // Initialize workingPattern to the empty list []
                    let rec tryReduce (parseStack: Stack<ParseTree<'element, 'token>>)
                                      (ContextFreeGrammar cfg: ContextFreeGrammar<'element>)
                                      (workingPattern: Stack<ParseTree<'element, 'token>>)
                                      : Option<Stack<ParseTree<'element, 'token>>> =
                        match parseStack with
                        | Cons (bottom, top) ->
                            match cfg.Get top.Element with
                            | Some (Choice1Of2 cfg') -> tryReduce bottom cfg' (workingPattern.Push top) // recurse
                            | Some (Choice2Of2 reduction) ->
                                // return reduction
                                let children = List.rev (workingPattern.Push top).List
                                Some (bottom.Push (Nonterminal (reduction, children)))
                            | None -> None // CFG has no patterns with the current workingPattern prefix; need to keep shifting
                        | EmptyStack -> None // Parse stack exhuasted with no match found; need to keep shifting

                    // First, we shift
                    let parseStack' = parseStack.Push (Terminal elementAndToken)
                    // Then, we reduce (multiple times if possible!)
                    let rec reduceUntilCan't stack =
                        match tryReduce stack this.grammar Stack.Empty with
                        | Some reducedStack -> reduceUntilCan't reducedStack
                        | None -> stack
                    reduceUntilCan't parseStack'
            ) Stack.Empty input
        match parsedStack with
        | Cons (EmptyStack, single) -> single
        //| _ -> parsedStack.Item 0 // TODO until able to parse all of input, ignore the rest
        | _ ->
            printf "Failed to parse input, parse stack of size %d:\n%s" (parsedStack.Length) (parsedStack.ToStringRec ())
            invalidArg "input" "Failed to parse input"

module ShiftReduceParser =

    let ofRuleList (rules: ('element * 'element list) list) : ShiftReduceParser<'element> =
        /// Recursively inserts the given reduction element positioned at the given pattern in the given context free grammar
        let rec insertReductionPattern (ContextFreeGrammar cfg: ContextFreeGrammar<'element>)
                                       (reduction: 'element, pattern: 'element list)
                                       : ContextFreeGrammar<'element> =
            match pattern with
            // If pattern is just one element long, insert the reduction as the final terminal in the map
            | last::[] -> cfg.Put last (Choice2Of2 reduction)
            // Otherwise, insert tail of pattern into child tree recursively
            | head::tail ->
                let childTree =
                    match cfg.Get head with
                    | None -> ContextFreeGrammar Association.Empty // If child tree doesn't exist yet, create it
                    | Some (Choice1Of2 c) -> c
                    // If where the child tree ought to be is itself a terminal, then parsers cannot distinguish between that rule and this one, fail
                    | Some (Choice2Of2 _) ->
                        invalidArg "pattern" (sprintf "Pattern conflicts with existing, cannot distinguish: reduction %A, tail %A" reduction pattern)
                // Insert rest of reduction pattern into child tree, replacing the old child tree (if it existed)
                cfg.Put head (insertReductionPattern childTree (reduction, tail) |> Choice1Of2)
            | [] -> invalidArg "pattern" "Pattern is empty"
            |> ContextFreeGrammar

        let rules' = List.map (fun (e, el) -> e, List.rev el) rules // Reverse patterns so they are inserted from right to left
        {
            grammar = List.fold insertReductionPattern (ContextFreeGrammar Association.Empty) rules';
        }
