namespace limec

type CFGPattern<'element when 'element: comparison> =
    CFGPattern of 'element list

type ContextFreeGrammar<'element when 'element: comparison> =
    ContextFreeGrammar of Association<CFGPattern<'element>, 'element>

type ShiftReduceParser<'element when 'element: comparison> =
    {
        grammar: ContextFreeGrammar<'element>;
    }

    member this.Parse (input: ('element * 'token) seq) =
        let parsedStack =
            Seq.fold (
                fun parseStack element ->
                    parseStack
            ) Stack.Empty input
        match parsedStack with
        | Cons (EmptyStack, single) -> single
        | _ -> invalidArg "input" "Failed to parse input"

module ShiftReduceParser =

    let ofRuleList (rules: ('element * CFGPattern<'element>) list) : ShiftReduceParser<'element> =
        let ruleMap =
            List.fold (
                fun (ruleMap: Association<CFGPattern<'element>, 'element>) (reduction: 'element, pattern: CFGPattern<'element>) ->
                    ruleMap.Put pattern reduction
            ) Association.Empty rules
        {
            grammar = ContextFreeGrammar ruleMap;
        }
