namespace limec

module Parser =

    type GrammarElement =
        | ImmutableBinding
        | Identifier
        | OperationEquals
        | Definition

    let Parse (code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                ImmutableBinding, CFGPattern [ Identifier; OperationEquals; Definition ];
            ]
            |> ShiftReduceParser.ofRuleList

        code
        |> ParsedCode
