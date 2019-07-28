namespace limec

module Parser =

    let Parse (code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                Expression, [ Expression; Expression ]
                TypeHint, [ DelimitBeginType; Expression; DelimitEndType ]
                Statement, [ Expression; Complete ]
                StatementList, [ Statement; Statement ]
                StatementList, [ StatementList; Statement ]
                Block, [ DelimitBeginBlock; Statement; DelimitEndBlock ]
                Block, [ DelimitBeginBlock; StatementList; DelimitEndBlock ]
                Definition, [ TypeHint; Complete; Block ]
                Definition, [ TypeHint; Statement ]
                ImmutableBinding, [ Expression; OperationEquals; Definition ]
                MutableBinding, [ Expression; OperationColon; Definition ]
                BindingList, [ ImmutableBinding ]
                BindingList, [ MutableBinding ]
                BindingList, [ BindingList; BindingList ]
            ]
            |> ShiftReduceParser.ofRuleList
        //printfn "%A" (cfgParser.grammar.ToString ())

        // Tag each lexeme with its equivalent grammar element
        let (LexedCode code') = code
        let tagCode (codeElement: CodePosition * Lexeme) =
            let _, lexeme = codeElement
            match lexeme with
            | Operator "=" -> OperationEquals
            | Operator ":" -> OperationColon
            | Delimiter '[' -> DelimitBeginType
            | Delimiter ']' -> DelimitEndType
            | BeginBlock -> DelimitBeginBlock
            | EndBlock -> DelimitEndBlock
            | Lexeme.Complete -> Complete
            | _ -> Expression
            , codeElement
        let taggedCode = Seq.map tagCode code'

        // Use tags to build up a parse tree
        let parseTree = cfgParser.Parse taggedCode
        //Logger.Log Info ("\n" + (parseTree.ToString ())) controls

        parseTree
        |> ParsedCode
