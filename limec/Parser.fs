namespace limec

module Parser =

    let Parse (code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                Expression,         [ Expression; Expression ]
                Statement,          [ Expression; Complete ]
                Statement,          [ Binding ]
                StatementList,      [ Statement; Statement ]
                StatementList,      [ StatementList; Statement ]
                Block,              [ DelimitBeginBlock; Statement; DelimitEndBlock ]
                Block,              [ DelimitBeginBlock; StatementList; DelimitEndBlock ]
                TypeHint,           [ DelimitBeginType; Expression; DelimitEndType ]
                Definition,         [ TypeHint; Complete; Block ]
                Definition,         [ TypeHint; Statement ] // Potential bug/feature: statement can be another binding! wack?
                ImmutableBinding,   [ Expression; OperationEquals; Definition ]
                MutableBinding,     [ Expression; OperationColon; Definition ]
                Binding,            [ ImmutableBinding ]
                Binding,            [ MutableBinding ]
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
