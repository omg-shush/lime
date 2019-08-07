namespace limec

module Parser =

    let Parse (LexedCode code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                Expression,         [ Expression; Expression ]
                TypeHint,           [ DelimitBeginType; Expression; DelimitEndType ]
                ComplexExpression,  [ TypeHint; Expression ]
                ComplexExpression,  [ TypeHint; ComplexExpression ]
                ComplexExpression,  [ Binding; Expression ]
                ComplexExpression,  [ Binding; ComplexExpression ]
                ComplexExpression,  [ Expression; Binding ]
                ComplexExpression,  [ ComplexExpression; Binding ]
                ComplexExpression,  [ DelimitBeginBlock; ComplexExpression; DelimitEndBlock ]
                ComplexExpression,  [ DelimitBeginBlock; Expression; DelimitEndBlock ]
                Binding,            [ Expression; OperationEquals; ComplexExpression ]
                Binding,            [ Expression; OperationEquals; Expression ]
                Binding,            [ Expression; OperationColon; ComplexExpression ]
                Binding,            [ Expression; OperationColon; Expression ]
            ]
            |> ShiftReduceParser.ofRuleList

        // Tag each lexeme with its equivalent grammar element
        let tagCode (codeElement: CodePosition * Lexeme) =
            let _, lexeme = codeElement
            match lexeme with
            | Operator "=" -> OperationEquals
            | Operator ":" -> OperationColon
            | Delimiter '[' -> DelimitBeginType
            | Delimiter ']' -> DelimitEndType
            | BeginBlock -> DelimitBeginBlock
            | EndBlock -> DelimitEndBlock
            | _ -> Expression
            , codeElement
        let taggedCode =
            code
            |> Seq.filter (fun cp, lxm -> match lxm with Complete -> false | _ -> true) // Remove all Complete's, since newlines can be ignored now
            |> Seq.map tagCode // Translate lexemes into grammatical units

        // Use tags to build up a parse tree
        let parseTree = cfgParser.Parse taggedCode

        parseTree
        |> ParsedCode
