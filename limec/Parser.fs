namespace limec

module Parser =

    let Parse (LexedCode code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                Expression, [ Expression; Expression ]

                TypeHint, [ DelimitBeginType; Expression; DelimitEndType ]
                TypeHint, [ DelimitBeginType; Statement; DelimitEndType ]

                Statement, [ TypeHint; Statement ]
                Statement, [ TypeHint; LineBreak; Statement ]
                Statement, [ Expression; LineBreak ]
                Statement, [ DelimitBeginBlock; Statement; DelimitEndBlock; LineBreak ] // TODO change preprocessor so linebreak is no longer needed
                Statement, [ Statement; Statement ]

                Statement, [ Expression; OperationEquals; Statement ]
                Statement, [ Expression; OperationColon; Statement ]
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
            | Complete -> LineBreak
            | _ -> Expression
            , codeElement
        let taggedCode =
            code
            //|> Seq.filter (fun (cp, lxm) -> match lxm with Complete -> false | _ -> true) // Remove all Complete's, since newlines can be ignored now
            |> Seq.map tagCode // Translate lexemes into grammatical units

        // Use tags to build up a parse tree
        let parseTree = cfgParser.Parse taggedCode

        parseTree
        |> ParsedCode

        (*
        let cfgParser =
            [
                Expression,         [ Expression; Expression ]
                Expression,         [ Expression; LineBreak; Expression; LineBreak ]
                TypeHint,           [ DelimitBeginType; Expression; DelimitEndType ]
                Binding,            [ Expression; OperationEquals; ComplexExpression; LineBreak ]
                Binding,            [ Expression; OperationEquals; Expression; LineBreak ]
                Binding,            [ Expression; OperationColon; ComplexExpression; LineBreak ]
                Binding,            [ Expression; OperationColon; Expression; LineBreak ]

                // Build a complex expression from smaller expressions
                ComplexExpression,  [ ComplexExpression; Expression; LineBreak ]
                ComplexExpression,  [ ComplexExpression; LineBreak; Expression; LineBreak ]
                ComplexExpression,  [ ComplexExpression; ComplexExpression ]
                ComplexExpression,  [ ComplexExpression; LineBreak; ComplexExpression ]
                ComplexExpression,  [ Expression; ComplexExpression ]
                ComplexExpression,  [ Expression; LineBreak; ComplexExpression ]

                // From a type hint
                ComplexExpression,  [ TypeHint; Expression ]
                ComplexExpression,  [ TypeHint; LineBreak; Expression ]
                ComplexExpression,  [ TypeHint; ComplexExpression ]
                ComplexExpression,  [ TypeHint; LineBreak; ComplexExpression ]

                // From a binding
                ComplexExpression,  [ Binding; Binding ]
                ComplexExpression,  [ Binding; Expression; LineBreak ]
                ComplexExpression,  [ Binding; ComplexExpression ]
                ComplexExpression,  [ Expression; Binding ]
                ComplexExpression,  [ ComplexExpression; Binding ]

                // From a block
                ComplexExpression,  [ DelimitBeginBlock; ComplexExpression; LineBreak; DelimitEndBlock ]
                ComplexExpression,  [ DelimitBeginBlock; ComplexExpression; DelimitEndBlock ]
                ComplexExpression,  [ DelimitBeginBlock; Expression; LineBreak; DelimitEndBlock ]
                ComplexExpression,  [ DelimitBeginBlock; Expression; DelimitEndBlock ]
            ]
            |> ShiftReduceParser.ofRuleList
        *)