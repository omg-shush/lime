namespace limec

module Parser =

    let Parse (controls: Parameters) (LexedCode code: LexedCode) : ParsedCode =
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
