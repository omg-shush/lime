namespace limec

module Parser =

    let Parse (code: LexedCode) (controls: Controls) : ParsedCode =
        let cfgParser =
            [
                TypeHint, [ DelimitBeginType; Identifier; DelimitEndType ]
                Value, [ StringLiteral ]
                Transfer, [ Value; OperationPassDataRight; Identifier ]
                Statement, [ Transfer; Complete ]
                StatementList, [ Statement; Statement ]
                StatementList, [ StatementList; Statement ]
                Block, [ DelimitBeginBlock; Statement; DelimitEndBlock ]
                Block, [ DelimitBeginBlock; StatementList; DelimitEndBlock ]
                Definition, [ Identifier; OperationEquals; TypeHint; Complete; Block ]
            ]
            |> ShiftReduceParser.ofRuleList
        //printfn "%A" (cfgParser.grammar.ToString ())

        // Tag each lexeme with its equivalent grammar element
        let (LexedCode code') = code
        let tagCode (codeElement: CodePosition * Lexeme) =
            let _, lexeme = codeElement
            match lexeme with
            | Lexeme.Identifier _ -> Identifier
            | Operator "=" -> OperationEquals
            | Operator "->" -> OperationPassDataRight
            | Delimiter '[' -> DelimitBeginType
            | Delimiter ']' -> DelimitEndType
            | BeginBlock -> DelimitBeginBlock
            | EndBlock -> DelimitEndBlock
            | Lexeme.Complete -> Complete
            | Lexeme.StringLiteral _ -> StringLiteral
            | _ -> Unknown
            , codeElement
        let taggedCode = Seq.map tagCode code'

        // Use tags to build up a parse tree
        let parseTree = cfgParser.Parse taggedCode
        //Logger.Log Info ("\n" + (parseTree.ToString ())) controls

        // TODO recurse over the entire parse tree to build up an Abstract Syntax Tree & return it
        parseTree
        |> ParsedCode
