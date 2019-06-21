namespace limec

open System

module Lexer =

    let Lex (code: PreprocessedCode) (controls: Controls) : LexedCode =
        let code = match code with PreprocessedCode code -> code

        let unzip s = Seq.map fst s, Seq.map snd s

        let charListToString cl = List.fold (fun str c -> str + (c.ToString ())) "" cl

        let recognizeSpaces = Regex.ofString " *"
        let recognizeIdentifier = Regex.ofString "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_][abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789]*"
        let recognizeString = Regex.ofString "\"\"|\"(<\">| |\t|\\\")*<\\\\>\""
        let recognizeChar = Regex.ofString "'(<'>| |\t|\\')*'"
        let recognizeOperator = Regex.ofString @"=|\[|\]|-\>"
        let recognizeComplete = Regex.ofString "\n"
        let recognizeBegin = Regex.ofString "\t"
        let recognizeEnd = Regex.ofString "\r"

        let lexNextToken (code: (CodePosition * char) seq) : (CodePosition * Lexeme) * (CodePosition * char) seq * bool =
            let tokenStartCode = code |> Seq.skipWhile (fun (_: CodePosition, c: char) -> recognizeSpaces.Query (c.ToString ()))
            let tokenStartChars = tokenStartCode |> unzip |> snd
            let tokenStartCharsOpti = tokenStartChars |> Seq.takeWhile (fun c -> c <> '\n')
            
            let possiblyBegin = tokenStartCharsOpti |> recognizeBegin.MatchEarlyLongest |> fst
            let possiblyEnd = tokenStartCharsOpti |> recognizeEnd.MatchEarlyLongest |> fst
            let possiblyOperator = tokenStartCharsOpti |> recognizeOperator.MatchEarlyLongest |> fst
            let possiblyChar = tokenStartCharsOpti |> recognizeChar.MatchEarlyLongest |> fst
            let possiblyString = tokenStartCharsOpti |> recognizeString.MatchEarlyLongest |> fst
            let possiblyIdentifier = tokenStartCharsOpti |> recognizeIdentifier.MatchEarlyLongest |> fst
            let possiblyComplete = tokenStartChars |> recognizeComplete.MatchEarlyLongest |> fst

            let token, length =
                if (not possiblyBegin.IsEmpty) then
                    BeginBlock, possiblyBegin |> List.length
                elif (not possiblyEnd.IsEmpty) then
                    EndBlock, possiblyEnd |> List.length
                elif (not possiblyOperator.IsEmpty) then
                    Operator (possiblyOperator |> charListToString), possiblyOperator |> List.length
                elif (not possiblyChar.IsEmpty) then
                    CharLiteral (possiblyChar.[1]), possiblyChar |> List.length // TODO support other ways of describing characters than just 1 char
                elif  (not possiblyString.IsEmpty) then
                    StringLiteral (possiblyString |> charListToString), possiblyString |> List.length
                elif (not possiblyIdentifier.IsEmpty) then
                    Identifier (possiblyIdentifier |> charListToString), possiblyIdentifier |> List.length
                elif (not possiblyComplete.IsEmpty) then
                    Complete, possiblyComplete |> List.length
                else
                    Unknown, 1

            // Return token, unlexed code and whether to keep lexing
            match token with
            | Unknown -> (CodePosition.Start, token), Seq.skip length tokenStartCode, false
            | _ -> (fst (Seq.head tokenStartCode), token), Seq.skip length tokenStartCode, true

        let rec initInfiniteFold (folder: 'State -> 'Result * 'State * bool) (init: 'State) : 'Result seq * 'State =
            let result, state, keepFolding = folder init
            let resultSeq = 
                if (keepFolding) then
                    Seq.append (Seq.singleton result) (initInfiniteFold folder state |> fst)
                else
                    Seq.empty // Stop
            resultSeq, state

        (*// TODO remove
        Diagnostics.Debug.Assert (recognizeSpaces.Query "", "")
        Diagnostics.Debug.Assert (recognizeSpaces.Query " ", " ")
        Diagnostics.Debug.Assert (recognizeSpaces.Query "  ", "  ")
        Diagnostics.Debug.Assert (recognizeSpaces.Query "   ", "   ")

        // TODO remove
        Diagnostics.Debug.Assert (recognizeIdentifier.Query "E", "E")
        Diagnostics.Debug.Assert (recognizeIdentifier.MatchEarlyLongest [ 'E' ] |> fst |> List.length = 1, sprintf "%A" (recognizeIdentifier.MatchEarlyLongest [ 'E' ] |> fst))
        Diagnostics.Debug.Assert (recognizeIdentifier.Query "En", "En")
        Diagnostics.Debug.Assert (recognizeIdentifier.Query "Ent", "Ent")
        Diagnostics.Debug.Assert (recognizeIdentifier.Query "Ent8", "Ent8")
        Diagnostics.Debug.Assert (recognizeIdentifier.Query "Ent8_", "Ent8_")

        Diagnostics.Debug.Assert (recognizeOperator.Query "=", "=")
        Diagnostics.Debug.Assert (recognizeOperator.Query "[", "[")
        Diagnostics.Debug.Assert (recognizeOperator.Query "]", "]")
        Diagnostics.Debug.Assert (recognizeOperator.Query "->", "->")

        Diagnostics.Debug.Assert (recognizeString.Query "\"\"", "\"\"")
        Diagnostics.Debug.Assert (recognizeString.Query "\"a\"", "\"a\"")
        Diagnostics.Debug.Assert (recognizeString.Query "\"a c\"", "\"a c\"")
        Diagnostics.Debug.Assert (recognizeString.Query "\"abcdeF\"", "\"abcdeF\"")
        Diagnostics.Debug.Assert (recognizeString.Query "\"aa\\\"ab\"", "\"aa\\\"ab\"")
        Diagnostics.Debug.Assert (recognizeString.Query "\"Hello world from \\\"  /*lime*/!\"", "actual")*)
        
        code
        |> initInfiniteFold lexNextToken
        |> fst
        |> Seq.cache
        |> LexedCode
