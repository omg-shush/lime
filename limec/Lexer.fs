namespace limec

open System

module Lexer =

    let Lex (controls: Parameters) (code: PreprocessedCode) : LexedCode =
        let code = match code with PreprocessedCode code -> code

        let unzip s = Array.map fst s, Array.map snd s

        let charListToString cl = List.fold (fun str c -> str + (c.ToString ())) "" cl

        let recognizeSpaces = Regex.ofString " *"
        let recognizeIdentifier = Regex.ofString "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ][abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789]*"
        let recognizeString = Regex.ofString "\"\"|\"(<\">| |\t|\\\")*(<\\\\>| |\t)\""
        //printf "!!!!!!! %A" (recognizeString.MatchEarlyLongest [ '"'; '$'; '>'; ' '; '"'; 'f'; 'o'; 'o' ])
        let recognizeChar = Regex.ofString "'(<'>| |\t|\\')*'"
        let recognizeDelimiter = Regex.ofString @"[\[\]\(\)\{\}]"
        // Options for a numerical: (_ for spacers anywhere where a digit is expected)
        // + or - digits (potentially unsigned)
        // (0h/0x hex digits) or (hex digits h/H)
        // (0 octal digits)
        // (0y/0b binary digits) or (binary digits)
        // + or - digits . digits (fF or dD or nothing)
        let regexDec = "(+|-)?[0123456789][_0123456789]*[uU]?"
        let regexHex = "0[hx][0123456789abcdefABCDEF][_0123456789abcdefABCDEF]*"// TODO hex of form "f00fH" are also identifiers, disabling for now "|[0123456789abcdefABCDEF][_0123456789abcdefABCDEF]*[hH]"
        let regexOct = "0[cq][01234567][_01234567]*|[01234567][_01234567]*[cC]"
        let regexBin = "0[by][01][_01]*|[01][_01]*[bB]"
        let regexFlo = "(+|-)?[0123456789][_0123456789]*.?([0123456789][_0123456789]*)?([fF]|[dD])?"
        let recognizeNumerical = Regex.ofString (regexDec + "|" + regexHex + "|" + regexOct + "|" + regexBin + "|" + regexFlo)
        let recognizeOperator = Regex.ofString @"[!#$%&\*+,-./;\<=\>\?@\\^_`\|~][!#$%&\*+,-./;\<=\>\?@\\^_`\|~]*"
        let recognizeComplete = Regex.ofString "\n"
        let recognizeBegin = Regex.ofString "\t"
        let recognizeEnd = Regex.ofString "\r"

        let lexNextToken (code: (CodePosition * char) seq) : (CodePosition * Lexeme) * (CodePosition * char) seq * bool =
            let tokenStartCode = code |> Seq.skipWhile (fun (_: CodePosition, c: char) -> recognizeSpaces.Query (c.ToString ())) // Strip leading spaces
            let tokenStartChars = tokenStartCode |> Seq.map (fun (x, y) -> y) // Strip code positions
            let tokenStartCharsOpti = tokenStartChars |> Seq.takeWhile (fun c -> c <> '\n') |> Array.ofSeq
            let tokenStartCharsSingle = match tokenStartChars |> Seq.tryHead with Some x -> [| x |] | None -> [||]

            let token, length =
                let possiblyBegin = tokenStartCharsSingle |> recognizeBegin.MatchEarlyLongest
                if (not possiblyBegin.IsEmpty) then
                    Lexeme.BeginBlock, possiblyBegin |> List.length
                else
                    let possiblyEnd = tokenStartCharsSingle |> recognizeEnd.MatchEarlyLongest
                    if (not possiblyEnd.IsEmpty) then
                        Lexeme.EndBlock, possiblyEnd |> List.length
                    else
                        let possiblyComplete = tokenStartCharsSingle |> recognizeComplete.MatchEarlyLongest
                        if (not possiblyComplete.IsEmpty) then
                            Lexeme.Complete, possiblyComplete |> List.length
                        else
                            let possiblyDelimiter = tokenStartCharsOpti |> recognizeDelimiter.MatchEarlyLongest
                            if (not possiblyDelimiter.IsEmpty) then
                                Lexeme.Delimiter (possiblyDelimiter.[0]), possiblyDelimiter |> List.length
                            else
                                let possiblyNumerical = tokenStartCharsOpti |> recognizeNumerical.MatchEarlyLongest
                                if (not possiblyNumerical.IsEmpty) then
                                    Lexeme.Numerical (possiblyNumerical |> charListToString), possiblyNumerical |> List.length
                                else
                                    let possiblyOperator = tokenStartCharsOpti |> recognizeOperator.MatchEarlyLongest
                                    if (not possiblyOperator.IsEmpty) then
                                        Lexeme.Operator (possiblyOperator |> charListToString), possiblyOperator |> List.length
                                    else
                                        let possiblyChar = tokenStartCharsOpti |> recognizeChar.MatchEarlyLongest
                                        if (possiblyChar.Length > 1) then
                                            Lexeme.CharLiteral (possiblyChar.[1]), possiblyChar |> List.length // TODO support other ways of describing characters than just 1 char
                                        else
                                            let possiblyString = tokenStartCharsOpti |> recognizeString.MatchEarlyLongest
                                            if (not possiblyString.IsEmpty) then
                                                Lexeme.StringLiteral (possiblyString |> charListToString), possiblyString |> List.length
                                            else
                                                let possiblyIdentifier = tokenStartCharsOpti |> recognizeIdentifier.MatchEarlyLongest
                                                if (not possiblyIdentifier.IsEmpty) then
                                                    Lexeme.Identifier (possiblyIdentifier |> charListToString), possiblyIdentifier |> List.length
                                                else
                                                    Lexeme.Unknown, 1

            // Return token (with code position of tokenStart added back in!), unlexed code and (false if we've reached an unknown token a.k.a. end of valid input, true otherwise)
            match token with
            | Lexeme.Unknown -> (CodePosition.Start, token), Seq.skip length tokenStartCode, false
            | _ -> (fst (Seq.head tokenStartCode), token), Seq.skip length tokenStartCode, true

        let rec initInfiniteFold (folder: 'State -> 'Result * 'State * bool) (init: 'State) : 'Result list =
            let result, state, keepFolding = folder init
            if keepFolding then
                result :: initInfiniteFold folder state
            else
                [] // Stop
        
        code
        |> Seq.ofArray
        |> initInfiniteFold lexNextToken
        |> LexedCode
