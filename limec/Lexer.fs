namespace limec

open System

module Lexer =

    let Lex (controls: Parameters) (code: PreprocessedCode) : LexedCode =
        let code = match code with PreprocessedCode code -> code

        let unzip s = Seq.map fst s, Seq.map snd s

        let charListToString cl = List.fold (fun str c -> str + (c.ToString ())) "" cl

        let recognizeSpaces = Regex.ofString " *"
        let recognizeIdentifier = Regex.ofString "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ][abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789]*"
        let recognizeString = Regex.ofString "\"\"|\"(<\">| |\t|\\\")*(<\\\\>| |\t)\""
        //printf "!!!!!!! %A" (recognizeString.MatchEarlyLongest [ '"'; '$'; '>'; ' '; '"' ])
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
        let regexFlo = "(+|-)?[0123456789][_0123456789]*(.[0123456789][_0123456789]*)?([fF]|[dD])?"
        let recognizeNumerical = Regex.ofString (regexDec + "|" + regexHex + "|" + regexOct + "|" + regexBin + "|" + regexFlo)
        let recognizeOperator = Regex.ofString @"[!#$%&\*+,-./;\<=\>\?@\\^_`\|~][!#$%&\*+,-./;\<=\>\?@\\^_`\|~]*"
        let recognizeComplete = Regex.ofString "\n"
        let recognizeBegin = Regex.ofString "\t"
        let recognizeEnd = Regex.ofString "\r"

        let lexNextToken (code: (CodePosition * char) seq) : (CodePosition * Lexeme) * (CodePosition * char) seq * bool =
            let tokenStartCode = code |> Seq.skipWhile (fun (_: CodePosition, c: char) -> recognizeSpaces.Query (c.ToString ())) // Strip leading spaces
            let tokenStartChars = tokenStartCode |> unzip |> snd // Strip code positions
            let tokenStartCharsOpti = tokenStartChars |> Seq.takeWhile (fun c -> c <> '\n')
            let tokenStartCharsSingle = match tokenStartChars |> Seq.tryHead with Some x -> Seq.singleton x | None -> Seq.empty
            
            let possiblyBegin = tokenStartCharsSingle |> recognizeBegin.MatchEarlyLongest
            let possiblyEnd = tokenStartCharsSingle |> recognizeEnd.MatchEarlyLongest
            let possiblyComplete = tokenStartCharsSingle |> recognizeComplete.MatchEarlyLongest

            let possiblyDelimiter = tokenStartCharsOpti |> recognizeDelimiter.MatchEarlyLongest
            let possiblyNumerical = tokenStartCharsOpti |> recognizeNumerical.MatchStartLongest // MatchStartLongest will keep trying to match, in case of decimal points that need additional digits
            let possiblyOperator = tokenStartCharsOpti |> recognizeOperator.MatchEarlyLongest
            let possiblyChar = tokenStartCharsOpti |> recognizeChar.MatchEarlyLongest
            let possiblyString = tokenStartCharsOpti |> recognizeString.MatchEarlyLongest
            let possiblyIdentifier = tokenStartCharsOpti |> recognizeIdentifier.MatchEarlyLongest

            let token, length =
                if (not possiblyBegin.IsEmpty) then
                    Lexeme.BeginBlock, possiblyBegin |> List.length
                elif (not possiblyEnd.IsEmpty) then
                    Lexeme.EndBlock, possiblyEnd |> List.length
                elif (not possiblyComplete.IsEmpty) then
                    Lexeme.Complete, possiblyComplete |> List.length
                elif (not possiblyDelimiter.IsEmpty) then
                    Lexeme.Delimiter (possiblyDelimiter.[0]), possiblyDelimiter |> List.length
                elif (not possiblyOperator.IsEmpty) then
                    Lexeme.Operator (possiblyOperator |> charListToString), possiblyOperator |> List.length
                elif (not possiblyNumerical.IsEmpty) then
                    Lexeme.Numerical (possiblyNumerical |> charListToString), possiblyNumerical |> List.length
                elif (possiblyChar.Length > 1) then
                    Lexeme.CharLiteral (possiblyChar.[1]), possiblyChar |> List.length // TODO support other ways of describing characters than just 1 char
                elif  (not possiblyString.IsEmpty) then
                    Lexeme.StringLiteral (possiblyString |> charListToString), possiblyString |> List.length
                elif (not possiblyIdentifier.IsEmpty) then
                    Lexeme.Identifier (possiblyIdentifier |> charListToString), possiblyIdentifier |> List.length
                else
                    Lexeme.Unknown, 1

            // Return token (with code position of tokenStart added back in!), unlexed code and (false if we've reached an unknown token a.k.a. end of valid input, true otherwise)
            match token with
            | Lexeme.Unknown -> (CodePosition.Start, token), Seq.skip length tokenStartCode, false
            | _ -> (fst (Seq.head tokenStartCode), token), Seq.skip length tokenStartCode, true

        let rec initInfiniteFold (folder: 'State -> 'Result * 'State * bool) (init: 'State) : 'Result seq * 'State =
            let result, state, keepFolding = folder init
            let resultSeq = 
                if (keepFolding) then
                    Seq.append (Seq.singleton result) (initInfiniteFold folder state |> fst)
                else
                    Seq.empty // Stop
            resultSeq, state
        
        code
        |> initInfiniteFold lexNextToken
        |> fst
        |> Seq.cache
        |> LexedCode
