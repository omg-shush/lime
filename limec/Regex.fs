namespace limec

open System

type RegexOp =
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftSquare | RightSquare
    | Concatenation
    | Union
    | KleeneStar

type Regex =
    {
        text: string;
        machine: NFA<char>;
    }

    member this.Machine = this.machine

    member this.Query (input: string) : bool =
        let inputStream = input.ToCharArray () |> Seq.ofArray
        this.machine.Simulate inputStream

module Regex =

    let Alphabet = [ 'a' ]

    let private parseRegex (regex: string) : NFA<char> =
        let regexStream = regex.ToCharArray () |> List.ofArray
        let regexParsable : Choice<char, RegexOp> list =
            regexStream
            |> List.mapFold (
                fun (prevChar: char) (nextChar: char) ->
                    let charOrOp =
                        match prevChar, nextChar with
                        | '\\', _ -> Choice1Of2 nextChar
                        | _, '[' -> Choice2Of2 LeftBracket
                        | _, ']' -> Choice2Of2 RightBracket
                        | _, '<' -> Choice2Of2 LeftSquare
                        | _, '>' -> Choice2Of2 RightSquare
                        | _, '(' -> Choice2Of2 LeftParen
                        | _, ')' -> Choice2Of2 RightParen
                        | _, '|' -> Choice2Of2 Union
                        | _, '*' -> Choice2Of2 KleeneStar
                        | _ -> Choice1Of2 nextChar
                    charOrOp, nextChar
            ) '\u0000'
            |> fst
        let regexOperatorPriority =
            [
                Circumliteral (LeftBracket, RightBracket)
                Circumliteral (LeftSquare, RightSquare)
                Circumfix (LeftParen, RightParen)
                Postfix KleeneStar
                Adjacent Concatenation
                Infix Union
            ]
        let regexParseTree = ParseTree.Parse regexOperatorPriority regexParsable
        // Returns an NFA that represents the given parse tree of atomic characters and regex operations
        let rec genMachineFromParseTree (tree: ParseTree<char, RegexOp>) : NFA<char> =
            match tree.data with
            | Atom a ->
                // Return an NFA that recognizes the atomic character
                NFA.RecognizeCharacter a
            | Operation op ->
                // Generate NFAs of each child
                let childNFAs = List.map genMachineFromParseTree tree.children
                // Combine child NFA(s) based on which operation this is
                match op with
                | LeftParen | RightParen -> invalidArg "regex" "Mismatched parentheses in regex"
                | LeftBracket | RightBracket ->
                    // Return an NFA that recognizes the atomic character set
                    List.map (fun (tree: ParseTree<char, RegexOp>) ->
                        match tree.data with
                        | Atom a -> a
                        | _ -> invalidArg "regex" "Operators are not allowed inside a character class"
                    ) tree.children
                    |> Tree.ofList
                    |> NFA.RecognizeCharacterSet
                | LeftSquare | RightSquare ->
                    // Return an NFA that recognnizes the inverse of the atomic character set
                    // First, construct tree of elements to NOT include
                    let inverseTree =
                        List.map (fun (tree: ParseTree<char, RegexOp>) ->
                            match tree.data with
                            | Atom a -> a
                            | _ -> invalidArg "regex" "Operators are not allowed inside a character class"
                        ) tree.children
                        |> Tree.ofList

                    // Next, take all characters in the alphabet that are NOT in to inverseTree
                    Alphabet
                    |> List.filter (fun (c: char) -> match inverseTree.Contains c with Some _ -> false | None -> true )
                    |> Tree.ofList
                    |> NFA.RecognizeCharacterSet
                | Concatenation ->
                    // Concatenate child NFAs
                    NFA.RecognizeConcatenation childNFAs.Head childNFAs.Tail.Head // TODO assert only two child NFAs
                | Union ->
                    // Take the union of child NFAs
                    NFA.RecognizeUnion childNFAs.Head childNFAs.Tail.Head // TODO assert only two child NFAs
                | KleeneStar ->
                    // Take the Kleene of the single child NFA
                    NFA.RecognizeKleene childNFAs.Head // TODO assert only one child NFA
        genMachineFromParseTree regexParseTree // Recursively build up an NFA from the parse tree

    let ofString (regex: string) : Regex =
        {
            text = regex
            machine = parseRegex regex;
        }
