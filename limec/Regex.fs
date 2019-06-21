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

    /// <summary><para>
    /// Finds the smallest number of characters that matches, then extends it until the string
    /// stops matching, and then stops.
    /// </para><para>
    /// Returns the list of characters matching the regex, followed by the remaining input.
    /// </para></summary>
    member this.MatchEarlyLongest (input: char seq) : char list * char seq =
        // Turns [1, 2, 3, ...] into [ [], [1], [1, 2], [1, 2, 3], ... ]
        let basedSubsequences (s: 'T seq) =
            Seq.initInfinite (fun i ->
                match (i - 1), Seq.tryItem (i - 1) s with
                | -1, _ | _, Some _ -> Some (Seq.take i s)
                | _ -> None
            )
            |> Seq.takeWhile (fun x -> match x with Some _ -> true | None -> false)
            |> Seq.map (fun x -> x.Value)
        // Turns [1, 2, 3, ...] into [ [1, 2, 3, ...], [2, 3, ...], [3, ...], ... ]
        //let tailSubsequences (s: 'T seq) = Seq.initInfinite (fun i -> s |> Seq.skip i) // TODO Fix like above!

        let matchingList =
            //printfn "%A" (basedSubsequences input)
            //printfn "matching %s..." this.text
            input
            |> basedSubsequences
            |> Seq.skipWhile (fun (c: char seq) ->
                // Take larger and larger bases until the regex matches
                //printfn "skip!"
                not (this.machine.Simulate c)
            )
            |> Seq.takeWhile (fun (c: char seq) ->
                // Take larger and larger bases until the regex stops matching
                //printfn "take"
                this.machine.Simulate c
            )
            |> (fun x -> if (Seq.isEmpty x) then List.Empty else Seq.last x |> List.ofSeq)

        matchingList, Seq.skip (matchingList.Length) input

module Regex =

    // Printable, ASCII characters
    let Alphabet =
        seq { 33 .. 126 }
        |> Seq.map (fun (i: int) -> char i)
        |> List.ofSeq

    let private parseRegex (regex: string) : NFA<char> =
        let mapFoldChoose (mapfoldchooser: 'State -> 'T -> 'U Option * 'State) (init: 'State) (li: list<'T>) : list<'U> * 'State =
            let resultOptionList, resultState = List.mapFold mapfoldchooser init li
            let resultList = List.choose (fun x -> x) resultOptionList
            resultList, resultState

        let regexStream = regex.ToCharArray () |> List.ofArray
        let regexParsable : Choice<char, RegexOp> list =
            regexStream
            |> mapFoldChoose (
                fun (prevChar: char) (nextChar: char) ->
                    match prevChar, nextChar with
                    | '\\', '\\' -> Some (Choice1Of2 nextChar), '\u0000' // Make sure second '\\' doesn't escape anything else!
                    | _, '\\' -> None, nextChar
                    | '\\', _ -> Some (Choice1Of2 nextChar), nextChar
                    | _, '[' -> Some (Choice2Of2 LeftBracket), nextChar
                    | _, ']' -> Some (Choice2Of2 RightBracket), nextChar
                    | _, '<' -> Some (Choice2Of2 LeftSquare), nextChar
                    | _, '>' -> Some (Choice2Of2 RightSquare), nextChar
                    | _, '(' -> Some (Choice2Of2 LeftParen), nextChar
                    | _, ')' -> Some (Choice2Of2 RightParen), nextChar
                    | _, '|' -> Some (Choice2Of2 Union), nextChar
                    | _, '*' -> Some (Choice2Of2 KleeneStar), nextChar
                    | _ -> Some (Choice1Of2 nextChar), nextChar
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
