namespace limec

open System

type RegexOp =
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftSquare | RightSquare
    | Concatenation
    | Union
    | KleeneStar
    | QuestionMark

type Regex =
    {
        text: string;
        machine: NFA<char>;
    }

    member this.Machine = this.machine

    member this.Query (input: string) : bool =
        let inputStream = input.ToCharArray () |> Seq.ofArray
        this.machine.Simulate inputStream

    member this.MatchImmediateLongest (input: char []) : char list =
        this.machine.SimulateImmediateLongest input

    /// <summary><para>
    /// Finds the smallest number of characters that matches, then extends it until the string
    /// stops matching, and then stops.
    /// </para><para>
    /// Returns the list of characters matching the regex.
    /// </para></summary>

    member this.MatchEarlyLongest (input: char []) : char list =
        this.machine.SimulateEarlyLongest input

    member this.MatchEarlyLongestOld (input: char seq) : char list = //* char seq =
        // Turns [1, 2, 3, ...] into [ [], [1], [1, 2], [1, 2, 3], ... ]
        let basedSubsequences (s: 'T seq) = Seq.init (Seq.length input) (fun i -> Seq.take (i + 1) s) // More optimized version
        // Turns [1, 2, 3, ...] into [ [1, 2, 3, ...], [2, 3, ...], [3, ...], ... ]
        let tailSubsequences (s: 'T seq) = Seq.init (Seq.length input) (fun i -> Seq.take i s) // Unused for now

        let matchingList =
            input
            |> basedSubsequences
            |> Seq.skipWhile (fun (c: char seq) ->
                // Take larger and larger bases until the regex matches
                not (this.machine.Simulate c)
            )
            |> Seq.takeWhile (fun (c: char seq) ->
                // Take larger and larger bases until the regex stops matching
                this.machine.Simulate c
            )
            |> (fun x -> if (Seq.isEmpty x) then List.Empty else Seq.last x |> List.ofSeq)

        matchingList//, Seq.skip (matchingList.Length) input

    // UNUSED 
    /// <summary><para>
    /// Finds the absolute largest sequence of input characters from the beginning that matches this regex.
    /// </para><para>
    /// Returns the list of characters matching the regex.
    /// </para></summary>
    (*member this.MatchStartLongest (input: char seq) : char list =
        this.machine.SimulateStartLongest input

    member this.MatchStartLongestOld (input: char seq) : char list =
        // Turns [1, 2, 3, ...] into [ [1], [1, 2], [1, 2, 3], ... ]
        let basedSubsequences (s: 'T seq) = Seq.init (Seq.length input) (fun i -> Seq.take (i + 1) s)

        input
        |> basedSubsequences
        |> Seq.fold (fun (longest: char seq) (next: char seq) ->
            // Take larger and larger bases until the regex matches
            if (this.machine.Simulate next) then
                next // Next is the new longest match!
            else
                longest // Stick with the old match
        ) Seq.empty
        |> List.ofSeq*)

module Regex =

    // Printable, non-whitespace ASCII characters
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
                    | '\\', '\\' -> Some (Choice1Of2 nextChar), '\u0000' // Make sure second '\\' doesn't escape anything after it!
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
                    | _, '?' -> Some (Choice2Of2 QuestionMark), nextChar
                    | _ -> Some (Choice1Of2 nextChar), nextChar
            ) '\u0000'
            |> fst
        // Lists operator precedence, from highest to lowest
        let regexOperatorPriority =
            [
                Circumfix (LeftParen, RightParen)
                Circumliteral (LeftBracket, RightBracket)
                Circumliteral (LeftSquare, RightSquare)
                Postfix KleeneStar
                Postfix QuestionMark
                Adjacent Concatenation
                Infix Union
            ]
        let regexParseTree = OperatorParseTree.Parse regexOperatorPriority regexParsable
        // Returns an NFA that represents the given parse tree of atomic characters and regex operations
        let rec genMachineFromParseTree (tree: OperatorParseTree<char, RegexOp>) : NFA<char> =
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
                    List.map (fun (tree: OperatorParseTree<char, RegexOp>) ->
                        match tree.data with
                        | Atom a -> a
                        | Operation op -> invalidArg "regex" ("Operator `" + op.ToString () + "' must be escaped with a `\\' when in a character class")
                    ) tree.children
                    |> Tree.ofList
                    |> NFA.RecognizeCharacterSet
                | LeftSquare | RightSquare ->
                    // Return an NFA that recognnizes the inverse of the atomic character set
                    // First, construct tree of elements to NOT include
                    let inverseTree =
                        List.map (fun (tree: OperatorParseTree<char, RegexOp>) ->
                            match tree.data with
                            | Atom a -> a
                            | Operation op -> invalidArg "regex" ("Operator `" + op.ToString () + "' must be escaped with a `\\' when in a character class")
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
                | QuestionMark ->
                    // Take the union of the single child NFA and the empty-string/epsilon/Accept NFA
                    NFA.RecognizeUnion childNFAs.Head NFA.Accept // TODO assert only one child NFA

        genMachineFromParseTree regexParseTree // Recursively build up an NFA from the parse tree

    let ofString (regex: string) : Regex =
        {
            text = regex
            machine = parseRegex regex;
        }
