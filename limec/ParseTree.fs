namespace limec

// TODO implement right-associative operators!

/// Defines whether an operation acts as a prefix (as in !value),
/// postfix (as in value*), infix (as in value + value), circumfix (as in [value]), or circumliteral (as in <literal-value>)
type OperationType = Prefix | Postfix | Infix | Circumfix | Circumliteral

/// Associates an operation with its type
type Operation<'operation when 'operation: equality> =
    | Prefix of 'operation
    | Postfix of 'operation
    | Infix of 'operation
    | Circumfix of 'operation * 'operation
    | Circumliteral of 'operation * 'operation
    | Adjacent of 'operation

    member this.GetOp =
        match this with
        | Circumfix _ -> Unchecked.defaultof<'operation>
        | Circumliteral _ -> Unchecked.defaultof<'operation>
        | Prefix op | Postfix op | Infix op | Adjacent op -> op

/// Carries the data associated with a single parse tree node;
/// either an atom within the alphabet, or an operation over the alphabet
type ParseData<'alphabet, 'operation when 'operation: equality> =
    | Atom of 'alphabet
    | Operation of 'operation

/// Represents a node in a parse tree, containing both its own data
/// and a list of its child nodes
type ParseTree<'alphabet, 'operation when 'operation: equality> =
    {
        data: ParseData<'alphabet, 'operation>
        children: ParseTree<'alphabet, 'operation> list
    }

    member private this.toString (indent: string) =
        indent
        + this.data.ToString ()
        + "\n"
        + (Seq.fold (fun str (child: ParseTree<_, _>) -> str + indent + child.toString (indent + "    ")) "" this.children)

    override this.ToString () =
        this.toString ""

module ParseTree =

    let rec private parseAtomized (opPriority: Operation<'operation> list) (atomizedInput: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
        // Run through operations in order of priority
        // and parse each instance of it in the (initially flat) list of ParseTrees
        List.fold (
            fun (workingInput: ParseTree<'alphabet, 'operation> list) (nextOperation: Operation<'operation>) ->
                // Iterate over all subtrees in the working list
                // If any match the given operation, then substitute a new tree with that operation
                // with the adjacent subtrees being operated upon as children
                match nextOperation with
                | Prefix nextOperation ->
                    // Recursively reads through the workingList and substitutes each instance
                    // of nextOperation with the appropriate prefix subtree
                    let rec parseListForPrefixOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 2) then
                            workingList // Too short to contain another top level infix operator
                        else
                            match workingList.[0].data with
                            | Atom a -> workingList.Head :: (parseListForPrefixOperation workingList.Tail) // Not an operation, recurse on rest of list
                            | Operation op ->
                                if (op = nextOperation) then
                                    // Construct subtree with this operation at the root
                                    let parsedSubtree =
                                        {
                                            // Keep same operation data
                                            data = workingList.[0].data;
                                            // Add right subtree as child of this operation
                                            children = List.singleton workingList.[1]
                                        }
                                    // Append new subtree to parsed version of rest of list
                                    parsedSubtree :: (parseListForPrefixOperation workingList.Tail.Tail)
                                else
                                    workingList.Head :: (parseListForPrefixOperation workingList.Tail) // Wrong operation, recurse on rest of list

                    // Run through workingInput, looking for instances of nextOperation
                    parseListForPrefixOperation workingInput

                | Postfix nextOperation ->
                    // Recursively reads through the workingList and substitutes each instance
                    // of nextOperation with the appropriate postfix subtree
                    let rec parseListForPostfixOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 2) then
                            workingList // Too short to contain another top level infix operator
                        else
                            match workingList.[1].data with
                            | Atom a -> workingList.Head :: (parseListForPostfixOperation workingList.Tail) // Not an operation, recurse on rest of list
                            | Operation op ->
                                if (op = nextOperation) then
                                    // Construct subtree with this operation at the root
                                    let parsedSubtree =
                                        {
                                            // Keep same operation data
                                            data = workingList.[1].data;
                                            // Add left subtree as child of this operation
                                            children = List.singleton workingList.[0]
                                        }
                                    // Append new subtree to parsed version of rest of list
                                    parsedSubtree :: (parseListForPostfixOperation workingList.Tail.Tail)
                                else
                                    workingList.Head :: (parseListForPostfixOperation workingList.Tail) // Wrong operation, recurse on rest of list

                    // Run through workingInput, looking for instances of nextOperation
                    parseListForPostfixOperation workingInput

                | Infix nextOperation ->
                    // Recursively reads through the workingList and substitutes each instance
                    // of nextOperation with the appropriate infix subtree
                    let rec parseListForInfixOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 3) then
                            workingList // Too short to contain another top level infix operator
                        else
                            match workingList.[1].data with
                            | Atom a -> workingList.Head :: (parseListForInfixOperation workingList.Tail) // Not an operation, recurse on rest of list
                            | Operation op ->
                                if (op = nextOperation) then
                                    // Construct subtree with this operation at the root
                                    let parsedSubtree =
                                        {
                                            // Keep same operation data
                                            data = workingList.[1].data;
                                            // Add left and right subtrees as children of this operation
                                            children = workingList.[0] :: (List.singleton workingList.[2])
                                        }
                                    // Append new subtree to parsed version of rest of list
                                    parsedSubtree :: (parseListForInfixOperation workingList.Tail.Tail.Tail)
                                else
                                    workingList.Head :: (parseListForInfixOperation workingList.Tail) // Wrong operation, recurse on rest of list

                    // Run through workingInput, looking for instances of nextOperation
                    parseListForInfixOperation workingInput

                | Circumfix (nextOperationLeft, nextOperationRight) ->
                    // Recursively reads through the workingList and substitutes each instance
                    // of nextOperationLeft and nextOperatonRight surrounding a sublist with the appropriate circumfix subtree
                    let rec parseListForCircumfixOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 1) then
                            workingList // Too short to contain another top level circumfix operator part
                        else
                            match workingList.[0].data with
                            | Atom a -> workingList.Head :: (parseListForCircumfixOperation workingList.Tail) // Not an operation, recurse on rest of list
                            | Operation op ->
                                if (op = nextOperationLeft) then
                                    // Opening parenthesis!
                                    // Construct subtree with this operation at the root and interior of parentheses
                                    // (whatever that might parse into) as the child

                                    // Get the interior of the circumfix operator
                                    let interiorOfCircumfix = parseListForCircumfixOperation workingList.Tail
                                    // Parse that interior into a united parse tree
                                    let interiorOfCircumfixParseTree = (parseAtomized opPriority interiorOfCircumfix).Head // TODO assert returned tree list is singleton
                                    // Replace self with parsed interior and append to parsed version of rest of list (skipping interior, +2 for left and right brackets)
                                    interiorOfCircumfixParseTree :: (parseListForCircumfixOperation (List.skip (interiorOfCircumfix.Length + 2) workingList))
                                else if (op = nextOperationRight) then
                                    // Closing parenthesis!
                                    // Assume was recursively called after finding an opening parenthesis,
                                    // so we can stop parsing and return interior of the circumfix operator
                                    List.empty
                                else
                                    workingList.Head :: (parseListForCircumfixOperation workingList.Tail) // Wrong operation, recurse on rest of list

                    // Run through workingInput, looking for instances of nextOperationLeft
                    parseListForCircumfixOperation workingInput

                | Circumliteral (nextOperationLeft, nextOperationRight) ->
                    // Recursively reads through the workingList and substitutes each instance
                    // of nextOperationLeft and nextOperatonRight surrounding a sublist and inherits that sublist raw
                    let rec parseListForCircumliteralOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 1) then
                            workingList // Too short to contain another top level circumfix operator part
                        else
                            match workingList.[0].data with
                            | Atom a -> workingList.Head :: (parseListForCircumliteralOperation workingList.Tail) // Not an operation, recurse on rest of list
                            | Operation op ->
                                if (op = nextOperationLeft) then
                                    // Opening parenthesis!
                                    // Construct subtree with this operation at the root and interior of parentheses
                                    // (whatever that might parse into) as the child

                                    // Get the interior of the circumfix operator
                                    let interiorOfCircumliteral = parseListForCircumliteralOperation workingList.Tail

                                    // Add it as self's children
                                    let circumliteralWithInteriorInherited = { workingList.[0] with children = interiorOfCircumliteral; }

                                    // Inherit unparsed interior and append to parsed version of rest of list (skipping interior, +2 for left and right brackets)
                                    circumliteralWithInteriorInherited :: (parseListForCircumliteralOperation (List.skip (interiorOfCircumliteral.Length + 2) workingList))
                                else if (op = nextOperationRight) then
                                    // Closing parenthesis!
                                    // Assume was recursively called after finding an opening parenthesis,
                                    // so we can stop looking and return interior of the circumfix operator
                                    List.empty
                                else
                                    workingList.Head :: (parseListForCircumliteralOperation workingList.Tail) // Wrong operation, recurse on rest of list

                    // Run through workingInput, looking for instances of nextOperationLeft
                    parseListForCircumliteralOperation workingInput

                | Adjacent nextOperation ->
                    // Recursively reads through the workingList and substitutes each instance of two adjacent complete subtrees,
                    // where complete means either atomic or operational but already parsed, with the correct operation
                    // as if it were inserted between them
                    let rec parseListForAdjacentOperation (workingList: ParseTree<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> list =
                        if (workingList.Length < 2) then
                            workingList // Too short to contain an adjacent pair of complete subtrees
                        else
                            let isCompleteSubtree (subtree: ParseTree<'alphabet, 'operation>) : bool =
                                match subtree.data with
                                | Atom _ -> true // Atoms cannot be further parsed
                                | Operation op ->
                                    // Complete iff it has children
                                    not subtree.children.IsEmpty
                                    (*// Complete iff we've already parsed this operation, so it must be complete by now
                                    let toBeParsedOps = List.skip (List.findIndex (fun e -> e = Adjacent nextOperation) opPriority) opPriority
                                    not (List.contains op (List.map (fun (opWithType: Operation<'operation>) -> opWithType.GetOp) toBeParsedOps))*)
                            if (isCompleteSubtree workingList.[0] && isCompleteSubtree workingList.[1]) then
                                // Construct subtree with this operation at the root
                                let parsedSubtree =
                                    {
                                        // Insert adjacency operation data
                                        data = Operation nextOperation;
                                        // Add left and right subtrees as children of this operation
                                        children = workingList.[0] :: (List.singleton workingList.[1])
                                    }
                                // Append new subtree to rest of list and re-parse the entire thing
                                parseListForAdjacentOperation (parsedSubtree :: workingList.Tail.Tail)
                            else
                                workingList.Head :: (parseListForAdjacentOperation workingList.Tail) // Not a valid adjacency, recurse on rest of list

                    // Run through workingInput, looking for instances of valid adjacencies
                    parseListForAdjacentOperation workingInput

        ) atomizedInput opPriority

    let Parse (opPriority: Operation<'operation> list) (input: Choice<'alphabet, 'operation> list) : ParseTree<'alphabet, 'operation> =
        // Convert entire input into a list of ParseTree
        let atomizedInput =
            input
            |> List.map (
                fun inputItem ->
                    match inputItem with
                    | Choice1Of2 atom -> { data = atom |> Atom; children = List.empty }
                    | Choice2Of2 op -> { data = op |> Operation; children = List.empty }
            )

        // Parse that list of atoms, operation by operation
        let parsedInput = parseAtomized opPriority atomizedInput

        // Assert that, having parsed all operations, there is only one root operation
        if (parsedInput.Length <> 1) then
            invalidArg "input" "Given input does not parse into a single tree"
        else
            parsedInput.Head // Return the root node

    let AddPrefixOperation op opList =
        Prefix op :: opList

    let AddPostfixOperation op opList =
        Postfix op :: opList

    let AddInfixOperation op opList =
        Infix op :: opList

    let AddCircumfixOperation opLeft opRight opList =
        Circumfix (opLeft, opRight) :: opList

    let AddAdjacentOperation op opList =
        Adjacent op :: opList
