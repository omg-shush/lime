namespace limec

type TreeElement<'T when 'T: comparison> = 
    {
        data: 'T;
        left: Tree<'T>;
        right: Tree<'T>;
        size: int;
    }

    // Balances the subtree rooted at this TreeElement to maintain
    // the AVL balanced binary tree invariant
    member tree.Rebalance : TreeElement<'T> =
        let sizeDiff = tree.left.Height - tree.right.Height
        if (sizeDiff > 1) then
            // rotate right
            match tree.left with
            | EmptyTree -> invalidArg "this" "Invalid (sub)tree"
            | Element oldLeft ->
                {
                    oldLeft with
                        right = { tree with left = oldLeft.right; size = tree.size - oldLeft.size + oldLeft.right.Size } |> Element;
                        size = oldLeft.size + 1 + tree.right.Size;
                }
        elif (sizeDiff < -1) then
            // rotate left
            match tree.right with
            | EmptyTree -> invalidArg "this" "Invalid (sub)tree"
            | Element oldRight ->
                {
                    oldRight with
                        left = { tree with right = oldRight.left; size = tree.size - oldRight.size + oldRight.left.Size } |> Element;
                        size = oldRight.size + 1 + tree.left.Size;
                }
        else
            tree // no balancing needed

and Tree<'T when 'T: comparison> =
    | EmptyTree
    | Element of TreeElement<'T>

    override this.ToString () =
        this.Array
        |> Array.map (fun x -> x.ToString ())
        |> String.concat ", "

    member this.Size : int =
        match this with
        | EmptyTree -> 0
        | Element e -> e.size

    member this.Height : int =
        if this.Size = 0 then 0
        else
            let size = this.Size |> double
            let height = (log size) / (log 2.0)
            ((floor height) |> int) + 1

    member this.Insert (data: 'T) : Tree<'T> =
        // Insert new element
        let newElement =
            match this with
            | EmptyTree -> { data = data; left = EmptyTree; right = EmptyTree; size = 1 }
            | Element e ->
                match compare data e.data |> sign with
                | +1 -> { e with right = e.right.Insert data; size = e.size + 1 }
                | -1 -> { e with left = e.left.Insert data; size = e.size + 1 }
                |  _ -> invalidArg "data" "Given data already exists in the tree"

        // Return balanced tree
        newElement.Rebalance |> Element

    member this.Replace (data: 'T) : Tree<'T> =
        // Replace existing element
        let newElement =
            match this with
            | EmptyTree -> invalidArg "data" "Given data didn't already exist in the tree"
            | Element e ->
                match compare data e.data |> sign with
                | +1 -> { e with right = e.right.Replace data; size = e.size }
                | -1 -> { e with left = e.left.Replace data; size = e.size }
                |  _ -> { e with data = data } // Overwrite existing element

        // No need to rebalance
        newElement |> Element

    member this.Contains (data: 'T) : 'T option =
        match this with
        | EmptyTree -> None
        | Element e ->
            match compare data e.data |> sign with
            | +1 -> e.right.Contains data
            | -1 -> e.left.Contains data
            |  _ -> Some e.data

    member this.toArray (arr: 'T[]) pos =
        match this with
        | EmptyTree -> ()
        | Element e ->
            e.left.toArray arr pos
            arr.[pos + e.left.Size] <- e.data
            e.right.toArray arr (pos + e.left.Size + 1)
    
    member this.Array: 'T[] =
        match this with
        | EmptyTree -> [||]
        | Element e ->
            let arr = Array.zeroCreate e.size
            this.toArray arr 0
            arr

    // Generates an ordered list of elements in this Tree via in-order traversal
    member private this.toList (list: 'T list) : 'T list =
        match this with
        | EmptyTree -> list
        | Element e -> (e.data :: (list |> e.right.toList)) |> e.left.toList

    member this.List : 'T list =
        this.toList List.empty

    member this.Elements: 'T seq =
        Seq.ofList this.List

module Tree =

    let Empty<'T when 'T: comparison> = Tree<'T>.EmptyTree

    /// Constructs a new Tree containing the elements of list
    let ofList (list: 'T list) : Tree<'T> =
        List.fold (fun tree data -> tree.Insert data) Empty list