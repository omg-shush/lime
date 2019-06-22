namespace limec

type Stack<'T> =
    | EmptyStack
    | Cons of 'T Stack * 'T

    override this.ToString () =
        this.Array
        |> Array.map (fun x -> x.ToString ())
        |> String.concat " | "

    member this.Bottom : 'T Stack =
        match this with
        | EmptyStack -> invalidArg "stack" "Stack is empty"
        | Cons (bottom, _) -> bottom

    member this.Top : 'T =
        match this with
        | EmptyStack -> invalidArg "stack" "Stack is empty"
        | Cons (_, top) -> top

    member this.Push pushMe : 'T Stack =
        Cons (this, pushMe)

    member this.Pop : 'T * 'T Stack =
        match this with
        | EmptyStack -> invalidArg "stack" "Can't pop an empty stack"
        | Cons (bottom, top) -> (top, bottom)

    member this.Length : int =
        match this with
        | EmptyStack -> 0
        | Cons (bottom, _) -> bottom.Length + 1

    member private this.toArray (arr: 'T[]) pos =
        match this with
        | EmptyStack -> ()
        | Cons (bottom, top) ->
            arr.[pos] <- top
            bottom.toArray arr (pos - 1)

    member this.Array : 'T[] =
        let len = this.Length
        let arr = Array.zeroCreate len
        this.toArray arr (len - 1)
        arr

    member private this.toList list : 'T list =
        match this with
        | EmptyStack -> list
        | Cons (bottom, top) -> bottom.toList (List.append (List.singleton top) list)

    member this.List =
        this.toList List.empty

    member private this.getItem i : Choice<int, 'T> =
        match this with
        | EmptyStack -> Choice1Of2 0
        | Cons (bottom, top) ->
            match bottom.getItem i with
            | Choice1Of2 pos ->
                if (pos = i) then
                    Choice2Of2 top
                else
                    Choice1Of2 (pos + 1)
            | Choice2Of2 item -> Choice2Of2 item

    member this.Item i : 'T =
        match this.getItem i with
        | Choice1Of2 pos -> invalidArg "index" (sprintf "Invalid index %d for Stack of length %d" i pos)
        | Choice2Of2 item -> item

module Stack =

    let Empty<'T> = Stack<'T>.EmptyStack
    let makeList (stack: Stack<'T>) : List<'T> = stack.List
    let rec ofList (input: List<'T>) : Stack<'T> =
        let rec ofList (stack: Stack<'T>) (append: List<'T>) =
            match append with
            | head :: tail -> ofList (Cons (stack, head)) tail
            | _ -> stack
        ofList Empty input