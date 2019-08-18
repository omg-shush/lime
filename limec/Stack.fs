namespace limec

/// Similar to list, but iterates from bottom to top instead of from front to back
type Stack<'T> =
    | EmptyStack
    | Cons of 'T Stack * 'T

    override this.ToString () =
        this.Array
        |> Array.map (fun x -> sprintf "%A" x)
        |> String.concat "\n|> "

    member this.ToStringRec () =
        this.Array
        |> Array.map (fun x -> x.ToString ())
        |> String.concat "\n|> "

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

    member this.Append other =
        match other with
        | Cons (bottom, top) -> Cons ((this.Append bottom), top)
        | EmptyStack -> this

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
    let isEmpty (s: Stack<'T>) = match s with EmptyStack -> true | Cons _ -> false
    let makeList (stack: Stack<'T>) : List<'T> = stack.List
    let makeArray (stack: Stack<'T>) : 'T [] = stack.Array
    let ofList (input: List<'T>) : Stack<'T> =
        let rec ofList (stack: Stack<'T>) (append: List<'T>) =
            match append with
            | head :: tail -> ofList (Cons (stack, head)) tail
            | _ -> stack
        ofList Empty input
    let ofArray (input: 'T []) : Stack<'T> =
        Array.fold (fun stack e -> Cons (stack, e)) Empty input

    let rec fold (folder: 'State -> 'T -> 'State) (init: 'State) (stack: Stack<'T>) : 'State =
        match stack with
        | EmptyStack -> init
        | Cons (bottom, top) -> folder (fold folder init bottom) top

    let takeWhileFold (foldPredicate: 'State -> 'T -> bool * 'State) (init: 'State) (source: 'T seq) : Stack<'T> * 'State =
        let mutable input = source
        let mutable state = init
        let mutable result = Empty
        let mutable breakNow = true
        while not (Seq.isEmpty input) && breakNow do
            let nextInput = Seq.head input
            let predicateResult, nextState = foldPredicate state nextInput
            if predicateResult then
                state <- nextState
                input <- Seq.tail input
                result <- result.Push nextInput
            else
                breakNow <- false
        result, state

    let takeWhileFoldOld (foldPredicate: 'State -> 'T -> bool * 'State) (init: 'State) (source: 'T seq) : Stack<'T> * 'State =
        let rec takeWhileFold' (foldPredicate: 'State -> 'T -> bool * 'State, currentState: 'State, source: 'T seq, acc: Stack<'T>) : Stack<'T> * 'State =
            if Seq.isEmpty source then
                acc, currentState
            else
                let value, nextState = foldPredicate currentState (Seq.head source)
                if value then
                    takeWhileFold' (foldPredicate, nextState, Seq.tail source, acc.Push (Seq.head source))
                else
                    acc, currentState
        takeWhileFold' (foldPredicate, init, source, Empty)

    let rec skipWhileFold (foldPredicate: 'State -> 'T -> bool * 'State) (init: 'State) (source: 'T seq) : 'T seq * 'State =
        if Seq.isEmpty source then
            Seq.empty, init
        else
            let value, nextState = foldPredicate init (Seq.head source)
            if value then
                skipWhileFold foldPredicate nextState (Seq.tail source)
            else
                source, nextState