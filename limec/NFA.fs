namespace limec

type State = int

type TransitionInput<'alphabet when 'alphabet: comparison> =
    | Epsilon
    | Alphabet of 'alphabet

type Transition<'alphabet when 'alphabet: comparison> =
    { input: TransitionInput<'alphabet> }

    override this.ToString () =
        match this.input with
        | Epsilon -> "{}"
        | Alphabet a -> a.ToString ()

type NFA<'alphabet when 'alphabet: comparison> =
    {
        stateCount: int;
        graph: Graph<State, Transition<Tree<'alphabet>>>;
        initial: State;
        final: Tree<State>;
    }

    // Adds all states reachable from the given
    // set of states via epsilon transitions
    member private this.addEpsilonStates (states: Tree<State>) : Tree<State> =
        Array.fold (
            fun allStates state ->
                // Adds all states reachable from the given state via epsilon transitions
                // Operates recursively; each newly reached state is then queried again for
                // epsilon transitions to additional states
                let rec addEpsilonAccessibleStates (state: State) (stateSet: Tree<State>) : Tree<State> =
                    let transitions : Edge<State, Transition<Tree<'alphabet>>> list = (this.graph.GetChildren state).Value.List
                    Seq.fold (
                        fun (stateSet: Tree<State>) (transition: Edge<State,Transition<Tree<'alphabet>>>) ->
                            match transition.label.input with
                            | Epsilon ->
                                // Check if destination state is new
                                match stateSet.Contains transition.dest with
                                | Some dest -> stateSet // Destination state is known, do nothing
                                | None ->
                                    // Add new destination state and recurse through it
                                    stateSet.Insert transition.dest
                                    |> addEpsilonAccessibleStates transition.dest
                            | _ -> stateSet // Do nothing
                    ) stateSet transitions

                addEpsilonAccessibleStates state allStates
        ) states states.Array

    // Check if any of the terminating states are accepting
    member private this.anyAccepting (states: Tree<State>) =
        Array.fold (
            fun anyAccepting (nextState: State) ->
                if anyAccepting then anyAccepting else (this.final.Contains nextState).IsSome
        ) false states.Array

    // Calculates the next set of possible states
    // from the current set and the next input character
    member private this.getNextStates (currentStates: Tree<State>) (nextInput: 'alphabet) =
        Array.fold (
            // Appends the possible next states reachable from the
            // given current state within the given next input character
            fun (nextStates: Tree<State>) (currentState: State) ->
                let transitions : Edge<State, Transition<Tree<'alphabet>>> list = (this.graph.GetChildren currentState).Value.List
                Seq.fold (
                    // Appends the destination of this possible transition
                    // if the character set assigned to it contains the next input character
                    fun (nextStates: Tree<State>) (possibleTransition: Edge<State, Transition<Tree<'alphabet>>>) ->
                        match possibleTransition.label.input with
                        | Alphabet alpha ->
                            match alpha.Contains nextInput with
                            | Some _ -> nextStates.Insert possibleTransition.dest // Add the state pointed to by this transition
                            | None -> nextStates // Add nothing
                        | Epsilon ->
                            nextStates // Ignore; assume it was followed and already added to currentStates
                ) nextStates transitions
        ) Tree.Empty currentStates.Array
        |> this.addEpsilonStates // Add immediately accessible states, in case they are accepting!

    member this.SimulateImmediateLongest (input: 'alphabet []) : 'alphabet list =
        let result, finalStates =
            Stack.takeWhileFold (
                fun (nfaStates: Tree<State>) (nextAlphabet: 'alphabet) ->
                    let nextStates = this.getNextStates nfaStates nextAlphabet
                    this.anyAccepting nextStates, nextStates
            ) (Tree.Empty.Insert this.initial) input
        result.List

    member this.SimulateEarlyLongest (input: 'alphabet []) : 'alphabet list =
        let firstHalf, firstMatchingStates =
            Stack.takeWhileFold (
                fun (nfaStates: Tree<State>) (nextAlphabet: 'alphabet) ->
                    let nextStates = this.getNextStates nfaStates nextAlphabet
                    not (this.anyAccepting nextStates), nextStates
            ) (Tree.Empty.Insert this.initial |> this.addEpsilonStates) input
        
        if firstHalf.Length = input.Length then
            []
        else
            let secondHalf, finalStates =
                Stack.takeWhileFold (
                    fun (nfaStates: Tree<State>) (nextAlphabet: 'alphabet) ->
                        let nextStates = this.getNextStates nfaStates nextAlphabet
                        this.anyAccepting nextStates, nextStates
                ) firstMatchingStates (Seq.skip firstHalf.Length input)
        
            firstHalf.List @ secondHalf.List

    member this.SimulateStartLongest (input: 'alphabet seq) : 'alphabet list =
        let longestMatch, finalList, endingStates =
            Seq.fold (
                fun (longestList: 'alphabet Stack, currentList: 'alphabet Stack, currentStates: Tree<State>) (nextAlphabet: 'alphabet) ->
                    let nextStates = this.getNextStates currentStates nextAlphabet
                    let nextList = currentList.Push nextAlphabet
                    let newLongestList = if this.anyAccepting nextStates then nextList else longestList
                    newLongestList, nextList, nextStates
            ) (Stack.Empty, Stack.Empty, Tree.Empty.Insert this.initial) input
        
        longestMatch.List

    member this.Simulate (input: 'alphabet seq) : bool =
        // Run the entire input sequence through the NFA and get the resulting possible states
        let finalStates =
            Seq.fold this.getNextStates (Tree.Empty.Insert this.initial |> this.addEpsilonStates) input
        this.anyAccepting finalStates

    member this.StateCount =
        this.stateCount

    member this.TransitionCount =
        this.graph.EdgeCount

module NFA =

    let Accept<'alphabet when 'alphabet: comparison> : NFA<'alphabet> =
        let initial = 0
        {
            stateCount = 1;
            graph = Graph.Empty.AddNode initial;
            initial = initial;
            final = Tree.Empty.Insert initial;
        }
    
    let Reject<'alphabet when 'alphabet: comparison> : NFA<'alphabet> =
        let initial = 0
        {
            stateCount = 1;
            graph = Graph.Empty.AddNode initial;
            initial = initial;
            final = Tree.Empty
        }

    let RecognizeCharacterSet (characterSet: Tree<'alphabet>) =
        let initial = 0
        let final = 1
        let g1 = Graph.Empty.AddNodes [ initial; final ]
        let graph = g1.AddEdge initial final { input = characterSet |> Alphabet }
        {
            stateCount = 2;
            graph = graph;
            initial = initial;
            final = Tree.Empty.Insert final;
        }

    let RecognizeCharacter (character: 'alphabet) =
        character |> List.singleton |> Tree.ofList |> RecognizeCharacterSet

    let CombineGraph (left: Graph<State, Transition<Tree<'alphabet>>>) (right: Graph<State, Transition<Tree<'alphabet>>>) =
        // Calculate offset to use with node ID's of right, to seamlessly merge node sets 
        let offset (node: State) = node + left.NodeCount

        // Add the nodes of right
        let graph =
            Seq.fold (
                fun (graph: Graph<State, _>) rightNode ->
                    graph.AddNode (offset rightNode)
            ) left right.NodeSet

        // Add the edges of right
        let graph =
            Seq.fold (
                fun (graph: Graph<_, Transition<Tree<'alphabet>>>) (rightEdge: Edge<_,_>) ->
                    graph.AddEdge (offset rightEdge.src) (offset rightEdge.dest) rightEdge.label
            ) graph right.EdgeSet

        graph // Return combination

    let CombineGraphs (graphs: Graph<State, Transition<Tree<'alphabet>>> seq) : Graph<State, Transition<Tree<'alphabet>>> =
        Seq.fold (
            fun combinedGraph nextGraph ->
                CombineGraph combinedGraph nextGraph
        ) Graph.Empty graphs // TODO: optimize by starting with the first graph itself

    let RecognizeConcatenation<'alphabet when 'alphabet: comparison> (left: NFA<'alphabet>) (right: NFA<'alphabet>) =
        // TODO: replace next three blocks with a call to CombineGraph(s)
        // Calculate offset to use with node ID's of right, to seamlessly merge node sets 
        let offset (node: State) = node + left.StateCount

        // Add the nodes of right
        let graph =
            Seq.fold (
                fun (graph: Graph<State, _>) rightNode ->
                    graph.AddNode (offset rightNode)
            ) left.graph right.graph.NodeSet

        // Add the edges of right
        let graph =
            Seq.fold (
                fun (graph: Graph<_, Transition<Tree<'alphabet>>>) (rightEdge: Edge<_,_>) ->
                    graph.AddEdge (offset rightEdge.src) (offset rightEdge.dest) rightEdge.label
            ) graph right.graph.EdgeSet

        // Patch final states of left to initial state of right
        let graph =
            Seq.fold (
                fun (graph: Graph<State, Transition<Tree<'alphabet>>>) leftFinalState ->
                    graph.AddEdge leftFinalState (offset right.initial) { input = Epsilon }
            ) graph left.final.List
        
        {
            stateCount = graph.NodeCount; // More optimally, left.nodeCount + right.nodeCount
            graph = graph;
            initial = left.initial; // Start from left NFA
            // Set new final states to final states of right, removing final states of left
            final = right.final.List |> Seq.fold (fun tree state -> tree.Insert (offset state)) Tree.Empty;
        }

    let RecognizeSequence (nfas: NFA<'alphabet> seq) : NFA<'alphabet> =
        Seq.fold (
            fun (sequenceNFA: NFA<'alphabet>) (nextNFA: NFA<'alphabet>) ->
                RecognizeConcatenation sequenceNFA nextNFA
        ) Accept nfas

    let RecognizeUnion (left: NFA<'alphabet>) (right: NFA<'alphabet>) : NFA<'alphabet> =
        let initialState = Reject

        // Combine NFAs
        let graph = CombineGraphs [ initialState.graph; left.graph; right.graph ]

        // Get states of left and right within the combined graph
        let leftInitial = left.initial + 1
        let rightInitial = right.initial + 1 + left.StateCount
        let leftFinal =
            Seq.fold (
                fun (newFinalStates: Tree<State>) (finalState: State) ->
                    newFinalStates.Insert (finalState + 1) // Offset by Reject state
            ) Tree.Empty left.final.List
        let leftRightFinal =
            Seq.fold (
                fun (newFinalStates: Tree<State>) (finalState: State) ->
                    newFinalStates.Insert (finalState + 1 + left.StateCount) // Offset by Reject and left's states
            ) leftFinal right.final.List

        // Allow initial state to map to either NFA
        let graph = graph.AddEdge initialState.initial leftInitial { input = Epsilon }
        let graph = graph.AddEdge initialState.initial rightInitial { input = Epsilon }

        // Return resulting NFA
        {
            stateCount = graph.NodeCount; // More optimally, 1 + left.StateCount + right.StateCount
            graph = graph;
            initial = initialState.initial;
            final = leftRightFinal; // Union of final states of left and right
        }

    let RecognizeUnions (nfas: NFA<'alphabet> seq) : NFA<'alphabet> =
        Seq.fold(
            fun (unionNFA: NFA<'alphabet>) (nextNFA: NFA<'alphabet>) ->
                RecognizeUnion unionNFA nextNFA
        ) Reject nfas // Reject union'd with anything is just that thing

    let RecognizeKleene (nfa: NFA<'alphabet>) : NFA<'alphabet> =
        // Make final states wrap around to initial
        let graph = nfa.final.List |> List.fold (fun (graph: Graph<State, Transition<Tree<'alphabet>>>) state -> graph.AddEdge state nfa.initial { input = Epsilon }) nfa.graph

        // Return resulting NFA
        {
            stateCount = graph.NodeCount; // More optimally, nfa.StateCount
            graph = graph;
            initial = nfa.initial;
            final = Tree.Empty.Insert nfa.initial; // Make the initial state the only acceping state, removing the old final states
        }
