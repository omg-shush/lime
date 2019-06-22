namespace limec

type Node<'node when 'node: comparison> =
    {
        value: 'node;
    }
    
    override this.ToString () =
        this.value.ToString ()

type Edge<'node, 'edge> =
    {
        src: 'node;
        dest: 'node;
        label: 'edge;
    }

    override this.ToString () =
        "(" + this.src.ToString () + " -> " + this.dest.ToString () + " via " + this.label.ToString () + ")"

type Graph<'node, 'edge when 'node: comparison> =
    | GraphAssociation of Association<Node<'node>, Stack<Edge<'node, 'edge>>>

    member private this.graph = match this with GraphAssociation g -> g

    override this.ToString () =
        this.graph.ToString ()

    member this.NodeCount =
        this.graph.Size

    member this.EdgeCount = 
        this.graph.ValueSet |> List.fold (fun edgeCount edgeStack -> edgeCount + edgeStack.Length) 0

    /// Returns a new Graph with the given node added
    member this.AddNode node =
        this.graph.Put { value = node } Stack.Empty |> GraphAssociation

    member this.AddNodes (nodes: 'node list) =
        nodes |> List.fold (fun (graph: Graph<'node, _>) node -> graph.AddNode node) this

    /// Returns a new Graph with the given edge added
    member this.AddEdge (src: 'node) (dest: 'node) (label: 'edge) =
        if (this.graph.ContainsKey { value = src } && this.graph.ContainsKey { value = dest }) then
            let oldEdges = this.graph.Get { value = src }
            match oldEdges with
            | Some oldEdges ->
                let newEdges = oldEdges.Push { src = src; dest = dest; label = label }
                this.graph.Put { value = src } newEdges
            | None -> invalidArg "src" "Source node does not exist"
        else
            invalidArg "src/dest" "Source/destination node does not exist"
        |> GraphAssociation

    member this.NodeSet =
        this.graph.KeySet
        |> Seq.map (fun node -> node.value)

    member this.EdgeSet =
        this.graph.ValueSet
        |> Seq.map (fun stackList -> stackList.List)
        |> Seq.concat

    member this.GetChildren node =
        this.graph.Get { value = node }

module Graph =

    let Empty<'node, 'edge when 'node: comparison> : Graph<'node, 'edge> = GraphAssociation Association.Empty