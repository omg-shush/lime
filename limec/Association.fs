namespace limec

[<CustomComparison; CustomEquality>]
type KeyValue<'K, 'V when 'K: comparison> =
    {
        key: 'K;
        value: 'V;
    }

    interface System.IComparable<KeyValue<'K, 'V>> with
        member this.CompareTo other =
            compare this.key other.key

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | null -> 1 // I'm more than nothing!
            | :? KeyValue<'K, 'V> as other -> (this :> System.IComparable<_>).CompareTo other
            | _ -> invalidArg "other" "Wrong object type"
            
    interface System.IEquatable<KeyValue<'K, 'V>> with
        member this.Equals other =
            this.key = other.key

    override this.Equals other =
        match other with
        | :? KeyValue<'K, 'V> as other -> (this :> System.IEquatable<_>).Equals other
        | _ -> false

    override this.GetHashCode () =
        hash this.key

    override this.ToString () =
        this.key.ToString () + " -> " + this.value.ToString ()

type Association<'K, 'V when 'K: comparison> =
    | AssociationTree of Tree<KeyValue<'K, 'V>>

    member private this.tree = match this with AssociationTree tree -> tree

    override this.ToString () =
        this.tree.Array
        |> Array.map (fun x -> x.ToString ())
        |> String.concat "\n"

    member this.Put key value =
        if (this.ContainsKey key) then
            this.tree.Replace { key = key; value = value } |> AssociationTree
        else
            this.tree.Insert { key = key; value = value } |> AssociationTree

    member this.Get key =
        let kvOption = this.tree.Contains { key = key; value = Unchecked.defaultof<'V> }
        match kvOption with
        | Some kv -> Some kv.value
        | None -> None

    member this.ContainsKey key =
        match this.Get key with
        | Some v -> true
        | None -> false

    /// Returns the number of key-value pairs in this Association
    member this.Size =
        this.tree.Size

    member this.Array =
        this.tree.Array

    member this.List =
        this.tree.List

    member this.KeyValueSet: KeyValue<'K, 'V> list =
        this.tree.List

    member this.KeySet =
        this.KeyValueSet |> List.map (fun keyvalue -> keyvalue.key)

    member this.ValueSet = 
        this.KeyValueSet |> List.map (fun keyvalue -> keyvalue.value)

module Association =

    let Empty<'K, 'V when 'K: comparison> = AssociationTree Tree.Empty<KeyValue<'K, 'V>>