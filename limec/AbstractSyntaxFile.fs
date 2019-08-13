namespace limec

open System

module AbstractSyntaxFile =

    let AST_HEADER = 0xb0000000 |> BitConverter.GetBytes

    let Read (parameters: Parameters) : AbstractSyntaxTree =
        let rec readAst (bs: byte list) : AbstractSyntaxTree * byte list =
            let readInt (bs: byte list) =
                let bytes = bs |> List.take 4 |> Array.ofList
                BitConverter.ToInt32 (bytes, 0), List.skip 4 bs

            let readInt64 (bs: byte list) =
                let bytes = bs |> List.take 8 |> Array.ofList
                BitConverter.ToInt64 (bytes, 0), List.skip 8 bs

            let readDouble (bs: byte list) =
                let bytes = bs |> List.take 8 |> Array.ofList
                BitConverter.ToDouble (bytes, 0), List.skip 8 bs

            let readBool (bs: byte list) =
                let bytes = bs |> List.take 1 |> Array.ofList
                BitConverter.ToBoolean (bytes, 0), List.skip 1 bs

            let readChar (bs: byte list) =
                let bytes = bs |> List.take 2 |> Array.ofList
                BitConverter.ToChar (bytes, 0), List.skip 2 bs

            let readString (bs: byte list) =
                let length, bs = readInt bs
                let bytes, bs = Array.zeroCreate length |> Array.mapFold (fun (bs: byte list) _ -> readChar bs) bs
                new string (bytes), bs

            let readCodePosition (bs: byte list) =
                let file, bs = readString bs
                let line, bs = readInt bs
                let character, bs = readInt bs
                { CodePosition.file = file; line = line; character = character }, bs

            let readLlamaLiteral (bs: byte list) =
                let literalType, bs = List.head bs, List.tail bs
                match literalType with
                | 1uy ->
                    let s, bs = readString bs
                    LlamaString s, bs
                | 2uy ->
                    let c, bs = readChar bs
                    LlamaChar c, bs
                | 3uy ->
                    let i, bs = readInt64 bs
                    LlamaInt i, bs
                | 4uy ->
                    let d, bs = readDouble bs
                    LlamaDouble d, bs
                | 5uy ->
                    let b, bs = readBool bs
                    LlamaBool b, bs
                | _ -> invalidArg parameters.input "Invalid file"

            let readLlamaIdentifier (bs: byte list) =
                let identifierType, bs = List.head bs, List.tail bs
                let identifierCtor =
                    match identifierType with
                    | Byte.MaxValue -> LlamaName
                    | Byte.MinValue -> LlamaOperator
                    | _ -> invalidArg parameters.input "Invalid file"
                let s, bs = readString bs
                identifierCtor s, bs

            let readList (elementReader: byte list -> 'a * byte list) (bs: byte list) =
                let length, bs = readInt bs
                let arr, bs = Array.zeroCreate length |> Array.mapFold (fun bs _ -> elementReader bs) bs
                List.ofArray arr, bs

            let readLlama (bs: byte list) =
                let typ, bs = readList readLlamaIdentifier bs
                let def, bs = readAst bs
                { Llama.typ = typ; def = def }, bs

            let readKeyValue (keyReader: byte list -> 'k * byte list) (valueReader: byte list -> 'v * byte list) (bs: byte list) =
                let key, bs = keyReader bs
                let value, bs = valueReader bs
                { KeyValue.key = key; value = value }, bs

            let rec readOperatorParseTree (atomReader: byte list -> 'a * byte list) (operatorReader: byte list -> 'b * byte list) (bs: byte list) =
                let dataType, bs = List.head bs, List.tail bs
                let data, bs =
                    match dataType with
                    | Byte.MaxValue ->
                        let atom, bs = atomReader bs
                        Atom atom, bs
                    | Byte.MinValue ->
                        let operator, bs = operatorReader bs
                        Operation operator, bs
                    | _ -> invalidArg parameters.input "Invalid file"
                let children, bs = readList (readOperatorParseTree atomReader operatorReader) bs
                { OperatorParseTree.data = data; children = children }, bs

            let cpLength, bs = readInt bs
            let varLength, bs = readInt bs
            let codeLength, bs = readInt bs
            let cp, bs = readCodePosition bs
            let var, bs = readList (readKeyValue readLlamaIdentifier readLlama) bs |> (fun (vararr, bs) -> Association.Empty.InsertAll vararr, bs)
            let code, bs = readOperatorParseTree readLlamaLiteral readLlamaIdentifier bs |> (fun (codeTree, bs) -> LlamaExpression codeTree, bs)

            AbstractSyntaxTree (cp, var, code), bs

        let sw = System.Diagnostics.Stopwatch.StartNew ()
        let file = parameters.input |> IO.File.ReadAllBytes |> List.ofArray
        let header, bytes = List.take AST_HEADER.Length file, List.skip AST_HEADER.Length file
        if Array.ofSeq header = AST_HEADER then
            let ast, remaining = readAst bytes
            if Seq.isEmpty remaining then
                sw.Stop ()
                printfn "Read %d bytes from %s in %f ms" (Seq.length bytes + AST_HEADER.Length) parameters.input sw.Elapsed.TotalMilliseconds
                ast
            else
                invalidArg parameters.input "Invalid file"
        else
            invalidArg parameters.input "Invalid file"

    let Write (parameters: Parameters) (program: AbstractSyntaxTree) =
        let rec writeAst (AbstractSyntaxTree (cp, vars, LlamaExpression code): AbstractSyntaxTree) : byte list =

            // Converts int to four-byte array
            let writeInt (i: int) =
                BitConverter.GetBytes i |> List.ofArray

            let writeInt64 (i: int64) =
                BitConverter.GetBytes i |> List.ofArray

            let writeDouble (d: double) =
                BitConverter.GetBytes d |> List.ofArray

            let writeBool (b: bool) =
                BitConverter.GetBytes b |> List.ofArray

            let writeChar (c: char) =
                BitConverter.GetBytes c |> List.ofArray

            // Convertes string to (a) four-byte integer length, followed by (b) array of Unicode characters
            let writeString (s: string) =
                List.append (writeInt s.Length) (s.ToCharArray () |> Array.collect BitConverter.GetBytes |> List.ofArray)

            let writeCodePosition (cp: CodePosition) =
                List.concat [ writeString cp.file; writeInt cp.line; writeInt cp.character ]

            let writeLlamaLiteral (ll: LlamaLiteral) =
                match ll with
                | LlamaString s -> List.append (List.singleton 1uy) (writeString s)
                | LlamaChar c -> List.append (List.singleton 2uy) (writeChar c)
                | LlamaInt i -> List.append (List.singleton 3uy) (writeInt64 i)
                | LlamaDouble d -> List.append (List.singleton 4uy) (writeDouble d)
                | LlamaBool b -> List.append (List.singleton 5uy) (writeBool b)

            let writeLlamaIdentifier (li: LlamaIdentifier) =
                match li with
                | LlamaName name -> List.append (List.singleton Byte.MaxValue) (writeString name)
                | LlamaOperator op -> List.append (List.singleton Byte.MinValue) (writeString op)

            let writeList (elementWriter: 'a -> byte list) (arr: 'a list) =
                List.append (writeInt (List.length arr)) (List.collect elementWriter arr)

            let writeLlama ({ typ = t; def = d }: Llama) =
                List.append (writeList writeLlamaIdentifier t) (writeAst d)

            let writeKeyValue (keyWriter: 'k -> byte list) (valueWriter: 'v -> byte list) ({ key = k; value = v }: KeyValue<'k, 'v>) =
                List.append (keyWriter k) (valueWriter v)

            let rec writeOperatorParseTree (atomWriter: 'a -> byte list) (operatorWriter: 'b -> byte list) ({ data = d; children = c }: OperatorParseTree<'a, 'b>) =
                let dataBytes =
                    match d with
                    | Atom a -> List.append (List.singleton Byte.MaxValue) (atomWriter a)
                    | Operation o -> List.append (List.singleton Byte.MinValue) (operatorWriter o)
                List.append dataBytes (writeList (writeOperatorParseTree atomWriter operatorWriter) c)

            let cpBytes = writeCodePosition cp
            let varBytes = writeList (writeKeyValue writeLlamaIdentifier writeLlama) vars.List
            let codeBytes = writeOperatorParseTree writeLlamaLiteral writeLlamaIdentifier code

            List.concat [ writeInt (List.length cpBytes); writeInt (List.length varBytes); writeInt (List.length codeBytes); cpBytes; varBytes; codeBytes ]


        let sw = System.Diagnostics.Stopwatch.StartNew ()
        let bytes = writeAst program |> Array.ofList |> Array.append AST_HEADER
        IO.File.WriteAllBytes (parameters.output, bytes)
        sw.Stop ()
        printfn "Wrote %d bytes to %s in %f ms" bytes.Length parameters.output sw.Elapsed.TotalMilliseconds
