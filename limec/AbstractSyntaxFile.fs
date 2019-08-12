namespace limec

open System

module AbstractSyntaxFile =

    let AST_HEADER = 0xb0000000 |> BitConverter.GetBytes

    let Read (parameters: Parameters) : AbstractSyntaxTree =
        ()

    let Write (parameters: Parameters) (program: AbstractSyntaxTree) =
        let rec writeAst (AbstractSyntaxTree (cp, vars, LlamaExpression code): AbstractSyntaxTree) : byte seq =

            // Converts int to four-byte array
            let writeInt (i: int) =
                BitConverter.GetBytes i |> Seq.ofArray

            let writeInt64 (i: int64) =
                BitConverter.GetBytes i |> Seq.ofArray

            let writeDouble (d: double) =
                BitConverter.GetBytes d |> Seq.ofArray

            let writeBool (b: bool) =
                BitConverter.GetBytes b |> Seq.ofArray

            let writeChar (c: char) =
                BitConverter.GetBytes c |> Seq.ofArray

            // Convertes string to (a) four-byte integer length, followed by (b) array of Unicode characters
            let writeString (s: string) =
                Seq.append (writeInt s.Length) (s.ToCharArray () |> Array.collect BitConverter.GetBytes)

            let writeCodePosition (cp: CodePosition) =
                Seq.concat [ writeString cp.file; writeInt cp.line; writeInt cp.character ]

            let writeLlamaLiteral (ll: LlamaLiteral) =
                match ll with
                | LlamaString s -> Seq.append (Seq.singleton 1uy) (writeString s)
                | LlamaChar c -> Seq.append (Seq.singleton 2uy) (writeChar c)
                | LlamaInt i -> Seq.append (Seq.singleton 3uy) (writeInt64 i)
                | LlamaDouble d -> Seq.append (Seq.singleton 4uy) (writeDouble d)
                | LlamaBool b -> Seq.append (Seq.singleton 5uy) (writeBool b)

            let writeLlamaIdentifier (li: LlamaIdentifier) =
                match li with
                | LlamaName name -> Seq.append (Seq.singleton Byte.MaxValue) (writeString name)
                | LlamaOperator op -> Seq.append (Seq.singleton Byte.MinValue) (writeString op)

            let writeSeq (elementWriter: 'a -> byte seq) (arr: 'a seq) =
                Seq.append (writeInt (Seq.length arr)) (Seq.collect elementWriter arr)

            let writeLlama ({ typ = t; def = d }: Llama) =
                Seq.append (writeSeq writeLlamaIdentifier t) (writeAst d)

            let writeKeyValue (keyWriter: 'k -> byte seq) (valueWriter: 'v -> byte seq) ({ key = k; value = v }: KeyValue<'k, 'v>) =
                Seq.append (keyWriter k) (valueWriter v)

            let rec writeOperatorParseTree (atomWriter: 'a -> byte seq) (operatorWriter: 'b -> byte seq) ({ data = d; children = c }: OperatorParseTree<'a, 'b>) =
                let dataBytes =
                    match d with
                    | Atom a -> Seq.append (Seq.singleton Byte.MaxValue) (atomWriter a)
                    | Operation o -> Seq.append (Seq.singleton Byte.MinValue) (operatorWriter o)
                Seq.append dataBytes (writeSeq (writeOperatorParseTree atomWriter operatorWriter) c)

            let cpBytes = writeCodePosition cp
            let varBytes = writeSeq (writeKeyValue writeLlamaIdentifier writeLlama) vars.Array
            let codeBytes = writeOperatorParseTree writeLlamaLiteral writeLlamaIdentifier code

            Seq.concat [ AST_HEADER; writeInt (Seq.length cpBytes); writeInt (Seq.length varBytes); writeInt (Seq.length codeBytes); cpBytes; varBytes; codeBytes ]

        let bytes = writeAst program |> Array.ofSeq
        IO.File.WriteAllBytes (parameters.output, bytes)
        printfn "Wrote %d bytes to %s" bytes.Length parameters.output
