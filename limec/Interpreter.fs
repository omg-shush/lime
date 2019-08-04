namespace limec

module Interpreter =

    type ValueType =
        | UnitType | StringType | CharType | IntType | DoubleType | BoolType | FunType of ValueType * ValueType

    type Value =
        | Unit
        | ValueString of string
        | ValueChar of char
        | ValueInt of int64
        | ValueDouble of double
        | ValueBool of bool
        | ValueFun of ValueType * ValueType * AbstractSyntaxTree

        member this.Type =
            match this with
            | Unit -> UnitType
            | ValueString _ -> StringType
            | ValueChar _ -> CharType
            | ValueInt _ -> IntType
            | ValueDouble _ -> DoubleType
            | ValueBool _ -> BoolType
            | ValueFun (i, o, _) -> FunType (i, o)

    type IdentifierValue =
        | Initialized of Value
        | Uninitialized of Llama

    type Environment = Association<LlamaIdentifier, IdentifierValue>

    let Interpret (ast: AbstractSyntaxTree) (controls: Controls) =
        let rec interpret (AbstractSyntaxTree (codePosition, localBindings, LlamaExpression expression)) (dynamicEnvironment: Environment) : Value * Environment =
            // Wrap the LlamaIdentifier -> Llama into a LlamaIdentifier -> (Uninitialized Llama), to prepare for substituting each with initialized values when evaluated
            let uninitBindings (b: Association<LlamaIdentifier, Llama>) = b.List |> List.map (fun kvpair -> kvpair.key, Uninitialized kvpair.value) |> Association.Empty.PutAll

            let rec evaluateExpression expr (env: Environment) : Value * Environment =
                match expr.data with
                | Atom literal ->
                    match literal with
                    | LlamaLiteral.LlamaBool b -> ValueBool b
                    | LlamaLiteral.LlamaChar c -> ValueChar c
                    | LlamaLiteral.LlamaDouble d -> ValueDouble d
                    | LlamaLiteral.LlamaInt i -> ValueInt i
                    | LlamaLiteral.LlamaString s -> ValueString s
                    , env
                | Operation identifier ->
                    match identifier with
                    | LlamaOperator "next" ->
                        // Evaluate lhs, ignoring its value but keeping its state changes, then proceed to evaluate rhs afterwards
                        let leftVal, leftEnv = evaluateExpression expr.children.[0] env
                        let rightVal, rightEnv = evaluateExpression expr.children.[1] leftEnv
                        rightVal, rightEnv
                    | LlamaOperator "->" ->
                        // Evaluate lhs first, then rhs; only matters if bindings can be made in either. Aka shouldn't matter, but idk yet
                        let lhs, env = evaluateExpression expr.children.[0] env
                        let rhs, env = evaluateExpression expr.children.[1] env
                        match rhs with
                        | ValueFun (itype, otype, ast) when itype = lhs.Type ->
                            // Acceptable function
                            //interpret ast dynamicEnvironment
                            let input = match lhs with ValueString s -> s | _ -> "nope"
                            printfn "%s" input
                            Unit, env
                        | _ ->
                            // Unacceptable function
                            invalidArg "program" "function bad"
                    | LlamaOperator "$init" ->
                        let id = match expr.children.[0].data with Operation llamaId -> llamaId | _ -> invalidArg "init" "Not an id"
                        let llama = match env.Get id with Some (Uninitialized expr) -> expr | _ -> invalidArg "init" "Duplicate initialization"
                        let value, _ = interpret llama.def env // TODO find "public" bindings in sub-env and append to current env
                        //printf "init'ing %A" id
                        Unit, (env.Put id (Initialized value))
                    | LlamaName "PrintLine" -> ValueFun (StringType, UnitType, Unchecked.defaultof<AbstractSyntaxTree>), env
                    | LlamaName _ | LlamaOperator _ as id ->
                        match env.Get id |> Option.get with
                        | Initialized llama -> llama, env
                        | Uninitialized _ -> invalidArg "program" (sprintf "using uninitialized binding") // TODO only crash if couldn't initialize beforehand

            // First, insert all local bindings into the current dynamic environment
            let env = dynamicEnvironment.Append (uninitBindings localBindings)

            // Then, evaluate the code in this new environment
            evaluateExpression expression env

        Logger.Log Info "\n----------------------" controls
        interpret ast (Association.Empty.PutAll [])

