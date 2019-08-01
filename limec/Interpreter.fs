namespace limec

module Interpreter =

    type IsInitialized = Initialized | Uninitialized

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

    let Interpret (ast: AbstractSyntaxTree) (controls: Controls) =
        let rec interpret (AbstractSyntaxTree (codePosition, localBindings, LlamaExpression expression)) (dynamicEnvironment: Association<LlamaIdentifier, IsInitialized * Llama>) =
            let uninitBindings (b: Association<LlamaIdentifier, Llama>) = b.List |> List.map (fun kvpair -> kvpair.key, (Uninitialized, kvpair.value)) |> Association.Empty.PutAll

            let rec evaluateExpression expr (env: Association<LlamaIdentifier, IsInitialized * Llama>) : Value * Association<LlamaIdentifier, IsInitialized * Llama> =
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
                        // Evaluate lhs, then proceed to evaluate rhs afterwards
                        let _, env = evaluateExpression expr.children.[0] env
                        evaluateExpression expr.children.[1] env
                        |> fst, env
                    (*| LlamaOperator "thunk" ->
                        let delayedExpression = evaluateExpression expr.children.[0] env // TODO don't evaluate useless value, just typecheck
                        let delayedAST = AbstractSyntaxTree (codePosition, env, LlamaExpression expr.children.[0])
                        ValueFun (UnitType, delayedExpression.Type, delayedAST)
                    | LlamaOperator "unthunk" ->
                        let thunk = expr.children.[0]
                        Unit*)
                    | LlamaOperator "->" ->
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
                        let _, llama = env.Get id |> Option.get
                        //interpret llama.def env
                        Unit, (env.Put id (Initialized, llama))
                        // TODO cache value
                    | LlamaName "PrintLine" -> ValueFun (StringType, UnitType, Unchecked.defaultof<AbstractSyntaxTree>), env
                    | LlamaName _ | LlamaOperator _ as id ->
                        let isInit, llama = env.Get id |> Option.get
                        match isInit with
                        | Uninitialized | Initialized -> interpret llama.def env
                        //| Uninitialized -> invalidArg "program" "using uninitialized binding" // TODO only crash if actually evaluating the binding, not just using

            // First, insert all local bindings into the current dynamic environment
            let env = dynamicEnvironment.Append (uninitBindings localBindings)

            // Then, evaluate the code in this new environment
            evaluateExpression expression env

        Logger.Log Info "\n----------------------" controls
        interpret ast Association.Empty

