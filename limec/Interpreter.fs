namespace limec

module Interpreter =

    type ValueType =
        | UnitType | StringType | CharType | IntType | DoubleType | BoolType | FunType of ValueType * ValueType | TupleType of int

    type Value =
        | Unit
        | ValueString of string
        | ValueChar of char
        | ValueInt of int64
        | ValueDouble of double
        | ValueBool of bool
        | ValueFun of ValueType * ValueType * AbstractSyntaxTree // TODO store input pattern within AST, otherwise don't know what inputs get put in what bindings!
        | ValueLibFun of ValueType * ValueType * (Value -> Value)
        | ValueTuple of int * Value list // TODO add subtypes to tuple's type, not just number of elements

        member this.Type =
            match this with
            | Unit -> UnitType
            | ValueString _ -> StringType
            | ValueChar _ -> CharType
            | ValueInt _ -> IntType
            | ValueDouble _ -> DoubleType
            | ValueBool _ -> BoolType
            | ValueFun (i, o, _) | ValueLibFun (i, o, _) -> FunType (i, o)
            | ValueTuple (n, _) -> TupleType n

    type IdentifierValue =
        | Initialized of Value
        | Uninitialized of Llama

    type Environment = Association<LlamaIdentifier, IdentifierValue>

    let Interpret (controls: Parameters) (ast: AbstractSyntaxTree) =
        let rec interpret (AbstractSyntaxTree (codePosition, localBindings, LlamaExpression expression)) (dynamicEnvironment: Environment) : Value * Environment =
            // Wrap the LlamaIdentifier -> Llama into a LlamaIdentifier -> (Uninitialized Llama), to prepare for substituting each with initialized values when evaluated
            let uninitBindings (b: Association<LlamaIdentifier, Llama>) = b.List |> List.map (fun kvpair -> kvpair.key, Uninitialized kvpair.value) |> Association.Empty.PutAll

            let rec evaluateExpression (env: Environment) expr : Value * Environment =
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
                        let leftVal, leftEnv = evaluateExpression env expr.children.[0]
                        let rightVal, rightEnv = evaluateExpression leftEnv expr.children.[1]
                        rightVal, rightEnv
                    | LlamaOperator "->" -> // Since left-associative, guarenteed that rhs is (w/r/t (->) op) atomic, while lhs can be recursive
                        // TODO Evaluate lhs first, then rhs; only matters if bindings can be made in either. Aka shouldn't matter, but idk yet
                        let lhs, env = evaluateExpression env expr.children.[0]
                        let rhs, env = evaluateExpression env expr.children.[1]
                        match rhs with
                        | ValueFun (itype, otype, ast) -> //when itype = lhs.Type -> TODO typecheck when it makes sense
                            // Acceptable function
                            interpret ast (env.Put (LlamaName "in") (Initialized lhs))
                        | ValueLibFun (itype, otype, func) when itype = lhs.Type ->
                            lhs |> func, env // TODO For now, assuming lib functions don't affect the environment
                        | _ ->
                            // Not a function
                            match expr.children.[1].data with
                            | Operation (LlamaName variable) -> // TODO ensure value is mutable - will require retaining type information of values?
                                Unit, (env.Put (LlamaName variable) (Initialized lhs))
                            | _ -> invalidArg "rhs" "can't transfer data into rhs"
                    | LlamaName "first" ->
                        ValueLibFun (TupleType 2, UnitType, (fun (i: Value) -> match i with | ValueTuple (2, [ v; _ ]) -> v | _ -> invalidArg "input" "first: bad argument type")), env
                    | LlamaName "second" ->
                        ValueLibFun (TupleType 2, UnitType, (fun (i: Value) -> match i with | ValueTuple (2, [ _; v ]) -> v | _ -> invalidArg "input" "second: bad argument type")), env
                    | LlamaName "unit" -> Value.Unit, env
                    | LlamaOperator "." ->
                        // TODO allow expression to produce types (w/ members) as values, so lhs and rhs will need to be evaluated into some sort of ValueType first
                        let lhs = expr.children.[0]
                        let rhs = expr.children.[1]
                        match lhs, rhs with
                        | { data = Operation (LlamaName "dotnet"); children = [] }, { data = Operation (LlamaName func); children = [] } ->
                            match func with
                            | "Write" -> ValueLibFun (StringType, UnitType, (fun (i: Value) ->
                                match i with
                                | ValueString string ->
                                    System.Console.Write string
                                    Unit
                                | _ -> invalidArg "input" "Write: bad argument type"))
                            | "WriteLine" -> ValueLibFun (StringType, UnitType, (fun (i: Value) ->
                                match i with
                                | ValueString string ->
                                    System.Console.WriteLine string
                                    Unit
                                | _ -> invalidArg "input" "WriteLine: bad argument type"))
                            | "ReadLine" -> ValueLibFun (UnitType, StringType, (fun (i: Value) ->
                                match i with
                                | Unit ->
                                    ValueString (System.Console.ReadLine ())
                                | _ -> invalidArg "input" "ReadLine: bad argument type"))
                            | _ -> invalidArg func "Unknown library function"
                            , env
                        | _ -> invalidArg "program" "Unknown lhs to dot" // TODO call functions within types
                    | LlamaOperator "$tuple" ->
                        ValueTuple (expr.children.Length, List.map (evaluateExpression env >> fst) expr.children), env // TODO assuming env stays the same

                    | LlamaOperator "$init" ->
                        let id = match expr.children.[0].data with Operation llamaId -> llamaId | _ -> invalidArg "init" "Not an id"
                        let llama = match env.Get id with Some (Uninitialized expr) -> expr | _ -> invalidArg "init" "Duplicate initialization"
                        let value =
                            if List.contains (LlamaName "subroutine") llama.typ then // TODO replace hardcoded types/operators with global constants
                                ValueFun (UnitType, UnitType, llama.def) // TODO actually typecheck (?) and use correct I/O types
                            else
                                interpret llama.def env |> fst // TODO find "public" bindings in sub-env and append to current env
                        Unit, (env.Put id (Initialized value))

                    | LlamaOperator "!" ->
                        let value, env = evaluateExpression env expr.children.[0] // TODO should we really keep the new env?
                        match value with
                        | ValueFun (UnitType, otype, func) ->
                            interpret func env
                        | ValueLibFun (UnitType, otype, func) ->
                            Unit |> func, env // TODO assume func doesn't change env
                        | _ -> invalidArg "!" "Not given a func"

                    | LlamaOperator "$if-then-else" ->
                        let condition, thenExpr, elseExpr = match expr.children with [ c; t; e ] -> c, t, e | _ -> invalidArg "if-then-else" "expected exactly 3 child expressions"
                        match evaluateExpression env condition with
                        // TODO ignoring any changes to the environment made by evaluating the condition, is that right?
                        | ValueBool true, _ -> evaluateExpression env thenExpr
                        | ValueBool false, _ -> evaluateExpression env elseExpr
                        | _ -> invalidArg "if-then-else" "expected condition to be a boolean"

                    | LlamaOperator "==" ->
                        let left, right = match expr.children with [ l; r ] -> l, r | _ -> invalidArg "==" "expected exactly 2 child expressions"
                        // TODO ignoring any changes to the environment made by evaluating either side, is that right?
                        let (leftValue, _), (rightValue, _) = evaluateExpression env left, evaluateExpression env right
                        let rec isEqual (left, right) =
                            match left, right with
                            | Unit, Unit -> true
                            | ValueString a, ValueString b -> a = b
                            | ValueChar a, ValueChar b -> a = b
                            | ValueInt a, ValueInt b -> a = b
                            | ValueBool a, ValueBool b -> a = b
                            | ValueDouble a, ValueDouble b -> a = b // TODO should i use fancier double equality or leave it as is, and let the client decide what kind of equality?
                            | ValueTuple (xn, xs), ValueTuple (yn, ys) -> xn = yn && not (List.zip xs ys |> List.map isEqual |> List.contains false)
                            | _ -> false //invalidArg "==" (sprintf "cannot test equality on %A and %A" left right) // TODO stop being dynamically typed
                        (ValueBool (isEqual (leftValue, rightValue)), env)

                    | LlamaName _ | LlamaOperator _ as id ->
                        match env.Get id with
                        | Some (Initialized llama) -> llama, env
                        | Some (Uninitialized _) -> invalidArg "program" (sprintf "using uninitialized binding: %A" id) // TODO only crash if couldn't initialize beforehand
                        | _ -> invalidArg "program" (sprintf "unknown identifier: %A" id)

            // First, insert all local bindings into the current dynamic environment
            let env = dynamicEnvironment.Append (uninitBindings localBindings)

            // Then, evaluate the code in this new environment
            evaluateExpression env expression

        Logger.Log Info "\n----------------------" controls
        interpret ast (Association.Empty.PutAll [])

