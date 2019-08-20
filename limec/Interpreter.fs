﻿namespace limec

module Interpreter =

    type ValueType =
        | UnitType | StringType | CharType | IntType | DoubleType | BoolType | ClosureType of ValueType * ValueType | TupleType of int | ModuleType

    type Value =
        | Unit
        | ValueString of string
        | ValueChar of char
        | ValueInt of int64
        | ValueDouble of double
        | ValueBool of bool
        | ValueClosure of ValueType * ValueType * Llama * LlamaIdentifier * Environment // TODO store input pattern within AST, otherwise don't know what inputs get put in what bindings!
        | ValueLibFun of ValueType * ValueType * (Value -> Value)
        | ValueTuple of int * Value list // TODO add subtypes to tuple's type, not just number of elements
        | ValueModule of Environment

        member this.Type =
            match this with
            | Unit -> UnitType
            | ValueString _ -> StringType
            | ValueChar _ -> CharType
            | ValueInt _ -> IntType
            | ValueDouble _ -> DoubleType
            | ValueBool _ -> BoolType
            | ValueClosure (i, o, _, _, _) | ValueLibFun (i, o, _) -> ClosureType (i, o)
            | ValueTuple (n, _) -> TupleType n
            | ValueModule _ -> ModuleType

    and IdentifierValue =
        | Initialized of Value
        | Uninitialized of Llama

    and Environment = Association<LlamaIdentifier, IdentifierValue>

    let Interpret (controls: Parameters) (llamas: Llama list) =
        let rec interpret { Llama.typ = typ; def = AbstractSyntaxTree (codePosition, localBindings, LlamaExpression expression)} (dynamicEnvironment: Environment) : Value * Environment =
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
                        let lhs, _ = evaluateExpression env expr.children.[0]
                        let rhs, _ = evaluateExpression env expr.children.[1]
                        match rhs with
                        | ValueClosure (itype, otype, llama, recursiveName, closureEnv) -> //when itype = lhs.Type -> TODO typecheck when it makes sense
                            // Acceptable function
                            let result, _ = interpret llama ((closureEnv.Put (LlamaName "in") (Initialized lhs)).Put recursiveName (Initialized rhs))
                            result, env // TODO should function be able to affect env?
                        | ValueLibFun (itype, otype, func) -> // when itype = lhs.Type -> TODO typecheck when we have types
                            lhs |> func, env // TODO For now, assuming lib functions don't affect the environment
                        | _ ->
                            // Not a function
                            match expr.children.[1].data with
                            | Operation (LlamaName variable) -> // TODO ensure value is mutable - will require retaining type information of values?
                                Unit, (env.Put (LlamaName variable) (Initialized lhs))
                            | _ -> invalidArg "rhs" "can't transfer data into rhs"
                    | LlamaName "nth" ->
                        ValueLibFun (TupleType -1, UnitType, (fun (v: Value) ->
                            match v with
                            | ValueTuple (2, [ ValueInt i; ValueTuple (n, t) ]) when (int i) <= n -> List.item ((int i) - 1) t
                            | _ -> invalidArg "input" "nth expected a tuple (index, tuple)"
                        )), env
                    | LlamaName "first" ->
                        ValueLibFun (TupleType 2, UnitType, (fun (i: Value) -> match i with | ValueTuple (2, [ v; _ ]) -> v | _ -> invalidArg "input" "first: bad argument type")), env
                    | LlamaName "second" ->
                        ValueLibFun (TupleType 2, UnitType, (fun (i: Value) -> match i with | ValueTuple (2, [ _; v ]) -> v | _ -> invalidArg "input" "second: bad argument type")), env
                    | LlamaName "unit" -> Value.Unit, env
                    | LlamaOperator "+" ->
                        let (lhs, _), (rhs, _) = evaluateExpression env expr.children.[0], evaluateExpression env expr.children.[1]
                        match lhs, rhs with
                        | ValueString s1, ValueString s2 -> ValueString (s1 + s2), env
                        | ValueInt i1, ValueInt i2 -> ValueInt (i1 + i2), env
                        | _ -> invalidArg "code" (sprintf "Cannot perform (+) on values %A, %A" lhs rhs)
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
                                | _ -> invalidArg "input" (sprintf "WriteLine: bad argument %A" i)))
                            | "ReadLine" -> ValueLibFun (UnitType, StringType, (fun (i: Value) ->
                                match i with
                                | Unit ->
                                    ValueString (System.Console.ReadLine ())
                                | _ -> invalidArg "input" "ReadLine: bad argument type"))
                            | _ -> invalidArg func "Unknown library function"
                            , env
                        | { data = Operation moduleName; children = [] }, { data = Operation moduleMember; children = [] } ->
                            match env.Get moduleName with
                            | Some (Initialized (ValueModule moduleEnv)) ->
                                match moduleEnv.Get moduleMember with
                                | Some (Initialized value) -> value, env
                                | _ -> invalidArg "code" (sprintf "%A is not a member of the module %A or has not been initialized yet" moduleMember moduleName)
                            | _ -> invalidArg "code" (sprintf "Module %A does not exist or has not been initialized yet" moduleName)
                        | _ -> invalidArg "program" "Unknown lhs to dot" // TODO call functions within types
                    | LlamaOperator "$tuple" ->
                        ValueTuple (expr.children.Length, List.map (evaluateExpression env >> fst) expr.children), env // TODO assuming env stays the same

                    | LlamaOperator "$list" ->
                        (List.foldBack (fun elem lis ->
                            ValueTuple (2, [ evaluateExpression env elem |> fst; lis ])
                        ) expr.children Unit), env // TODO assuming env stays the same, right??

                    | LlamaOperator "$init" ->
                        let id = match expr.children.[0].data with Operation llamaId -> llamaId | _ -> invalidArg "init" "Not an id"
                        let llama = match env.Get id with Some (Uninitialized expr) -> expr | _ -> invalidArg "init" "Duplicate initialization"
                        let value =
                            if List.contains (LlamaName "subroutine") llama.typ then // TODO replace hardcoded types/operators with global constants
                                ValueClosure (UnitType, UnitType, llama, id, env) // TODO actually typecheck (?) and use correct I/O types
                            elif List.contains (LlamaName "module") llama.typ then
                                let _, resultEnv = interpret llama Association.Empty // TODO allow chained (?) module inheritance (?) what am i saying (?)
                                ValueModule resultEnv
                            else
                                interpret llama env |> fst // TODO find "public" bindings in sub-env and append to current env (?)
                        Unit, (env.Put id (Initialized value))

                    | LlamaOperator "!" -> // TODO make this evaluate completed functions from "->" as well
                        let value, _ = evaluateExpression env expr.children.[0] // TODO should it be possible to effect the environment?
                        match value with
                        | ValueClosure (UnitType, otype, func, recursiveName, closureEnv) ->
                            interpret func (closureEnv.Put recursiveName (Initialized value))
                        | ValueLibFun (UnitType, otype, func) ->
                            Unit |> func, env // TODO assume func doesn't change env
                        | _ -> invalidArg "!" "Not given a func"

                    | LlamaOperator "-" ->
                        let value, _ = evaluateExpression env expr.children.[0]
                        match value with
                        | ValueInt i -> ValueInt (-i), env
                        | _ -> invalidArg "code" (sprintf "%s(-) expected and integer, given %A" (codePosition.ToString ()) value)

                    | LlamaOperator "$if-then-else" ->
                        let condition, thenExpr, elseExpr = match expr.children with [ c; t; e ] -> c, t, e | _ -> invalidArg "if-then-else" "expected exactly 3 child expressions"
                        match evaluateExpression env condition with
                        // TODO ignoring any changes to the environment made by evaluating the condition, is that right?
                        | ValueBool true, _ -> evaluateExpression env thenExpr
                        | ValueBool false, _ -> evaluateExpression env elseExpr
                        | _ -> invalidArg "if-then-else" "expected condition to be a boolean"

                    | LlamaOperator ">" ->
                        let left, right = match expr.children with [ l; r ] -> l, r | _ -> invalidArg ">" "expected exactly 2 child expressions"
                        // TODO ignoring any changes to the environment made by evaluating either side, is that right?
                        let (leftValue, _), (rightValue, _) = evaluateExpression env left, evaluateExpression env right
                        match leftValue, rightValue with
                        | ValueInt a, ValueInt b -> ValueBool (a > b), env
                        | _ -> invalidArg "code" (sprintf "%s(>) expected two integers, given %A and %A" (codePosition.ToString ()) left right)

                    | LlamaOperator "<" ->
                        let left, right = match expr.children with [ l; r ] -> l, r | _ -> invalidArg "<" "expected exactly 2 child expressions"
                        // TODO ignoring any changes to the environment made by evaluating either side, is that right?
                        let (leftValue, _), (rightValue, _) = evaluateExpression env left, evaluateExpression env right
                        match leftValue, rightValue with
                        | ValueInt a, ValueInt b -> ValueBool (a < b), env
                        | _ -> invalidArg "code" (sprintf "%s(<) expected two integers, given %A and %A" (codePosition.ToString ()) left right)

                    | LlamaOperator "/" ->
                        let left, right = match expr.children with [ l; r ] -> l, r | _ -> invalidArg "/" "expected exactly 2 child expressions"
                        // TODO ignoring any changes to the environment made by evaluating either side, is that right?
                        let (leftValue, _), (rightValue, _) = evaluateExpression env left, evaluateExpression env right
                        match leftValue, rightValue with
                        | ValueInt a, ValueInt b -> ValueInt (a / b), env
                        | _ -> invalidArg "code" (sprintf "%s(/) expected two integers, given %A and %A" (codePosition.ToString ()) left right)

                    | LlamaOperator "%" ->
                        let left, right = match expr.children with [ l; r ] -> l, r | _ -> invalidArg "%" "expected exactly 2 child expressions"
                        // TODO ignoring any changes to the environment made by evaluating either side, is that right?
                        let (leftValue, _), (rightValue, _) = evaluateExpression env left, evaluateExpression env right
                        match leftValue, rightValue with
                        | ValueInt a, ValueInt b -> ValueInt (a % b), env
                        | _ -> invalidArg "code" (sprintf "%s(%%) expected two integers, given %A and %A" (codePosition.ToString ()) left right)

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
                        | Some (Uninitialized _) -> invalidArg "program" (sprintf "%susing uninitialized binding: %A" (codePosition.ToString ()) id) // TODO only crash if couldn't initialize beforehand
                        | _ -> invalidArg "program" (sprintf "%sunknown identifier: %A" (codePosition.ToString ()) id )

            // First, insert all local bindings into the current dynamic environment
            let env = dynamicEnvironment.Append (uninitBindings localBindings)

            // Then, evaluate the code in this new environment
            evaluateExpression env expression

        Logger.Log Info "\n----------------------" controls
        List.fold (fun (env: Environment) llama ->
            //printfn "Env: %A" (env.ToString ())
            interpret llama env |> snd) Association.Empty llamas // TODO should we preserve values from interpreting?

