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

    let Interpret (ast: AbstractSyntaxTree) =
        let rec interpret (AbstractSyntaxTree (codePosition, localBindings, LlamaExpression expression)) (dynamicEnvironment: Association<LlamaIdentifier, Llama>) =
            
            let rec evaluateExpression expr =
                match expr.data with
                | Atom literal ->
                    match literal with
                    | LlamaLiteral.LlamaBool b -> ValueBool b
                    | LlamaLiteral.LlamaChar c -> ValueChar c
                    | LlamaLiteral.LlamaDouble d -> ValueDouble d
                    | LlamaLiteral.LlamaInt i -> ValueInt i
                    | LlamaLiteral.LlamaString s -> ValueString s
                | Operation identifier ->
                    match identifier with
                    | LlamaOperator "next" ->
                        // Evaluate lhs, then proceed to evaluate rhs afterwards
                        let lhs = evaluateExpression expr.children.[0]
                        evaluateExpression expr.children.[1]
                    | LlamaOperator "thunk" ->
                        let delayedExpression = evaluateExpression expr.children.[0] // TODO don't evaluate useless value, just typecheck
                        let delayedAST = AbstractSyntaxTree (codePosition, dynamicEnvironment, LlamaExpression expr.children.[0])
                        ValueFun (UnitType, delayedExpression.Type, delayedAST)

                    | LlamaOperator "->" ->
                        let lhs = evaluateExpression expr.children.[0]
                        let rhs = evaluateExpression expr.children.[1]
                        match rhs with
                        | ValueFun (itype, otype, ast) when itype = lhs.Type ->
                            // Acceptable function
                            //interpret ast dynamicEnvironment
                            let input = match lhs with ValueString s -> s | _ -> "nope"
                            printf "Console output: %s" input
                            Unit
                        | _ ->
                            // Unacceptable function
                            invalidArg "program" "function bad"
                    | LlamaName "PrintLine" -> ValueFun (StringType, UnitType, Unchecked.defaultof<AbstractSyntaxTree>)
                    | _ -> Unit

            evaluateExpression expression

        interpret ast Association.Empty

