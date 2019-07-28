namespace limec

open System

module SemanticAnalyzer =

    let Analyze (ParsedCode tree: ParsedCode) (controls: Controls) (AST llamas: AST) : AST =
        match tree with
        | Terminal (element, token) -> To.Do ()
        | Nonterminal (element, subtree) -> To.Do ()
