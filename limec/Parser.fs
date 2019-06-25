namespace limec

module Parser =

    let Parse (code: LexedCode) (controls: Controls) : ParsedCode =
        code
        |> ParsedCode
