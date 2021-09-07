module CodeGen.Test exposing (..)

import Elm.CodeGen as Gen
import Elm.Pretty as GenPretty
import Expect
import Pretty exposing (pretty)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "code generation"
        [ test "arguments"
            (\() ->
                Gen.construct "f"
                    [ Gen.access
                        (Gen.construct "A" [ Gen.val "()" ]
                            |> Gen.parens
                        )
                        "a"
                    ]
                    |> GenPretty.prettyExpression
                    |> pretty 120
                    -- returns a wrong result
                    -- see https://github.com/the-sett/elm-syntax-dsl/issues/32
                    |> Expect.equal "f A ().a"
            )
        ]
