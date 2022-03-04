module NoRecordAliasWithConstructor.Internal exposing (errorInfo)


errorInfo : { message : String, details : List String }
errorInfo =
    { message = "direct alias of record type"
    , details =
        [ "Read more about the why in [`no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)."
        , [ "Use [`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)"
          , "when directly aliasing a record type to avoid its constructor function becoming unavailable."
          ]
            |> String.concat
        ]
    }
