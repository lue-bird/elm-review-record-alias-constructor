module NoRecordAliasConstructor exposing (rule)

{-|

@docs rule

-}

import NoRecordAliasConstructor.Internal as Internal
import Review.Rule exposing (Rule)


{-| 🔧`NoRecordAliasConstructor` forbids using the automatically generated record type alias constructor.


## Examples

    type alias User =
        { name : String, age : Int }

    User "Balsa" 42

will be marked as error and automatically fixed:

    { name = "Balsa", age = 42 }

The same goes for cases where no arguments are applied:

    map2 User
        (field "name" string)
        (field "age" int)

fixed

    map2 (\name age -> { name = name, age = age })
        (field "name" string)
        (field "age" int)

See the [readme](https://package.elm-lang.org/packages/lue-bird/elm-review-record-alias-constructor/latest/) for why this is useful.


## Configuration

    config : List Rule
    config =
        [ NoRecordAliasConstructor.rule
        ]

-}
rule : Rule
rule =
    Internal.rule
