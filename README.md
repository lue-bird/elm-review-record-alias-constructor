# elm-review-record-alias-constructor

[elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule: forbid using record type alias constructors. 

In contrast to [lxierita's no-typealias-constructor-call](lxierita/no-typealias-constructor-call), this rule also reports constructors that aren't called (for example in `Json.Decode.mapN` functions). [↑ all differences](#comparison-to-no-typealias-constructor-call)

## examples

```elm
type alias User =
    { name : String, age : Int }
```

```elm
User "Balsa" 42
```
will be marked as error and automatically fixed:
```elm
{ name = "Balsa", age = 42 }
```

The same goes for cases where no arguments are applied:
```elm
map2 User
    (field "name" string)
    (field "age" int)
```
fixed
```elm
map2 (\name age -> { name = name, age = age })
    (field "name" string)
    (field "age" int)
```

[Skip to "try it out"](#try-it-out)

## why

Fields in a record don't have a "natural order".

```elm
{ age = 42, name = "Balsa" }
== { name = "Balsa", age = 42 }
--> True
```

So it shouldn't matter whether you write

```elm
type alias User =
    { name : String, age : Int }
or  { age : Int, name : String }
```
as well.

```elm
User "Balsa" 42
```
however relies on a specific field order in the type and is more difficult to understand/read.
These constructors also open up the possibility for bugs to sneak in without the compiler warning you:

```elm
type alias User =
    { status : String
    , name : String 
    }

decodeUser : Decoder User
decodeUser =
    map2 User
        (field "name" string)
        (field "status" string)
```
Did you spot the mistake? [↑ a similar example](https://sporto.github.io/elm-patterns/advanced/pipeline-builder.html#caveat)

To avoid these kinds of bugs, don't use type alias constructors:
```elm
decodeUser : Decoder User
decodeUser =
    map2 (\name status -> { name = name, status = status })
        (field "name" string)
        (field "status" string)
```

Problems don't end there.

### implicit magic

>"There are worse things than being explicit" – Evan

Most record type aliases are not intended to work with positional arguments!
`Model` as the perfect example.

### there are better alternatives

It's so easy to create an explicit constructor

```elm
xy : Float -> Float -> { x : Float, y : Float }
```

As argued, unnamed arguments shouldn't be the default.
Additionally, your record will be more descriptive and type-safe using `type`

```elm
type Cat
    = Cat { mood : Mood, birthTime : Time.Posix }
```

to make wrapping, unwrapping and combining easier, you can try [typed-value](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/).

### only generated for specific scenarios

```elm
type alias NoConstructor =
    Extensible {}

type alias NoConstructor =
    RecordTypeAlias
```

[Skip to "try it out"](#try-it-out)

### `succeed`/`constant` are misused

For record `Codec`s (from [MartinSStewart's `elm-serialize`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) in this example) where we don't need to encode every field value:
```elm
serializeProject : Codec String Project
serializeProject =
    record Project
        |> field .name string
        |> field .scale float
        |> field .selected
            (succeed NothingSelected)
        |> finishRecord
```
`succeed` is a weird concept for codecs because some dummy value must be encoded which will never be read.

It does not exist in elm-serialize, but it does exist in [miniBill's `elm-codec`](https://package.elm-lang.org/packages/miniBill/elm-codec/latest) (, [prozacchiwawa's `elm-json-codec`](https://package.elm-lang.org/packages/prozacchiwawa/elm-json-codec/latest), ...):
> Create a Codec that produces null as JSON and always decodes as the same value.

Do you really want this behavior? If not, you'll need
```elm
serializeProject : Codec String Project
serializeProject =
    record
        (\name scale ->
            { name = name
            , scale = scale
            , selected = NothingSelected
            }
        )
        |> field .name string
        |> field .scale float
        |> finishRecord
```
So why not consistently use this record constructing method?

This will also be used often for versioning
```elm
enum ProjectVersion0 [ ... ]
    |> andThen
        (\version ->
            case version of
                ProjectVersion0 ->
                    record
                        (\name -> { name = name, scale = 1 })
                        |> field .name string
                        |> finishRecord
                    
                ...
        )
```
Again: Why not consistently use one record constructing method?


## try it out

```noformattingples
elm-review --template lue-bird/elm-record-alias-constructor/example
```

## use it

After adding [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) to your project, import this rule from
your `ReviewConfig.elm` file and add it to the config. E.g.:

```elm
import NoRecordAliasConstructor
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoRecordAliasConstructor.rule
    ]

```

## comparison to no-typealias-constructor-call

[lxierita/no-typealias-constructor-call](lxierita/no-typealias-constructor-call)...
- ... only reports record alias constructors that are called directly
  ```elm
  (User <| "A") <| 42
  (identity User) "Bill" 42
  User |> (\user -> user "Bill" 42)
  ```
  aren't reported for example
- ... doesn't provide automatic fixes → refactoring is inconvenient
- ... only looks for type aliases in the same module. This package finds _every used record alias_

## too verbose?

```elm
decodeUser =
    map2 (\name status -> { name = name, status = status })
        (field "name" string)
        (field "status" string)
```
is rather verbose.

There are languages that introduce extra sugar

```elm
-- purescript
decodeUser =
    map2 (\name status -> { name, status })
        (field "name" string)
        (field "status" string)

-- Dhall
decodeUser =
    map2 { name, status }
        (field "name" string)
        (field "status" string)
```
Which all have problems (see the [`succeed`/`constant` are misused section](#succeed/constant-are-misused) for example)!

Maybe something crazy using record unions would be neat but... elm will probably be kept simple.

[lxierita/no-typealias-constructor-call]: https://package.elm-lang.org/packages/lxierita/no-typealias-constructor-call/latest/
