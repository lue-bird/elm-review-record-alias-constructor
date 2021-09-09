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


## usage

### trying it out

```noformattingples
elm-review --template lue-bird/elm-record-alias-constructor/example
```

### using it

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
- ... doesn't report alias constructors that aren't called directly
  ```elm
  (User <| "A") <| 42
  (identity User) "Bill" 42
  User |> (\user -> user "Bill" 42)
  ```
  aren't reported for example
- ... doesn't provide automatic fixes → refactoring is inconvenient
- ... only looks for type aliases in the same module. This package finds _every record alias_, even in the dependencies

## too verbose?

```elm
decodeUser =
    map2 (\name status -> { name = name, status = status })
        (field "name" string)
        (field "status" string)
```
is rather verbose.

Ultimately, I think elm could introduce some simpler syntax, for example

```elm
decodeUser =
    map2 (\name status -> { name, status })
        (field "name" string)
        (field "status" string)
```
like purescript does it or
```elm
decodeUser =
    map2 { name, status }
        (field "name" string)
        (field "status" string)
```
(or something crazy using field addition / record unions)

[lxierita/no-typealias-constructor-call]: https://package.elm-lang.org/packages/lxierita/no-typealias-constructor-call/latest/
