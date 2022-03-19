module NoRecordAliasConstructor.Test exposing (tests)

import Elm.Package as PackageMetadata
import Elm.Project as ProjectMetadata
import Elm.Type as TypeMetadata
import Elm.Version as VersionMetadata
import Json.Decode
import NoRecordAliasConstructor exposing (rule)
import NoRecordAliasConstructor.Common exposing (errorInfo)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency
import Review.Test as Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NoRecordAliasConstructor"
        [ fail
        , success
        ]


fail : Test
fail =
    describe "reports"
        [ test "record alias constructor call"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init : Foo
init =
    Foo "hello" True 0.2
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\" True 0.2"
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init : Foo
init =
    { foo = "hello", bar = True, baz = 0.2 }
"""
                        ]
            )
        , test "long record constructor lambda"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foofooTheTrain : String
    , barbarTheMan : Bool
    , bazbazTheBreakfast : Float
    }

init : Foo
init =
    let
        fooConstructor =
            identity Foo
    in
    fooConstructor
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo"
                            }
                            |> Test.atExactly
                                { start = { row = 13, column = 22 }, end = { row = 13, column = 25 } }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foofooTheTrain : String
    , barbarTheMan : Bool
    , bazbazTheBreakfast : Float
    }

init : Foo
init =
    let
        fooConstructor =
            identity (\\foofooTheTrain barbarTheMan bazbazTheBreakfast ->
                         { foofooTheTrain = foofooTheTrain
                         , barbarTheMan = barbarTheMan
                         , bazbazTheBreakfast = bazbazTheBreakfast
                         })
    in
    fooConstructor
"""
                        ]
            )
        , test "long record constructor"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foofooTheTrain : String
    , barbarTheMan : Bool
    , bazbazTheBreakfast : Float
    }

init : Foo
init =
    let
        fooConstructor =
            identity (Foo "hello, allow me to introduce myself" True 0.2)
    in
    fooConstructor
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello, allow me to introduce myself\" True 0.2"
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foofooTheTrain : String
    , barbarTheMan : Bool
    , bazbazTheBreakfast : Float
    }

init : Foo
init =
    let
        fooConstructor =
            identity ({ foofooTheTrain = "hello, allow me to introduce myself"
                      , barbarTheMan = True
                      , bazbazTheBreakfast = 0.2
                      })
    in
    fooConstructor
"""
                        ]
            )
        , test "record alias constructor curried"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init : Float -> Foo
init =
    Foo "hello" True
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\" True"
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init : Float -> Foo
init =
    (\\baz -> { foo = "hello", bar = True, baz = baz })
"""
                        ]
            )
        , test "record alias constructor as an argument"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init =
    identity Foo
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo"
                            }
                            |> Test.atExactly
                                { start = { row = 10, column = 14 }, end = { row = 10, column = 17 } }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init =
    identity (\\foo bar baz -> { foo = foo, bar = bar, baz = baz })
"""
                        ]
            )
        , test "record alias constructor"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    Foo
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo"
                            }
                            |> Test.atExactly
                                { start = { row = 10, column = 5 }, end = { row = 10, column = 8 } }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    (\\foo bar baz -> { foo = foo, bar = bar, baz = baz })
"""
                        ]
            )
        , test "nested record alias constructor"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    Foo (Foo "hello" True 0.2 |> .foo)
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\" True 0.2"
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    Foo ({ foo = "hello", bar = True, baz = 0.2 } |> .foo)
"""
                        , Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo (Foo \"hello\" True 0.2 |> .foo)"
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    (\\bar baz -> { foo = Foo "hello" True 0.2 |> .foo, bar = bar, baz = baz })
"""
                        ]
            )
        , test "record alias constructor from different dependency"
            (\() ->
                let
                    project : Project
                    project =
                        let
                            projectMetadata =
                                applicationElmJson
                        in
                        Project.new
                            |> Project.addDependency
                                (Dependency.create "lue-bird/elm-foo"
                                    projectMetadata
                                    [ { name = "Foo"
                                      , comment = ""
                                      , unions = []
                                      , aliases =
                                            [ { name = "Foo"
                                              , comment = ""
                                              , args = []
                                              , tipe =
                                                    TypeMetadata.Record
                                                        [ ( "foo", TypeMetadata.Var "String" )
                                                        , ( "bar", TypeMetadata.Var "Bool" )
                                                        , ( "baz", TypeMetadata.Var "Float" )
                                                        ]
                                                        Nothing
                                              }
                                            ]
                                      , values = []
                                      , binops = []
                                      }
                                    ]
                                )
                in
                [ """module A exposing (..)

import Foo exposing (Foo)

constructFoo =
    Foo
"""
                ]
                    |> Test.runOnModulesWithProjectData project rule
                    |> Test.expectErrorsForModules
                        [ ( "A"
                          , [ Test.error
                                { message = errorInfo.message
                                , details = errorInfo.details
                                , under = "Foo"
                                }
                                |> Test.atExactly
                                    { start = { row = 6, column = 5 }, end = { row = 6, column = 8 } }
                                |> Test.whenFixed
                                    """module A exposing (..)

import Foo exposing (Foo)

constructFoo =
    (\\foo bar baz -> { foo = foo, bar = bar, baz = baz })
"""
                            ]
                          )
                        ]
            )
        , test "record alias constructor from different module"
            (\() ->
                [ """module Foo exposing (Foo)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }
"""
                , """module A exposing (..)

import Foo exposing (Foo)

constructFoo =
    Foo
"""
                ]
                    |> Test.runOnModules rule
                    |> Test.expectErrorsForModules
                        [ ( "A"
                          , [ Test.error
                                { message = errorInfo.message
                                , details = errorInfo.details
                                , under = "Foo"
                                }
                                |> Test.atExactly
                                    { start = { row = 6, column = 5 }, end = { row = 6, column = 8 } }
                                |> Test.whenFixed
                                    """module A exposing (..)

import Foo exposing (Foo)

constructFoo =
    (\\foo bar baz -> { foo = foo, bar = bar, baz = baz })
"""
                            ]
                          )
                        ]
            )
        , noFixPossible
        ]


noFixPossible : Test
noFixPossible =
    describe "no fix possible"
        [ test "because of name clash in let block"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    let
        bar =
            "chocolate"
    in
    Foo "hello"
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\""
                            }
                            |> Test.atExactly
                                { start = { row = 14, column = 5 }, end = { row = 14, column = 16 } }
                        ]
            )
        , test "because of name clash in case"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    case Just 3 of
        Just bar ->
            Foo "hello"
        
        Nothing ->
            defaultFoo
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\""
                            }
                            |> Test.atExactly
                                { start = { row = 12, column = 13 }, end = { row = 12, column = 24 } }
                        ]
            )
        , test "because of name clash with top-level function"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

bar =
    "chocolate"

constructFoo =
    Foo "hello"
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\""
                            }
                            |> Test.atExactly
                                { start = { row = 13, column = 5 }, end = { row = 13, column = 16 } }
                        ]
            )
        , test "because of name clash with imported function"
            (\() ->
                """module A exposing (..)

import Foo exposing (bar)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    Foo "hello"
"""
                    |> Test.run rule
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = "Foo \"hello\""
                            }
                            |> Test.atExactly
                                { start = { row = 12, column = 5 }, end = { row = 12, column = 16 } }
                        ]
            )
        , test "because of name clash with imported function from module exposing (..)"
            (\() ->
                [ """module Foo exposing (..)

bar =
    "chocolate"
"""
                , """module A exposing (..)

import Foo exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

constructFoo =
    Foo "hello"
"""
                ]
                    |> Test.runOnModules rule
                    |> Test.expectErrorsForModules
                        [ ( "A"
                          , [ Test.error
                                { message = errorInfo.message
                                , details = errorInfo.details
                                , under = "Foo \"hello\""
                                }
                                |> Test.atExactly
                                    { start = { row = 12, column = 5 }, end = { row = 12, column = 16 } }
                            ]
                          )
                        ]
            )
        ]


success : Test
success =
    describe "does not report"
        [ test "record format"
            (\() ->
                """module A exposing (..)

type alias Foo = 
    { foo : String
    , bar : Bool
    , baz : Float
    }

init : Foo
init = 
    { foo = "hello"
    , bar = True
    , baz = 0.2    
    }
"""
                    |> Test.run rule
                    |> Test.expectNoErrors
            )
        , test "variant constructors"
            (\() ->
                """module A exposing (..)

greater : Order
greater = 
    GT
"""
                    |> Test.run rule
                    |> Test.expectNoErrors
            )
        , test "normal functions and values"
            (\() ->
                """module A exposing(..)

add : Int -> Int -> Int
add a b = 
    a + b

one = 1

three : Int
three =
    add one 2
"""
                    |> Test.run rule
                    |> Test.expectNoErrors
            )
        ]


applicationElmJson : ProjectMetadata.Project
applicationElmJson =
    let
        versionTupleToString ( mj, mn, pt ) =
            [ mj, mn, pt ]
                |> List.map String.fromInt
                |> String.join "."

        versionFromTuple version =
            case
                Json.Decode.decodeString VersionMetadata.decoder
                    ("\"" ++ versionTupleToString version ++ "\"")
            of
                Ok ok ->
                    ok

                Err err ->
                    Debug.todo ("Invalid version format: " ++ Debug.toString err)

        directDependencies =
            []
    in
    ProjectMetadata.Application
        { elm = versionFromTuple ( 0, 19, 1 )
        , dirs = [ "src" ]
        , depsDirect =
            directDependencies
                |> List.map
                    (\( name, version ) ->
                        case
                            Json.Decode.decodeString PackageMetadata.decoder
                                ("\"" ++ name ++ "\"")
                        of
                            Ok validName ->
                                ( validName, versionFromTuple version )

                            Err err ->
                                Debug.todo
                                    ([ "wrong direct-dependency name format: "
                                     , err |> Debug.toString
                                     ]
                                        |> String.concat
                                    )
                    )
        , depsIndirect = []
        , testDepsDirect = []
        , testDepsIndirect = []
        }
