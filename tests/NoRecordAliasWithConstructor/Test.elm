module NoRecordAliasWithConstructor.Test exposing (tests)

import NoRecordAliasWithConstructor exposing (configDefault, importRecordWithoutConstructorFunctionTypeAlias, ruleWith)
import NoRecordAliasWithConstructor.Common exposing (errorInfo)
import Review.Test as Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NoRecordAliasWithConstructor"
        [ test "no direct alias"
            (\() ->
                """module A exposing (..)
import Some.Module exposing (SomeRecordWithoutConstructorFunction)

type alias A =
    SomeRecordWithoutConstructorFunction
        { a : ()
        }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectNoErrors
            )
        , test "no existing import, no module comment"
            (\() ->
                """module A exposing (..)

type alias A =
    { a : ()
    }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = """{ a : ()
    }"""
                            }
                            |> Test.whenFixed
                                """module A exposing (..)
import Your.Module exposing (YourRecordWithoutConstructorFunction)

type alias A =
    YourRecordWithoutConstructorFunction
        { a : ()
        }
"""
                        ]
            )
        , test "no existing import but a module comment"
            (\() ->
                """module A exposing (..)

{-| A
-}

type alias A =
    { a : ()
    }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = """{ a : ()
    }"""
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

{-| A
-}
import Your.Module exposing (YourRecordWithoutConstructorFunction)

type alias A =
    YourRecordWithoutConstructorFunction
        { a : ()
        }
"""
                        ]
            )
        , test "no existing import, a port doc comment"
            (\() ->
                """port module A exposing (..)

{-| A
-}
port p : ()

type alias A =
    { a : ()
    }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = """{ a : ()
    }"""
                            }
                            |> Test.whenFixed
                                """port module A exposing (..)
import Your.Module exposing (YourRecordWithoutConstructorFunction)

{-| A
-}
port p : ()

type alias A =
    YourRecordWithoutConstructorFunction
        { a : ()
        }
"""
                        ]
            )
        , test "required import exists"
            (\() ->
                """module A exposing (..)

import Your.Module exposing (YourRecordWithoutConstructorFunction)


type alias A =
    { a : ()
    }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = """{ a : ()
    }"""
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

import Your.Module exposing (YourRecordWithoutConstructorFunction)


type alias A =
    YourRecordWithoutConstructorFunction
        { a : ()
        }
"""
                        ]
            )
        , test "imports but required import doesn't exist"
            (\() ->
                """module A exposing (..)

import Other.Module


type alias A =
    { a : ()
    }
"""
                    |> Test.run
                        (ruleWith
                            (configDefault
                                |> importRecordWithoutConstructorFunctionTypeAlias
                                    { moduleName = "Your.Module"
                                    , typeAliasName = "YourRecordWithoutConstructorFunction"
                                    }
                            )
                        )
                    |> Test.expectErrors
                        [ Test.error
                            { message = errorInfo.message
                            , details = errorInfo.details
                            , under = """{ a : ()
    }"""
                            }
                            |> Test.whenFixed
                                """module A exposing (..)

import Your.Module exposing (YourRecordWithoutConstructorFunction)
import Other.Module


type alias A =
    YourRecordWithoutConstructorFunction
        { a : ()
        }
"""
                        ]
            )
        ]
