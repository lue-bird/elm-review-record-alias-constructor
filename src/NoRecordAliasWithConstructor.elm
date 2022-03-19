module NoRecordAliasWithConstructor exposing
    ( rule, ruleWith
    , Config, configDefault, importRecordWithoutConstructorFunctionTypeAlias
    )

{-|

@docs rule, ruleWith


## configure

@docs Config, configDefault, importRecordWithoutConstructorFunctionTypeAlias

-}

import Common exposing (indentFurther, indentationLevel, isDocComment, moduleNameToString)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import NoRecordAliasWithConstructor.Common exposing (errorInfo)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)


{-| Configuration where you can specify

  - where/what your `RecordWithoutConstructorFunction` is: [`importRecordWithoutConstructorFunctionTypeAlias`](#importRecordWithoutConstructorFunctionTypeAlias)

-}
type Config
    = Config ConfigInternal


type alias ConfigInternal =
    { recordWithoutConstructorFunction :
        { moduleName : String, typeAliasName : String }
    }


{-| The standard [`Config`](#Config) used for [`rule`](#rule)s without further configuration:

    rule =
        NoRecordAliasWithConstructor.ruleWith
            NoRecordAliasWithConstructor.configDefault

  - imports [`RecordWithoutConstructorFunction` from `lue-bird/elm-no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/):

        importRecordWithoutConstructorFunctionTypeAlias
            { moduleName = "RecordWithoutConstructorFunction"
            , typeAliasName = "RecordWithoutConstructorFunction"
            }

-}
configDefault : Config
configDefault =
    { recordWithoutConstructorFunction =
        { moduleName = "RecordWithoutConstructorFunction"
        , typeAliasName = "RecordWithoutConstructorFunction"
        }
    }
        |> Config


{-| Configure what

    module Your.Module exposing (YourRecordWithoutConstructorFunction)

    type alias YourRecordWithoutConstructorFunction record =
        record

to import:

    import NoRecordAliasWithConstructor exposing (importRecordWithoutConstructorFunctionTypeAlias)

    NoRecordAliasWithConstructor.configDefault
        |> importRecordWithoutConstructorFunctionTypeAlias
            { moduleName = "Your.Module"
            , typeAliasName = "YourRecordWithoutConstructorFunction"
            }

[`configDefault`](#configDefault) imports
[`RecordWithoutConstructorFunction` from `lue-bird/elm-no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/):

    importRecordWithoutConstructorFunctionTypeAlias
        { moduleName = "RecordWithoutConstructorFunction"
        , typeAliasName = "RecordWithoutConstructorFunction"
        }

-}
importRecordWithoutConstructorFunctionTypeAlias :
    { moduleName : String, typeAliasName : String }
    -> Config
    -> Config
importRecordWithoutConstructorFunctionTypeAlias origin =
    alterConfig
        (\config ->
            { config
                | recordWithoutConstructorFunction = origin
            }
        )


alterConfig : (ConfigInternal -> ConfigInternal) -> Config -> Config
alterConfig alter =
    \(Config config) ->
        config |> alter |> Config



--


{-| 🔧`NoRecordAliasWithConstructor` forbids directly aliasing a record type.
As a consequence, its constructor function becomes unavailable.

Read more about the why in [`no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/).

Use [`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
when directly aliasing a record type to avoid its constructor function becoming unavailable.


## example

    type alias User =
        { name : String, age : Int }

will be marked as error and automatically fixed:

    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias User =
        RecordWithoutConstructorFunction
            { name : String, age : Int }


## configuration

    import NoRecordAliasWithConstructor

    config : List Rule
    config =
        [ NoRecordAliasWithConstructor.rule
        ]

[Defaults](#configDefault) can be altered by using [`ruleWith`](#ruleWith) [`Config`](#Config).

-}
rule : Rule
rule =
    ruleWith configDefault


{-| 🔧`NoRecordAliasWithConstructor` forbids directly aliasing a record type.
As a consequence, its constructor function becomes unavailable.

Read more about the why in [`no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/).

You can use [`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
when directly aliasing a record type to avoid its constructor function becoming unavailable.


## example

    type alias User =
        { name : String, age : Int }

will be marked as error and automatically fixed:

    import Util exposing (WithoutConstructorFunction)

    type alias User =
        WithoutConstructorFunction
            { name : String, age : Int }


## configuration

    import NoRecordAliasWithConstructor exposing (importRecordWithoutConstructorFunctionTypeAlias)

    config : List Rule
    config =
        [ NoRecordAliasWithConstructor.ruleWith
            (NoRecordAliasWithConstructor.configDefault
                |> importRecordWithoutConstructorFunctionTypeAlias
                    { moduleName = "Util"
                    , typeAliasName = "WithoutConstructorFunction"
                    }
            )
        ]

[`rule`](#rule) simply uses the [default](#configDefault) [`Config`](#Config).

-}
ruleWith : Config -> Rule
ruleWith config =
    let
        (Config { recordWithoutConstructorFunction }) =
            config

        initContext : Rule.ContextCreator () Context
        initContext =
            Rule.initContextCreator
                (\extractSourceCode () ->
                    { extractSourceCode = extractSourceCode
                    , recordWithoutConstructorFunctionImport =
                        RequiredNotImported
                            { importLocation = { column = 1, row = 2 } }
                    , findableDirectlyAliasedRecordRange = NotFound
                    }
                )
                |> Rule.withSourceCodeExtractor

        reviewDeclaration : Declaration -> Findable Range
        reviewDeclaration =
            \declaration ->
                case declaration of
                    AliasDeclaration typeAlias ->
                        case typeAlias.typeAnnotation of
                            Node recordTypeRange (Record _) ->
                                Found recordTypeRange

                            _ ->
                                NotFound

                    _ ->
                        NotFound

        importRecordWithoutConstructorFunctionAt : Location -> List Fix
        importRecordWithoutConstructorFunctionAt importLocation =
            [ Fix.insertAt importLocation
                (String.concat
                    [ "import "
                    , recordWithoutConstructorFunction.moduleName
                    , " exposing ("
                    , recordWithoutConstructorFunction.typeAliasName
                    , ")\n"
                    ]
                )
            ]

        reviewImport :
            { moduleName : String
            , range : Range
            , exposing_ : Maybe Exposing
            }
            -> RequiredImport
        reviewImport import_ =
            if import_.moduleName == recordWithoutConstructorFunction.moduleName then
                case import_.exposing_ of
                    Just (Exposing.All _) ->
                        RequiredImported

                    Just (Exposing.Explicit exposed) ->
                        if
                            List.any
                                (\(Node _ expose) ->
                                    expose == TypeOrAliasExpose recordWithoutConstructorFunction.typeAliasName
                                )
                                exposed
                        then
                            RequiredImported

                        else
                            RequiredNotImported { importLocation = import_.range.start }

                    _ ->
                        RequiredNotImported { importLocation = import_.range.start }

            else
                RequiredNotImported { importLocation = import_.range.start }

        fixModule { directlyAliasedRecord, extractSourceCode, recordWithoutConstructorFunctionImport } =
            let
                fixDirectRecordAlias : List Fix
                fixDirectRecordAlias =
                    [ Fix.replaceRangeBy
                        directlyAliasedRecord
                        (indentationLevel
                            ++ extractSourceCode directlyAliasedRecord
                            |> indentFurther
                        )
                    , Fix.insertAt
                        directlyAliasedRecord.start
                        (recordWithoutConstructorFunction.typeAliasName ++ "\n")
                    ]

                importIfNecessary =
                    case recordWithoutConstructorFunctionImport of
                        RequiredImported ->
                            []

                        RequiredNotImported { importLocation } ->
                            importRecordWithoutConstructorFunctionAt importLocation
            in
            fixDirectRecordAlias
                ++ importIfNecessary
    in
    Rule.newModuleRuleSchemaUsingContextCreator
        "NoRecordAliasWithConstructor"
        initContext
        |> Rule.withImportVisitor
            (\(Node importRange importSyntax) context ->
                ( []
                , { context
                    | recordWithoutConstructorFunctionImport =
                        reviewImport
                            { moduleName =
                                importSyntax.moduleName |> (moduleNameToString << Node.value)
                            , exposing_ =
                                importSyntax.exposingList |> Maybe.map Node.value
                            , range = importRange
                            }
                  }
                )
            )
        |> Rule.withCommentsVisitor
            (\comments context ->
                ( []
                , let
                    { extractSourceCode } =
                        context

                    moduleDocComment : Maybe (Node String)
                    moduleDocComment =
                        comments
                            |> List.filter (isDocComment << Node.value)
                            |> List.filter
                                (\(Node commentRange _) ->
                                    -- only keep the _module doc_ comment
                                    -- checking only the next line assumes `elm-format`ting
                                    extractSourceCode
                                        { start = after commentRange { column = 1 }
                                        , end = after commentRange { column = 5 }
                                        }
                                        /= "port"
                                )
                            |> List.head
                  in
                  case moduleDocComment of
                    Just (Node moduleCommentRange _) ->
                        { context
                            | recordWithoutConstructorFunctionImport =
                                RequiredNotImported
                                    { importLocation =
                                        after moduleCommentRange { column = 1 }
                                    }
                        }

                    Nothing ->
                        context
                )
            )
        |> Rule.withDeclarationEnterVisitor
            (\(Node _ declaration) context ->
                ( []
                , case context.findableDirectlyAliasedRecordRange of
                    NotFound ->
                        { context
                            | findableDirectlyAliasedRecordRange =
                                reviewDeclaration declaration
                        }

                    Found _ ->
                        context
                )
            )
        |> Rule.withFinalModuleEvaluation
            (\{ extractSourceCode, findableDirectlyAliasedRecordRange, recordWithoutConstructorFunctionImport } ->
                case findableDirectlyAliasedRecordRange of
                    Found directlyAliasedRecordRange ->
                        [ Rule.errorWithFix
                            errorInfo
                            directlyAliasedRecordRange
                            (fixModule
                                { directlyAliasedRecord = directlyAliasedRecordRange
                                , extractSourceCode = extractSourceCode
                                , recordWithoutConstructorFunctionImport = recordWithoutConstructorFunctionImport
                                }
                            )
                        ]

                    NotFound ->
                        []
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSourceCode : Range -> String
    , recordWithoutConstructorFunctionImport : RequiredImport
    , findableDirectlyAliasedRecordRange : Findable Range
    }


type Findable needle
    = Found needle
    | NotFound


type RequiredImport
    = RequiredImported
    | RequiredNotImported { importLocation : Location }


after : Range -> { column : Int } -> Location
after range { column } =
    { row = range.end.row + 1, column = column }
