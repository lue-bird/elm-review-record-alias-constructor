module NoRecordAliasConstructor exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.CodeGen as Gen
import Elm.Pretty as GenPretty
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as Type
import Elm.Type as TypeMetadata
import Help exposing (ExposingInfo(..), allBindingsInPattern, functionsExposedFromImport, moduleInfo, putParensAround, reindent, subExpressions)
import NoRecordAliasConstructor.Common exposing (errorInfo)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency
import Review.Rule as Rule exposing (ModuleKey, Rule)


{-| 🔧`NoRecordAliasConstructor` forbids using a record type alias constructor function.

Read more about the why in [`no-record-type-alias-constructor-function`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/).


## examples

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


## configuration

    import NoRecordAliasConstructor

    config : List Rule
    config =
        [ NoRecordAliasConstructor.rule
        ]

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoRecordAliasConstructor"
        { aliases = []
        , constructorUses = []
        , modulesExposingAll = Dict.empty
        }
        |> Rule.providesFixesForProjectRule
        |> Rule.withDependenciesProjectVisitor
            (\dependencies context ->
                ( []
                , let
                    modules =
                        dependencies
                            |> Dict.values
                            |> List.concatMap Dependency.modules

                    moduleNameParts : String -> ModuleName
                    moduleNameParts =
                        String.split "."
                  in
                  { context
                    | modulesExposingAll =
                        context.modulesExposingAll
                            |> Dict.union
                                (modules
                                    |> List.map
                                        (\{ name, values } ->
                                            ( name |> moduleNameParts
                                            , { functions = values |> List.map .name }
                                            )
                                        )
                                    |> Dict.fromList
                                )
                    , aliases =
                        modules
                            |> List.concatMap
                                (\module_ ->
                                    module_.aliases
                                        |> List.filterMap
                                            (\alias ->
                                                case alias.tipe of
                                                    TypeMetadata.Record fields Nothing ->
                                                        { moduleName = module_.name |> moduleNameParts
                                                        , name = alias.name
                                                        , recordFields =
                                                            fields
                                                                |> List.map (\( fieldName, _ ) -> fieldName)
                                                        }
                                                            |> Just

                                                    _ ->
                                                        Nothing
                                            )
                                )
                  }
                )
            )
        |> Rule.withModuleVisitor
            (Rule.withModuleDefinitionVisitor
                (\(Node _ module_) context ->
                    ( []
                    , let
                        { moduleName, exposing_ } =
                            moduleInfo module_
                      in
                      { context
                        | moduleName = moduleName
                        , exposing_ = exposing_
                      }
                    )
                )
                >> Rule.withImportVisitor
                    (\(Node _ import_) context ->
                        ( []
                        , case import_.exposingList of
                            Just (Node _ exposing_) ->
                                case functionsExposedFromImport exposing_ of
                                    ExposingAll ->
                                        { context
                                            | allImportedModules =
                                                context.allImportedModules
                                                    |> (::) (import_.moduleName |> Node.value)
                                        }

                                    ExposingExplicit { functions } ->
                                        { context
                                            | explicitlyImportedFunctions =
                                                context.explicitlyImportedFunctions
                                                    ++ functions
                                        }

                            Nothing ->
                                context
                        )
                    )
                >> Rule.withDeclarationEnterVisitor
                    (\(Node _ declaration) context ->
                        ( []
                        , let
                            contextWithThisDeclaration =
                                case declaration of
                                    FunctionDeclaration fun ->
                                        let
                                            (Node _ name) =
                                                fun.declaration |> Node.value |> .name
                                        in
                                        { context
                                            | topLevelFunctions =
                                                context.topLevelFunctions
                                                    |> (::) name
                                        }

                                    _ ->
                                        context
                          in
                          case declaration of
                            AliasDeclaration alias ->
                                visitDeclarationForRecordAlias alias contextWithThisDeclaration

                            FunctionDeclaration fun ->
                                { contextWithThisDeclaration
                                    | functionsAndValues =
                                        let
                                            (Node _ implementation) =
                                                fun.declaration
                                        in
                                        context.functionsAndValues
                                            ++ collectFunctions
                                                { bindingsInScope =
                                                    implementation.arguments
                                                        |> List.concatMap
                                                            (\(Node _ pattern) -> pattern |> allBindingsInPattern)
                                                }
                                                implementation.expression
                                                context
                                }

                            _ ->
                                contextWithThisDeclaration
                        )
                    )
            )
        |> Rule.withModuleContextUsingContextCreator translateContexts
        |> Rule.withFinalProjectEvaluation checkEverything
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { modulesExposingAll :
        Dict ModuleName { functions : List String }
    , aliases :
        List
            { name : String
            , moduleName : ModuleName
            , recordFields : List String
            }
    , constructorUses :
        List
            { moduleKey : ModuleKey
            , at :
                List
                    { name : String
                    , moduleName : ModuleName
                    , arguments : List Expression
                    , range : Range
                    , functionsInScope : List String
                    }
            , allImportedModules : List ModuleName
            }
    }


type alias ModuleContext =
    { moduleName : ModuleName
    , exposing_ : ExposingInfo
    , topLevelFunctions : List String
    , explicitlyImportedFunctions : List String
    , allImportedModules : List ModuleName
    , aliases :
        List
            { name : String
            , recordFields : List String
            }
    , functionsAndValues :
        List
            { name : String
            , moduleName : ModuleName
            , arguments : List Expression
            , bindingsInScope : List String
            , range : Range
            }
    , moduleNameLookupTable : ModuleNameLookupTable
    }


translateContexts :
    { fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
    , fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
    , foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
    }
translateContexts =
    { fromProjectToModule = projectToModuleContext
    , fromModuleToProject = moduleToProjectContext
    , foldProjectContexts =
        \a b ->
            { aliases = a.aliases ++ b.aliases
            , constructorUses =
                a.constructorUses ++ b.constructorUses
            , modulesExposingAll =
                a.modulesExposingAll
                    |> Dict.union b.modulesExposingAll
            }
    }


projectToModuleContext : Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContext =
    Rule.initContextCreator
        (\lookupTable _ ->
            -- many dummy values
            { moduleName = []
            , exposing_ = ExposingAll
            , explicitlyImportedFunctions = []
            , topLevelFunctions = []
            , allImportedModules = []
            , aliases = []
            , functionsAndValues = []
            , moduleNameLookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


moduleToProjectContext : Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContext =
    Rule.initContextCreator
        (\moduleKey moduleContext ->
            { aliases =
                moduleContext.aliases
                    |> List.map
                        (\{ name, recordFields } ->
                            { name = name
                            , recordFields = recordFields
                            , moduleName = moduleContext.moduleName
                            }
                        )
            , constructorUses =
                [ { moduleKey = moduleKey
                  , allImportedModules = moduleContext.allImportedModules
                  , at =
                        moduleContext.functionsAndValues
                            |> List.map
                                (\fun ->
                                    { name = fun.name
                                    , moduleName = fun.moduleName
                                    , arguments = fun.arguments
                                    , functionsInScope =
                                        [ moduleContext.explicitlyImportedFunctions
                                        , moduleContext.topLevelFunctions
                                        , fun.bindingsInScope
                                        ]
                                            |> List.concat
                                    , range = fun.range
                                    }
                                )
                  }
                ]
            , modulesExposingAll =
                case moduleContext.exposing_ of
                    ExposingAll ->
                        Dict.singleton moduleContext.moduleName
                            { functions = moduleContext.topLevelFunctions }

                    ExposingExplicit _ ->
                        Dict.empty
            }
        )
        |> Rule.withModuleKey


visitDeclarationForRecordAlias : TypeAlias -> ModuleContext -> ModuleContext
visitDeclarationForRecordAlias alias context =
    let
        name =
            alias.name |> Node.value
    in
    case alias.typeAnnotation |> Node.value of
        Type.Record fields ->
            let
                recordAlias =
                    { name = name
                    , recordFields =
                        fields
                            |> List.map
                                (\(Node _ ( Node _ field, _ )) -> field)
                    }
            in
            { context
                | aliases =
                    context.aliases
                        |> (::) recordAlias
            }

        _ ->
            context


collectFunctions :
    { bindingsInScope : List String }
    -> Node Expression
    -> ModuleContext
    ->
        List
            { name : String
            , moduleName : ModuleName
            , arguments : List Expression
            , bindingsInScope : List String
            , range : Range
            }
collectFunctions { bindingsInScope } expressionNode context =
    let
        (Node expressionRange expression) =
            expressionNode

        moduleNameAt : Range -> Maybe ModuleName
        moduleNameAt range =
            ModuleNameLookupTable.moduleNameAt
                context.moduleNameLookupTable
                range
                |> Maybe.map
                    (\moduleName ->
                        case moduleName of
                            [] ->
                                context.moduleName

                            _ :: _ ->
                                moduleName
                    )

        goWith :
            List (Node Pattern)
            -> List (Node Expression)
            ->
                List
                    { name : String
                    , moduleName : ModuleName
                    , arguments : List Expression
                    , bindingsInScope : List String
                    , range : Range
                    }
        goWith newFunctions expressions =
            expressions
                |> List.concatMap
                    (\expressionToGoTo ->
                        collectFunctions
                            { bindingsInScope =
                                bindingsInScope
                                    ++ (newFunctions
                                            |> List.concatMap
                                                (\(Node _ pattern) -> pattern |> allBindingsInPattern)
                                       )
                            }
                            expressionToGoTo
                            context
                    )

        step expressions =
            goWith [] expressions
    in
    case expression of
        Expression.Application ((Node nameRange (Expression.FunctionOrValue _ name)) :: arguments) ->
            case moduleNameAt nameRange of
                Just moduleName ->
                    step arguments
                        ++ [ { name = name
                             , moduleName = moduleName
                             , arguments = arguments |> List.map Node.value
                             , bindingsInScope =
                                bindingsInScope
                             , range = expressionRange
                             }
                           ]

                Nothing ->
                    step arguments

        Expression.FunctionOrValue _ name ->
            case moduleNameAt expressionRange of
                Just moduleName ->
                    [ { name = name
                      , moduleName = moduleName
                      , arguments = []
                      , range = expressionRange
                      , bindingsInScope =
                            bindingsInScope
                      }
                    ]

                Nothing ->
                    []

        Expression.LetExpression letBlock ->
            let
                declarations =
                    letBlock.declarations
                        |> List.map Node.value

                bindingsInLetDeclarations =
                    declarations
                        |> List.map
                            (\declaration ->
                                case declaration of
                                    LetFunction fun ->
                                        fun.declaration
                                            |> Node.value
                                            |> .name
                                            |> Node.map Gen.varPattern

                                    LetDestructuring pattern _ ->
                                        pattern
                            )

                stepFromLetBlockWith newBindings expressions =
                    expressions
                        |> goWith
                            (bindingsInLetDeclarations
                                ++ newBindings
                            )
            in
            [ [ letBlock.expression ] |> stepFromLetBlockWith []
            , declarations
                |> List.concatMap
                    (\declaration ->
                        case declaration of
                            LetFunction fun ->
                                let
                                    funDeclaration =
                                        fun.declaration |> Node.value
                                in
                                [ funDeclaration.expression ]
                                    |> stepFromLetBlockWith funDeclaration.arguments

                            LetDestructuring pattern implementation ->
                                [ implementation ]
                                    |> stepFromLetBlockWith [ pattern ]
                    )
            ]
                |> List.concat

        Expression.CaseExpression caseBlock ->
            step [ caseBlock.expression ]
                ++ (caseBlock.cases
                        |> List.concatMap
                            (\( pattern, expr ) ->
                                [ expr ] |> goWith [ pattern ]
                            )
                   )

        Expression.LambdaExpression lambda ->
            [ lambda.expression ]
                |> goWith lambda.args

        _ ->
            (expressionNode |> Node.value |> subExpressions) |> step


checkEverything : ProjectContext -> List (Rule.Error { useErrorForModule : () })
checkEverything context =
    context.constructorUses
        |> List.concatMap
            (\{ moduleKey, at, allImportedModules } ->
                let
                    exposingAllImportedFunctions : List String
                    exposingAllImportedFunctions =
                        allImportedModules
                            |> List.filterMap
                                (\import_ ->
                                    Dict.get import_ context.modulesExposingAll
                                )
                            |> List.concatMap .functions
                in
                context.aliases
                    |> List.concatMap
                        (\alias ->
                            at
                                |> List.filter
                                    (\use ->
                                        (alias.moduleName == use.moduleName)
                                            && (alias.name == use.name)
                                    )
                                |> List.concatMap
                                    (\use ->
                                        let
                                            curriedFields : List String
                                            curriedFields =
                                                alias.recordFields
                                                    |> List.drop (use.arguments |> List.length)

                                            functionsInScope =
                                                use.functionsInScope
                                                    ++ exposingAllImportedFunctions
                                        in
                                        if
                                            curriedFields
                                                |> List.any
                                                    (\field ->
                                                        List.member field functionsInScope
                                                    )
                                        then
                                            [ Rule.errorForModule moduleKey
                                                errorInfo
                                                use.range
                                            ]

                                        else
                                            [ Rule.errorForModuleWithFix moduleKey
                                                errorInfo
                                                use.range
                                                [ let
                                                    record =
                                                        Gen.record
                                                            (List.map2
                                                                (\field value -> ( field, value ))
                                                                alias.recordFields
                                                                (use.arguments
                                                                    ++ (curriedFields |> List.map Gen.val)
                                                                )
                                                            )
                                                  in
                                                  Fix.replaceRangeBy use.range
                                                    (case curriedFields of
                                                        [] ->
                                                            record
                                                                |> GenPretty.prettyExpression
                                                                |> pretty 100
                                                                |> reindent use.range.start.column

                                                        _ ->
                                                            Gen.lambda
                                                                (curriedFields |> List.map Gen.varPattern)
                                                                record
                                                                |> GenPretty.prettyExpression
                                                                |> pretty 100
                                                                |> reindent use.range.start.column
                                                                |> putParensAround
                                                    )
                                                ]
                                            ]
                                    )
                        )
            )
