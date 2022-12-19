module NoRecordAliasConstructor exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.CodeGen as Gen
import Elm.Docs
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
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


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
        { modulesRecordTypeAliases = Dict.empty
        , modulesExposingAll = Dict.empty
        }
        |> Rule.providesFixesForProjectRule
        |> Rule.withDependenciesProjectVisitor
            (\dependencies context ->
                ( []
                , let
                    modules : List Elm.Docs.Module
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
                                            , values |> List.map .name |> Set.fromList
                                            )
                                        )
                                    |> Dict.fromList
                                )
                    , modulesRecordTypeAliases =
                        modules
                            |> List.map
                                (\module_ ->
                                    ( module_.name |> moduleNameParts
                                    , module_.aliases
                                        |> List.filterMap
                                            (\alias ->
                                                case alias.tipe of
                                                    TypeMetadata.Record fields Nothing ->
                                                        ( alias.name
                                                        , { recordFields =
                                                                fields
                                                                    |> List.map (\( fieldName, _ ) -> fieldName)
                                                          }
                                                        )
                                                            |> Just

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Dict.fromList
                                    )
                                )
                            |> Dict.fromList
                  }
                )
            )
        |> Rule.withContextFromImportedModules
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
                                            | importedModulesExposingAll =
                                                context.importedModulesExposingAll
                                                    |> Set.insert (import_.moduleName |> Node.value)
                                        }

                                    ExposingExplicit exposedExplicitly ->
                                        { context
                                            | importedExplicitly =
                                                context.importedExplicitly
                                                    |> Set.union (exposedExplicitly |> Set.fromList)
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
                                            | moduleLevelBindings =
                                                context.moduleLevelBindings
                                                    |> Set.insert name
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
                                                        |> Set.fromList
                                                }
                                                implementation.expression
                                                context
                                }

                            _ ->
                                contextWithThisDeclaration
                        )
                    )
                >> Rule.withFinalModuleEvaluation
                    moduleReport
            )
        |> Rule.withModuleContextUsingContextCreator translateContexts
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { modulesExposingAll :
        Dict ModuleName (Set String)
    , modulesRecordTypeAliases :
        Dict
            ModuleName
            (Dict String { recordFields : List String })
    }


type alias ModuleContext =
    { modulesExposingAll : Dict ModuleName (Set String)
    , moduleNameLookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , exposing_ : ExposingInfo
    , moduleLevelBindings : Set String
    , importedExplicitly : Set String
    , importedModulesExposingAll : Set ModuleName
    , importedModulesRecordTypeAliases :
        Dict
            ModuleName
            (Dict String { recordFields : List String })
    , moduleLevelRecordTypeAliases :
        Dict String { recordFields : List String }
    , functionsAndValues :
        List
            { name : String
            , moduleName : ModuleName
            , arguments : List Expression
            , bindingsInScope : Set String
            , range : Range
            }
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
            { modulesRecordTypeAliases = a.modulesRecordTypeAliases |> Dict.union b.modulesRecordTypeAliases
            , modulesExposingAll =
                Dict.union a.modulesExposingAll b.modulesExposingAll
            }
    }


projectToModuleContext : Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContext =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { importedExplicitly = Set.empty
            , moduleLevelBindings = Set.empty
            , moduleLevelRecordTypeAliases = Dict.empty
            , importedModulesRecordTypeAliases = projectContext.modulesRecordTypeAliases
            , functionsAndValues = []
            , importedModulesExposingAll = Set.empty
            , moduleNameLookupTable = lookupTable
            , modulesExposingAll = projectContext.modulesExposingAll

            -- dummy values. sad
            , moduleName = []
            , exposing_ = ExposingAll
            }
        )
        |> Rule.withModuleNameLookupTable


moduleToProjectContext : Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContext =
    Rule.initContextCreator
        (\moduleContext ->
            { modulesRecordTypeAliases =
                Dict.singleton
                    moduleContext.moduleName
                    moduleContext.moduleLevelRecordTypeAliases
            , modulesExposingAll =
                case moduleContext.exposing_ of
                    ExposingAll ->
                        Dict.singleton
                            moduleContext.moduleName
                            moduleContext.moduleLevelBindings

                    ExposingExplicit _ ->
                        Dict.empty
            }
        )


visitDeclarationForRecordAlias :
    TypeAlias
    -> (ModuleContext -> ModuleContext)
visitDeclarationForRecordAlias alias context =
    let
        name =
            alias.name |> Node.value
    in
    case alias.typeAnnotation |> Node.value of
        Type.Record fields ->
            { context
                | moduleLevelRecordTypeAliases =
                    context.moduleLevelRecordTypeAliases
                        |> Dict.insert name
                            { recordFields =
                                fields
                                    |> List.map
                                        (\(Node _ ( Node _ field, _ )) -> field)
                            }
            }

        _ ->
            context


collectFunctions :
    { bindingsInScope : Set String }
    -> Node Expression
    ->
        (ModuleContext
         ->
            List
                { name : String
                , moduleName : ModuleName
                , arguments : List Expression
                , bindingsInScope : Set String
                , range : Range
                }
        )
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
                    , bindingsInScope : Set String
                    , range : Range
                    }
        goWith newFunctions expressions =
            expressions
                |> List.concatMap
                    (\expressionToGoTo ->
                        collectFunctions
                            { bindingsInScope =
                                bindingsInScope
                                    |> Set.union
                                        (newFunctions
                                            |> List.concatMap
                                                (\(Node _ pattern) -> pattern |> allBindingsInPattern)
                                            |> Set.fromList
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
                             , bindingsInScope = bindingsInScope
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


moduleReport : ModuleContext -> List (Rule.Error {})
moduleReport context =
    let
        exposingAllImportedFunctions : Set String
        exposingAllImportedFunctions =
            context.importedModulesExposingAll
                |> Set.toList
                |> List.filterMap
                    (\importedModuleExposingAll ->
                        context.modulesExposingAll |> Dict.get importedModuleExposingAll
                    )
                |> List.foldl Set.union Set.empty
    in
    context.importedModulesRecordTypeAliases
        |> Dict.insert context.moduleName
            context.moduleLevelRecordTypeAliases
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, moduleLevelRecordTypeAliases ) ->
                moduleLevelRecordTypeAliases
                    |> Dict.toList
                    |> List.map
                        (\( name, moduleLevelRecordTypeAlias ) ->
                            { moduleName = moduleName
                            , name = name
                            , recordFields = moduleLevelRecordTypeAlias.recordFields
                            }
                        )
            )
        |> List.concatMap
            (\alias ->
                context.functionsAndValues
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

                                reservedInScope : Set String
                                reservedInScope =
                                    use.bindingsInScope
                                        |> Set.union exposingAllImportedFunctions
                                        |> Set.union context.importedExplicitly
                                        |> Set.union context.moduleLevelBindings
                            in
                            if
                                curriedFields
                                    |> List.any
                                        (\field -> Set.member field reservedInScope)
                            then
                                [ Rule.error errorInfo use.range ]

                            else
                                [ Rule.errorWithFix
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
