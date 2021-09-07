module Util exposing (ExposingInfo(..), ModuleInfo, allBindingsInPattern, functionsExposedFromImport, moduleInfo, subexpressions)

{-| Helpers
-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Node Expression -> List (Node Expression)
subexpressions e =
    case Node.value e of
        LetExpression letBlock ->
            let
                subExprs : Node LetDeclaration -> Node Expression
                subExprs n =
                    case Node.value n of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression

                        LetDestructuring _ expr ->
                            expr
            in
            letBlock.expression
                :: List.map subExprs letBlock.declarations

        ListExpr exprs ->
            exprs

        TupledExpression exprs ->
            exprs

        RecordExpr setters ->
            List.map (Tuple.second << Node.value) setters

        RecordUpdateExpression record updaters ->
            Node.map (FunctionOrValue []) record
                :: List.map (Tuple.second << Node.value) updaters

        Application exprs ->
            exprs

        CaseExpression caseBlock ->
            caseBlock.expression
                :: List.map Tuple.second caseBlock.cases

        OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        IfBlock predExpr thenExpr elseExpr ->
            [ predExpr, thenExpr, elseExpr ]

        LambdaExpression { expression } ->
            [ expression ]

        RecordAccess record _ ->
            [ record ]

        ParenthesizedExpression expr ->
            [ expr ]

        Negation expr ->
            [ expr ]

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        GLSLExpression _ ->
            []

        RecordAccessFunction _ ->
            []

        FunctionOrValue _ _ ->
            []

        Operator _ ->
            []

        PrefixOperator _ ->
            []


{-| Recursively find all bindings in a pattern.
-}
allBindingsInPattern : Node Pattern -> List String
allBindingsInPattern pattern =
    let
        go : List (Node Pattern) -> List String
        go =
            List.concatMap allBindingsInPattern
    in
    case Node.value pattern of
        ListPattern patterns ->
            go patterns

        TuplePattern patterns ->
            go patterns

        RecordPattern patterns ->
            patterns |> List.map Node.value

        NamedPattern _ patterns ->
            go patterns

        UnConsPattern headPattern tailPattern ->
            go [ headPattern, tailPattern ]

        VarPattern name ->
            [ name ]

        AsPattern asedPattern (Node _ name) ->
            name :: go [ asedPattern ]

        ParenthesizedPattern p ->
            go [ p ]

        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []


type ExposingInfo
    = ExposingAll
    | ExposingExplicit { functions : List String }


type alias ModuleInfo =
    { moduleName : List String
    , exposing_ : ExposingInfo
    }


moduleInfo : Module -> ModuleInfo
moduleInfo module_ =
    let
        extract { moduleName, exposingList } =
            case exposingList |> Node.value of
                Exposing.All _ ->
                    { moduleName = moduleName |> Node.value
                    , exposing_ = ExposingAll
                    }

                Exposing.Explicit list ->
                    { moduleName = moduleName |> Node.value
                    , exposing_ =
                        ExposingExplicit
                            { functions =
                                list
                                    |> List.filterMap
                                        (\(Node _ expose) ->
                                            case expose of
                                                TypeOrAliasExpose name ->
                                                    Just name

                                                _ ->
                                                    Nothing
                                        )
                            }
                    }
    in
    case module_ of
        NormalModule data ->
            extract data

        PortModule data ->
            extract data

        EffectModule data ->
            extract data


functionsExposedFromImport : Exposing -> ExposingInfo
functionsExposedFromImport expose =
    case expose of
        Exposing.Explicit list ->
            ExposingExplicit
                (list |> List.map Node.value |> groupExposingList)

        Exposing.All _ ->
            ExposingAll


groupExposingList : List TopLevelExpose -> { functions : List String }
groupExposingList exposingList =
    exposingList
        |> List.foldl
            (\exposed groups ->
                case exposed of
                    FunctionExpose function ->
                        { groups
                            | functions =
                                groups.functions |> (::) function
                        }

                    InfixExpose _ ->
                        groups

                    TypeOrAliasExpose _ ->
                        groups

                    TypeExpose _ ->
                        groups
            )
            { functions = [] }
