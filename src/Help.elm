module Help exposing (ExposingInfo(..), ModuleInfo, functionsExposedFromImport, indentFurther, indentationLevel, isDocComment, moduleInfo, moduleNameToString, patternBindings, patternListBindings, putParensAround, reindent, subExpressions)

{-| Common helpers
-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Set exposing (Set)


{-| Get all immediate child expressions of an expression.
-}
subExpressions : Expression -> List (Node Expression)
subExpressions expression =
    case expression of
        LetExpression letBlock ->
            letBlock.declarations
                |> List.map Node.value
                |> List.map
                    (\letDeclaration ->
                        case letDeclaration of
                            LetFunction { declaration } ->
                                declaration |> Node.value |> .expression

                            LetDestructuring _ expression_ ->
                                expression_
                    )
                |> (::) letBlock.expression

        ListExpr expressions ->
            expressions

        TupledExpression expressions ->
            expressions

        RecordExpr fields ->
            fields |> List.map (\(Node _ ( _, value )) -> value)

        RecordUpdateExpression record updaters ->
            (record |> Node.map (FunctionOrValue []))
                :: (updaters |> List.map (\(Node _ ( _, newValue )) -> newValue))

        Application expressions ->
            expressions

        CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, expression_ ) -> expression_))

        OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        LambdaExpression lambda ->
            [ lambda.expression ]

        RecordAccess record _ ->
            [ record ]

        ParenthesizedExpression expression_ ->
            [ expression_ ]

        Negation expression_ ->
            [ expression_ ]

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


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    patterns
        |> List.map
            (\(Node _ pattern_) -> pattern_ |> patternBindings)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


{-| Recursively find all bindings in a pattern.
-}
patternBindings : Pattern -> Set String
patternBindings pattern =
    case pattern of
        ListPattern patterns ->
            patternListBindings patterns

        TuplePattern patterns ->
            patternListBindings patterns

        RecordPattern patterns ->
            Set.fromList (List.map Node.value patterns)

        NamedPattern _ patterns ->
            patternListBindings patterns

        UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (patternBindings tailPattern) (patternBindings headPattern)

        VarPattern name ->
            Set.singleton name

        AsPattern (Node _ pattern_) (Node _ name) ->
            Set.insert name (patternBindings pattern_)

        ParenthesizedPattern (Node _ inParens) ->
            patternBindings inParens

        AllPattern ->
            Set.empty

        UnitPattern ->
            Set.empty

        CharPattern _ ->
            Set.empty

        StringPattern _ ->
            Set.empty

        IntPattern _ ->
            Set.empty

        HexPattern _ ->
            Set.empty

        FloatPattern _ ->
            Set.empty


type ExposingInfo
    = ExposingAll
    | ExposingExplicit (List String)


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
                            (list
                                |> List.filterMap
                                    (\(Node _ expose) ->
                                        case expose of
                                            TypeOrAliasExpose name ->
                                                Just name

                                            _ ->
                                                Nothing
                                    )
                            )
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


groupExposingList : List TopLevelExpose -> List String
groupExposingList exposingList =
    exposingList
        |> List.foldl
            (\exposed groups ->
                case exposed of
                    FunctionExpose function ->
                        groups |> (::) function

                    InfixExpose _ ->
                        groups

                    TypeOrAliasExpose _ ->
                        groups

                    TypeExpose _ ->
                        groups
            )
            []


moduleNameToString : List String -> String
moduleNameToString =
    \moduleName ->
        moduleName |> String.join "."


isDocComment : String -> Bool
isDocComment =
    \comment ->
        comment |> String.startsWith "{-|"


{-| Re-indent a section of generated code to ensure that it doesn't cause issues
when used as a fix.
-}
reindent : Int -> String -> String
reindent amount =
    let
        indent : String
        indent =
            String.repeat (amount - 1) " "
    in
    String.lines
        >> List.map
            (\l ->
                -- Don't indent empty lines
                if String.isEmpty l then
                    l

                else
                    indent ++ l
            )
        >> String.join "\n"
        >> String.trimLeft


indentFurther : String -> String
indentFurther =
    \code ->
        code
            |> String.lines
            |> List.map (\codeLine -> indentationLevel ++ codeLine)
            |> String.join "\n"


indentationLevel : String
indentationLevel =
    "    "


{-| `Gen.parens` is ignored when printing, so the parens are put around manually.
-}
putParensAround : String -> String
putParensAround string =
    "(" ++ string ++ ")"
