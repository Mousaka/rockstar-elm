module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)
import Set


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { codeText = ""
    , result = ""
    , ast = Ok (Val F)
    }


type alias Model =
    { codeText : String
    , result : String
    , ast : Result String Term
    }


type Msg
    = ParseButtonClicked
    | CodeTextUpdated String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseButtonClicked ->
            { model | ast = parse model.codeText |> Result.mapError deadEndsToString }

        CodeTextUpdated text ->
            { model | codeText = text }


type Value
    = Boolean Bool
    | Numeric Int
    | Error String


type Val
    = T
    | F


type Term
    = IfExpr Term Term Term
    | Val Val
    | Variable String Val



{--
Case-insensitive token parser
Credit for this function goes to dmy
https://discourse.elm-lang.org/t/solved-elm-parser-parsing-keyword-in-a-case-insensitive-way/3169/4
--}


iToken : String -> Parser ()
iToken token =
    let
        iTokenHelp : String -> Parser (Step String ())
        iTokenHelp chars =
            case String.uncons chars of
                Just ( char, remainingChars ) ->
                    oneOf
                        [ succeed (Loop remainingChars)
                            |. chompIf (\c -> Char.toLower c == char)
                        , problem ("Expected case insensitive \"" ++ chars ++ "\"")
                        ]

                Nothing ->
                    succeed <| Done ()
    in
    backtrackable (loop token iTokenHelp)


simpleVariableName : Parser String
simpleVariableName =
    succeed identity
        |= variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlpha c
            , reserved = Set.fromList [ "let" ]
            }


valParser : Parser Val
valParser =
    oneOf
        [ true
        , false
        ]


simpleVariableHelper : Parser ( String, Val )
simpleVariableHelper =
    succeed (\name value -> ( String.toLower name, value ))
        |= simpleVariableName
        |. symbol " "
        |. symbol "is"
        |. symbol " "
        |= valParser


commonVariableKeyword : Parser String
commonVariableKeyword =
    getChompedString <|
        succeed identity
            |= oneOf
                [ iToken "an"
                , iToken "a"
                , iToken "the"
                , iToken "my"
                , iToken "your"
                ]


commonVariable : Parser Term
commonVariable =
    succeed (\keyword ( name, value ) -> Variable (String.toLower <| keyword ++ " " ++ name) value)
        |= commonVariableKeyword
        |. symbol " "
        |= simpleVariableHelper


isAsEmptyString : Parser String
isAsEmptyString =
    succeed (always "")
        |= symbol "is"


properVariableName : Parser String
properVariableName =
    succeed
        (\firstPart secondPart ->
            if secondPart == "" then
                firstPart

            else
                firstPart ++ " " ++ secondPart
        )
        |= variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlpha c
            , reserved = Set.fromList [ "let" ]
            }
        |. symbol " "
        |= oneOf [ isAsEmptyString, lazy (\_ -> properVariableName) ]
        |> map String.toLower


properVariable : Parser Term
properVariable =
    succeed (\name value -> Variable name value)
        |= properVariableName
        |. symbol " "
        |= valParser


true : Parser Val
true =
    succeed T
        |. oneOf
            [ symbol "true"
            , symbol "right"
            , symbol "ok"
            , symbol "yes"
            ]


false : Parser Val
false =
    succeed F
        |. oneOf
            [ symbol "false"
            , symbol "no"
            , symbol "lies"
            , symbol "wrong"
            ]


simpleVariable =
    simpleVariableHelper |> Parser.map (\( name, value ) -> Variable name value)


termP : Parser Term
termP =
    succeed identity
        |. spaces
        |= oneOf
            [ valParser |> Parser.map Val
            , commonVariable
            , backtrackable simpleVariable
            , properVariable
            ]


parse : String -> Result (List Parser.DeadEnd) Term
parse codeText =
    Parser.run termP codeText


eval : Term -> Value
eval term =
    Error "not implemented"


view model =
    div [ style "padding" "50px" ]
        [ button [ onClick ParseButtonClicked ] [ text "Parse" ]
        , div [ style "width" "600px", style "height" "1000px" ]
            [ textarea [ style "width" "100%", style "height" "100%", onInput CodeTextUpdated ] [] ]
        ]
