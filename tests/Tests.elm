module Tests exposing (..)

import Expect
import Fuzz exposing (Fuzzer, constant)
import Main exposing (Term(..), Val(..), Value(..), parse, simpleVariable)
import Parser as Praser
import Random
import Shrink
import Test exposing (Test, describe, fuzz, test)


truthStringFuzzer : Fuzzer String
truthStringFuzzer =
    Fuzz.oneOf
        [ constant "true"
        , constant "right"
        , constant "yes"
        , constant "ok"
        ]


falseStringFuzzer : Fuzzer String
falseStringFuzzer =
    Fuzz.oneOf
        [ constant "false"
        , constant "no"
        , constant "lies"
        , constant "wrong"
        ]


commonVariableFuzzer : Fuzzer ( String, String, Val )
commonVariableFuzzer =
    Fuzz.map2
        (\shouldStartWithUpper keyword ->
            ( (if shouldStartWithUpper then
                (keyword |> String.left 1 |> String.toUpper) ++ String.dropLeft 1 keyword

               else
                keyword
              )
                ++ " thing is right"
            , keyword ++ " thing"
            , T
            )
        )
        Fuzz.bool
        (Fuzz.oneOf
            [ constant "a"
            , constant "an"
            , constant "the"
            , constant "my"
            , constant "your"
            ]
        )


alphaFuzzer : Fuzzer Char
alphaFuzzer =
    let
        mapper : Char -> Bool -> Char
        mapper char upper =
            if upper then
                Char.toUpper char

            else
                char
    in
    Fuzz.map2 mapper
        (Fuzz.oneOf
            [ Fuzz.constant 'a'
            , Fuzz.constant 'b'
            , Fuzz.constant 'c'
            , Fuzz.constant 'd'
            , Fuzz.constant 'e'
            , Fuzz.constant 'f'
            , Fuzz.constant 'g'
            , Fuzz.constant 'h'
            , Fuzz.constant 'i'
            , Fuzz.constant 'j'
            , Fuzz.constant 'k'
            , Fuzz.constant 'l'
            , Fuzz.constant 'm'
            , Fuzz.constant 'n'
            , Fuzz.constant 'o'
            , Fuzz.constant 'p'
            , Fuzz.constant 'q'
            , Fuzz.constant 'r'
            , Fuzz.constant 's'
            , Fuzz.constant 't'
            , Fuzz.constant 'u'
            , Fuzz.constant 'v'
            , Fuzz.constant 'w'
            , Fuzz.constant 'x'
            , Fuzz.constant 'y'
            , Fuzz.constant 'z'
            ]
        )
        Fuzz.bool


alph : Bool -> Int -> Char
alph lower n =
    let
        normalized =
            remainderBy 26 n
                + 65
                |> Char.fromCode
    in
    if lower then
        Char.toLower normalized

    else
        normalized


atilla : List (List Int) -> List (List Int)
atilla =
    identity


properVariableNameFuzzer : Fuzzer String
properVariableNameFuzzer =
    Fuzz.custom
        (Random.int 1 10
            |> Random.andThen
                (\nOfWords ->
                    -- 5 words
                    Random.list nOfWords
                        (Random.int 1 20
                            |> Random.andThen
                                (\wordLen -> Random.list wordLen (Random.int 0 25))
                        )
                )
            |> Random.map atilla
            |> Random.map
                (\wordsAsChars ->
                    wordsAsChars
                        |> List.foldl
                            (\chars acc ->
                                let
                                    stuff =
                                        chars
                                            |> List.indexedMap
                                                (\index charCode ->
                                                    if index == 0 then
                                                        alph False charCode

                                                    else
                                                        alph True charCode
                                                )
                                            |> String.fromList
                                in
                                if acc == "" then
                                    stuff

                                else
                                    String.join " " [ acc, stuff ]
                            )
                            ""
                )
        )
        Shrink.noShrink



-- Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


suite : Test
suite =
    describe "Parser"
        [ describe "Boolean values"
            [ fuzz truthStringFuzzer "true values" <|
                \truthString ->
                    truthString
                        |> parse
                        |> Expect.equal (Ok (Val T))
            , fuzz falseStringFuzzer "false values" <|
                \faleString ->
                    faleString
                        |> parse
                        |> Expect.equal (Ok (Val F))
            ]
        , describe "number values"
            [ fuzz Fuzz.float "Parse number values" <|
                \number ->
                    number
                        |> String.fromFloat
                        |> parse
                        |> Expect.equal (Ok (Val (Number number)))
            ]
        , describe "Variables"
            [ test "Simple variable func" <|
                \_ ->
                    "Cool is right"
                        |> Praser.run simpleVariable
                        |> Expect.equal (Ok (Variable "cool" T))
            , test "Simple variable" <|
                \_ ->
                    "Cool is right"
                        |> parse
                        |> Expect.equal (Ok (Variable "cool" T))
            , fuzz commonVariableFuzzer "Common variables" <|
                \( textToParse, expectedName, expectedValue ) ->
                    textToParse
                        |> parse
                        |> Expect.equal (Ok (Variable expectedName expectedValue))
            , test "Proper Two word variable" <|
                \_ ->
                    "Floor Jansen is right"
                        |> parse
                        |> Expect.equal (Ok (Variable "floor jansen" T))
            , test "Proper Three word variable" <|
                \_ ->
                    "Jonathan III Rowlins is 1.1"
                        |> parse
                        |> Expect.equal (Ok (Variable "jonathan iii rowlins" (Number 1.1)))
            , fuzz properVariableNameFuzzer "Proper variables" <|
                \name ->
                    name
                        ++ " is lies"
                        |> parse
                        |> Expect.equal (Ok (Variable (String.toLower name) F))
            ]
        ]
