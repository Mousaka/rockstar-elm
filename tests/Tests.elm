module Tests exposing (..)

import Expect
import Fuzz exposing (Fuzzer, constant)
import Main exposing (Term(..), Val(..), Value(..), parse, simpleVariable)
import Parser as Praser
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
                    "Jonathan III Rowlins is right"
                        |> parse
                        |> Expect.equal (Ok (Variable "jonathan iii rowlins" T))
            ]
        ]
