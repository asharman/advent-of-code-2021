module Utils.List exposing (..)

import Dict exposing (Dict)
import Expect
import Test exposing (..)


transpose : List (List a) -> List (List a)
transpose matrix =
    let
        numberOfRows m =
            case m of
                [] ->
                    0

                row :: _ ->
                    List.length row
    in
    matrix
        |> List.map (List.map List.singleton)
        |> List.foldr (List.map2 List.append) (List.repeat (numberOfRows matrix) [])


count : List comparable -> Dict comparable Int
count =
    List.foldl
        (\a dict ->
            Dict.update a
                (\maybeCount ->
                    case maybeCount of
                        Just b ->
                            Just (b + 1)

                        Nothing ->
                            Just 1
                )
                dict
        )
        Dict.empty


absoluteRange : Int -> Int -> List Int
absoluteRange a b =
    if b < a then
        List.range b a |> List.reverse

    else
        List.range a b


average : List Int -> Float
average xs =
    (toFloat <| List.sum xs) / (toFloat <| List.length xs)


allTests : Test
allTests =
    describe "Functions"
        [ describe "transpose"
            [ test "swaps rows and columns" <|
                \_ ->
                    transpose
                        [ [ 1, 2 ]
                        , [ 3, 4 ]
                        ]
                        |> Expect.equal
                            [ [ 1, 3 ]
                            , [ 2, 4 ]
                            ]
            ]
        , describe "count"
            [ test "count values into a dictionary" <|
                \_ ->
                    count [ 1, 2, 1, 3, 7, 4, 3 ]
                        |> Dict.get 1
                        |> Expect.equal (Just 2)
            ]
        , describe "absoluteRange"
            [ test "produces a range of numbers between the smaller and larger number inclusive" <|
                \_ ->
                    absoluteRange 5 2
                        |> Expect.equal [ 5, 4, 3, 2 ]
            , test "case 2" <|
                \_ ->
                    absoluteRange 1 3
                        |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "average"
            [ test "averages a list of integers" <|
                \_ ->
                    average [ 1, 3, 4, 5 ]
                        |> Expect.within (Expect.Absolute 0.000001) 3.25
            ]
        ]
