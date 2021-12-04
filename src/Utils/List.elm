module Utils.List exposing (..)

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
        ]
