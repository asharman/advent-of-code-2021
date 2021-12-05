port module Day05 exposing (..)

import Dict
import Expect
import Html exposing (input)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Platform exposing (Program)
import Test exposing (..)
import Utils.List as List


type alias Point =
    ( Int, Int )


type Line
    = Horizontal Int ( Int, Int )
    | Vertical Int ( Int, Int )
    | Diagonal ( Point, Point )


type Input
    = Input (List Line)


isDiagonal : Line -> Bool
isDiagonal line =
    case line of
        Diagonal _ ->
            True

        _ ->
            False


getPoints : Line -> List Point
getPoints line =
    case line of
        Horizontal y ( x1, x2 ) ->
            List.map (\x -> ( x, y )) (List.absoluteRange x1 x2)

        Vertical x ( y1, y2 ) ->
            List.map (Tuple.pair x) (List.absoluteRange y1 y2)

        Diagonal ( ( x1, y1 ), ( x2, y2 ) ) ->
            List.map2 Tuple.pair (List.absoluteRange x1 x2) (List.absoluteRange y1 y2)


solve : Input -> Int
solve (Input input) =
    input
        |> List.filter (not << isDiagonal)
        |> List.concatMap getPoints
        |> List.count
        |> Dict.toList
        |> List.filter (\( _, a ) -> a > 1)
        |> List.length


solve2 : Input -> Int
solve2 (Input input) =
    input
        |> List.concatMap getPoints
        |> List.count
        |> Dict.toList
        |> List.filter (\( _, a ) -> a > 1)
        |> List.length


decode : String -> Input
decode input =
    input
        |> String.lines
        |> List.filterMap parseLine
        |> Input


parseLine : String -> Maybe Line
parseLine input =
    input
        |> Parser.run lineParser
        |> Result.toMaybe


lineParser : Parser Line
lineParser =
    Parser.succeed (\a b -> ( a, b ))
        |. Parser.spaces
        |= pointParser
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= pointParser
        |> Parser.andThen
            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                if x1 == x2 then
                    Parser.succeed (Vertical x1 ( y1, y2 ))

                else if y1 == y2 then
                    Parser.succeed (Horizontal y1 ( x1, x2 ))

                else if abs (x1 - x2) == abs (y1 - y2) then
                    Parser.succeed (Diagonal ( ( x1, y1 ), ( x2, y2 ) ))

                else
                    Parser.problem "Lines can only be vertical or horizontal"
            )


pointParser : Parser Point
pointParser =
    Parser.succeed (\a b -> ( a, b ))
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


testInput : String
testInput =
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
"""


parseInput : Test
parseInput =
    test "Parse puzzle input" <|
        \_ ->
            testInput
                |> decode
                |> Expect.equal
                    (Input
                        [ Horizontal 9 ( 0, 5 )
                        , Diagonal ( ( 8, 0 ), ( 0, 8 ) )
                        , Horizontal 4 ( 9, 3 )
                        , Vertical 2 ( 2, 1 )
                        , Vertical 7 ( 0, 4 )
                        , Diagonal ( ( 6, 4 ), ( 2, 0 ) )
                        , Horizontal 9 ( 0, 2 )
                        , Horizontal 4 ( 3, 1 )
                        , Diagonal ( ( 0, 0 ), ( 8, 8 ) )
                        , Diagonal ( ( 5, 5 ), ( 8, 2 ) )
                        ]
                    )


getPointsTest : Test
getPointsTest =
    describe "Gets all of the points in a line"
        [ test "Diagonal Line" <|
            \_ ->
                getPoints (Diagonal ( ( 1, 1 ), ( 3, 3 ) ))
                    |> Expect.equal [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
        , test "Diagonal Line 2" <|
            \_ ->
                getPoints (Diagonal ( ( 0, 0 ), ( 8, 8 ) ))
                    |> Expect.equal
                        [ ( 0, 0 )
                        , ( 1, 1 )
                        , ( 2, 2 )
                        , ( 3, 3 )
                        , ( 4, 4 )
                        , ( 5, 5 )
                        , ( 6, 6 )
                        , ( 7, 7 )
                        , ( 8, 8 )
                        ]
        ]


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            testInput
                |> decode
                |> solve
                |> Expect.equal 5
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            testInput
                |> decode
                |> solve2
                |> Expect.equal 12
        )



-- Boilerplate stuff


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = GotInput String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput inputString ->
            let
                input =
                    decode inputString
            in
            ( model
            , output
                (encode
                    { part1 = solve input
                    , part2 = solve2 input
                    }
                )
            )


port receiveInput : (String -> msg) -> Sub msg


port output : Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveInput GotInput
