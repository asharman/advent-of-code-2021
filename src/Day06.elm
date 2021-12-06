port module Day06 exposing (..)

import Expect
import Json.Encode as Encode exposing (Value)
import Platform exposing (Program)
import Test exposing (..)
import Utils.List as List


type alias School =
    List Int


type Input
    = Input School


population : Int -> Int -> Int
population days initialTimer =
    populationAfter (days + 8 - initialTimer)


populationAfter : Int -> Int
populationAfter days =
    let
        populationIter a b c d e f g h i count =
            if count <= 8 then
                a

            else
                populationIter (g + i) a b c d e f g h (count - 1)
    in
    populationIter 1 1 1 1 1 1 1 1 1 days


solve : Input -> Int
solve (Input input) =
    input
        |> List.map (population 80)
        |> List.sum


solve2 : Input -> Int
solve2 (Input input) =
    input
        |> List.map (population 256)
        |> List.sum


decode : String -> Input
decode input =
    input
        |> String.lines
        |> List.concatMap (String.split ",")
        |> List.filterMap String.toInt
        |> Input


encode : { part1 : Int, part2 : Int } -> Value
encode answer =
    Encode.object
        [ ( "part1", Encode.int answer.part1 )
        , ( "part2", Encode.int answer.part2 )
        ]



-- Tests


testInput : String
testInput =
    "3,4,3,1,2"


parseInput : Test
parseInput =
    test "Parse Input" <|
        \_ ->
            testInput
                |> decode
                |> Expect.equal (Input [ 3, 4, 3, 1, 2 ])


part1 : Test
part1 =
    test "Part 1"
        (\_ ->
            testInput
                |> decode
                |> solve
                |> Expect.equal 5934
        )


part2 : Test
part2 =
    test "Part 2"
        (\_ ->
            testInput
                |> decode
                |> solve2
                |> Expect.equal 26984457539
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
